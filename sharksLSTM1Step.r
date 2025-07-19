#########################################################
# Theodor Owchariw
# Forecasting Shark Populations Using Predictive Modeling
# 12/4/24
#########################################################

# Read in data, preprocess, and fit LSTM with 1 timestep to create a predictive model
# Predicts categories of number of sharks by giving it year, coordinates, and shark species

# Import necessary libraries
library(readr)
library(ggplot2)
library(dplyr)
library(keras)

# Read in data
IATTC_ll_untuned_final_predict <- read_csv("IATTC_ll_untuned_final_predict.csv")

sharkLocation <- select(IATTC_ll_untuned_final_predict, catch, year, latitude, longitude, species_commonname) # Select only used data
sharkLocation <- filter(sharkLocation, catch >= 1) # Removes data that records 0 catches
sharkLocation <- sharkLocation %>% mutate(across(c('catch'), round, 0)) # Round catches to integer

ggplot(sharkLocation, aes(year, catch, color=species_commonname)) + geom_hex() + theme_bw() # Plot a simple graph (shark species vs number caught)

sharkLocation <- sharkLocation %>% # One-hot encode shark species
  mutate(
    blacktipShark = ifelse(species_commonname == "BLACKTIP SHARK", 1, 0),
    blueShark = ifelse(species_commonname == "BLUE SHARK", 1, 0),
    hammerheadShark = ifelse(species_commonname == "HAMMERHEAD SHARKS NEI", 1, 0),
    makoShark = ifelse(species_commonname == "MAKO SHARKS", 1, 0),
    requiemSharksNEI = ifelse(species_commonname == "REQUIEM SHARKS NEI", 1, 0),
    sharksNEI = ifelse(species_commonname == "SHARKS NEI", 1, 0),
    shortfinMakoShark = ifelse(species_commonname == "SHORTFIN MAKO SHARK", 1, 0),
    silkyShark = ifelse(species_commonname == "SILKY SHARK", 1, 0),
    thresherShark = ifelse(species_commonname == "THRESHER SHARKS NEI", 1, 0)
  )

sharkLocation <- sharkLocation %>%  # Create buckets to categorize ranges of catches
  
  mutate(categorizedCatch = catch) %>%
  
  mutate(categorizedCatch = case_when(
    catch >=  1 & catch <= 2 ~ 1,
    catch > 2 & catch <= 7 ~5, 
    catch > 7 & catch <= 25 ~ 15,
    catch > 25 & catch <= 70 ~ 50,
    catch > 70  ~ 100, # 1424 is the most amount of sharks caught in dataset
  ))

sharkLocation <- sharkLocation %>%
  
  mutate(remappedCatch = categorizedCatch) %>%
  
  mutate(remappedCatch = case_when( # Encode into data TensorFlow can read given it has 5 outputs
    categorizedCatch ==  1 ~ 0,
    categorizedCatch == 5 ~ 1,
    categorizedCatch == 15 ~ 2,
    categorizedCatch == 50 ~ 3,
    categorizedCatch == 100 ~ 4,
  ))

# Initialize columns
sharkLocation$normalizedLatitude <- 0
sharkLocation$normalizedLongitude <- 0
sharkLocation$cosYear <- 0
sharkLocation$sinYear <- 0

# Preprocess all rows of data in dataframe
for (row in 1:nrow(sharkLocation)){   # Normalize coordinates
  
  latitude <- floor(sharkLocation$latitude[row])  # Round coordinates down
  longitude <- floor(sharkLocation$longitude[row])
  
  shiftedLatitude <- latitude + 90  # Remap coordinates
  shiftedLongitude <- longitude + 180
  
  normalizedLatitude <- shiftedLatitude / 180
  normalizedLongitude <- shiftedLongitude / 360
  
  sharkLocation$normalizedLongitude[row] <- normalizedLongitude
  sharkLocation$normalizedLatitude[row] <- normalizedLatitude
  
  # Normalize/cyclize years
  maxYear <- 2020
  minYear <- 2012
  
  range <- maxYear - minYear
  
  year <- sharkLocation$year[row]
  normalizedYear <- year - minYear # Remap year data to start at 0
  
  cosYear <- round(cos((2*pi*normalizedYear)/range), 4) # Convert years into cyclical data
  sinYear <- round(sin((2*pi*normalizedYear)/range), 4)
  
  sharkLocation$cosYear[row] <- cosYear
  sharkLocation$sinYear[row] <- sinYear
}

shuffled <- sample(1:nrow(sharkLocation)) # Shuffle data to ensure even distribution
sharkLocation <- sharkLocation[shuffled, ]
# Print out sample of what preprocessed data looks like
head(sharkLocation)

# Add necessary attributes to a matrix for predicting y 
x <- as.matrix(sharkLocation[, c("normalizedLatitude","normalizedLongitude", "cosYear","sinYear","blacktipShark", 
                                 "blueShark", "hammerheadShark", "makoShark", "requiemSharksNEI", 
                                 "sharksNEI", "shortfinMakoShark", "silkyShark","thresherShark"
)])

y <- (sharkLocation$remappedCatch) # Labels defined

timestep <- 1 # 1 year of data will be analyzed at a time
sampleSize <- length(y)
sequenceSize <- floor(sampleSize / timestep)

# Reshape y to account for timestep
yReshaped <- matrix(y[1:(sequenceSize * timestep)], nrow = sequenceSize, byrow = TRUE)

# Reshape x to account for timestep
trainSize <-  as.integer(0.8*nrow(x))
featuresSize <- ncol(x)

xReshaped <- array(NA, dim = c(sequenceSize, timestep, featuresSize))

for (i in 1:sequenceSize) {
  startIdx <- (i - 1) * timestep + 1
  endIdx <- i * timestep
  xReshaped[i, 1, ] <- x[startIdx:endIdx, ]
}

# Separate data into training and testing data
trainingSize <- floor(0.8 * nrow(xReshaped))
trainIndices <- sample(1:nrow(xReshaped), size = trainingSize)

xTrain <- xReshaped[trainIndices, , ,drop = FALSE]
xTest <- xReshaped[-trainIndices, , ,drop = FALSE]

yTrain <- yReshaped[trainIndices]
yTest <- yReshaped[-trainIndices]

set.seed(11) # Set seed to ensure consistent outputs

model <- keras_model_sequential() %>% # LSTM layered neural network
  layer_lstm(units = 64, input_shape = c(timestep, featuresSize), return_sequences = TRUE) %>%
  layer_lstm(units = 64, return_sequences = TRUE) %>% 
  layer_lstm(units = 64, return_sequences = TRUE) %>% 
  layer_lstm(units = 64) %>% 
  layer_dense(units = 5, activation = "softmax")

model %>% compile( # Compile model
  loss = "sparse_categorical_crossentropy",
  optimizer = optimizer_adam(),
  metrics = c("accuracy")
)

history <- model %>% fit( # Fit model
  xTrain, yTrain,
  epochs = 1000,
  batch_size = 64,
  validation_split = 0.20,
  verbose = 1,
)

save_model_tf(model, file = "sharkPredictionModel") # Save model for future predictions
saveRDS(history, file = "sharkPredictionHistory.rds") # Save history for future analysis
saveRDS(sharkLocation, file="sharkLocationDF.rds")
