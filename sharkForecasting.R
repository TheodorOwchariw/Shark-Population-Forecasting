#########################################################
# Theodor Owchariw
# Forecasting Shark Populations Using Predictive Modeling
# 12/4/24
#########################################################

# Plotting 9 years of one set of coordinates on all shark species
# Refer to "sharksLSTM1Step.r" for further comments on repeated code

# Import necessary libraries
library(ggplot2)
library(keras)
library(dplyr)

sharkLocation <- readRDS("sharkLocationDF.rds")

uniqueSpecies <- unique(sharkLocation$species_commonname)
uniqueYears <- 2021:2029

# Find total number of rows in new data frame and initialize values
numRows <- length(uniqueSpecies) * length(uniqueYears)
sharkCatchForecastDF <- data.frame(
  bucket = integer(numRows),
  species_commonname = character(numRows),
  year = integer(numRows),
  latitude = numeric(numRows),
  longitude = numeric(numRows),
  stringsAsFactors = FALSE
)


i <- 1 # Indexing for adding rows

# Use a nested loop to go through all shark species and then all years
for (species in uniqueSpecies){
  for (year in uniqueYears){
    
    # Predicting using same coordinates on all data
    latitude <- 38.5 # In the deep sea about 175 miles northwest of San Francisco
    longitude <- -125.5
    roundedLatitude <- floor(latitude)
    roundedLongitude <- floor(longitude)
    year <- year
    species_commonname <- species
    
    # Preprocess data same way as original data
    blacktipShark = ifelse(species_commonname == "BLACKTIP SHARK", 1, 0)
    blueShark = ifelse(species_commonname == "BLUE SHARK", 1, 0)
    hammerheadShark = ifelse(species_commonname == "HAMMERHEAD SHARKS NEI", 1, 0)
    makoShark = ifelse(species_commonname == "MAKO SHARKS", 1, 0)
    requiemSharksNEI = ifelse(species_commonname == "REQUIEM SHARKS NEI", 1, 0)
    sharksNEI = ifelse(species_commonname == "SHARKS NEI", 1, 0)
    shortfinMakoShark = ifelse(species_commonname == "SHORTFIN MAKO SHARK", 1, 0)
    silkyShark = ifelse(species_commonname == "SILKY SHARK", 1, 0)
    thresherShark = ifelse(species_commonname == "THRESHER SHARKS NEI", 1, 0)
    
    shiftedLatitude <- roundedLatitude + 90
    
    shiftedLongitude <- roundedLongitude + 180
    
    normalizedLatitude <- shiftedLatitude / 180
    normalizedLongitude <- shiftedLongitude / 360
    
    maxYear <- 2020 # Still use original min and max year
    minYear <- 2012
    range <- maxYear - minYear
    
    normalizedYear <- year - minYear
    
    cosYear <- round(cos((2*pi*normalizedYear)/range), 4)
    sinYear <- round(sin((2*pi*normalizedYear)/range), 4)
    
    row <- c(normalizedLatitude, normalizedLongitude, cosYear, sinYear, 
             blacktipShark, blueShark, hammerheadShark, makoShark,
             requiemSharksNEI, sharksNEI, shortfinMakoShark, silkyShark,
             thresherShark
    )
    
    x <- matrix(row, nrow = 1, byrow = TRUE)
    x <- array_reshape(x, c(1, 1, 13)) # Shape necessary for 1 row of data
    
    # Load model
    model <- load_model_tf("sharkPredictionModel")
    
    # Predict "x" with model
    predictions <- predict(model, x)
    prediction <- max.col(predictions) # Find the bucket that was predicted
    
    # Switch to handle bucket cases for visualization in console
    cases <- function(prediction) {
      switch(prediction,
             "0" = { 
               print(paste("There should be around 2 (ranging from 1 to 2)", species_commonname,"at the coordinates (",latitude,",",longitude,") in ",year))
             },
             "1" = { 
               print(paste("There should be around 5 (ranging from 3 to 7)", species_commonname,"at the coordinates (",latitude,",",longitude,") in ",year))
             },
             "2" = { 
               print(paste("There should be around 15 (ranging from  7 to 25)", species_commonname,"at the coordinates (",latitude,",",longitude,") in ",year))
             },
             "3" = { 
               print(paste("There should be around 50 (ranging from 26 to 70)", species_commonname,"at the coordinates (",latitude,",",longitude,") in ",year))
             },
             "4" = {
               print(paste("There should be likely over 100 (ranging from greater than 71)", species_commonname,"at the coordinates (",latitude,",",longitude,") in ",year))
             },
             { 
               print("Error: no case predicted!")
             }
      )
    }
    
    cases(prediction) # Print out interpreted prediction
   
    newRow <- data.frame( # Create a new row of data
      bucket = prediction, species_commonname = species_commonname, 
      year = year, latitude = latitude, longitude = longitude  
    )
    
    sharkCatchForecastDF[i, ] <- newRow
    
    i <- i + 1
  }
}

saveRDS(sharkCatchForecastDF, "sharkForecasting.rds") # save data for future usage
write.csv(sharkCatchForecastDF, "sharkForecasting.csv", row.names = FALSE) # save data frame as a csv file

# Plot separate bar graphs
ggplot(sharkCatchForecastDF, aes(x = year, y = bucket, fill = species_commonname)) + 
  geom_bar(stat = "identity") + facet_wrap(~ species_commonname, ncol = 3) + 
  labs (title = "Year vs Buckets at Coordinates (38.5, -125.5)", x = "Year", 
        y = "Bucket Count", fill = "Shark Species") + 
  scale_fill_manual(values = c("black", "blue", "gray", "orange", "green", "purple", "red", "pink", "cyan") )+
  scale_x_continuous(breaks = seq(2021, 2029, by = 1)) +
  scale_y_continuous(breaks = seq(0, 6, by = 1)) +
  theme_bw() 
