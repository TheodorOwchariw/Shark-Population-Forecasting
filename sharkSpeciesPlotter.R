#########################################################
# Theodor Owchariw
# Forecasting Shark Populations Using Predictive Modeling
# 12/23/24
#########################################################

# Plot total number of sharks caught in a given year for each shark species

# Import necessary libraries
library(ggplot2)
library(readr)
library(dplyr)

# Read in dataset
IATTC_ll_untuned_final_predict <- read_csv("IATTC_ll_untuned_final_predict.csv")

# Filter data
sharkLocation <- select(IATTC_ll_untuned_final_predict, catch, year, latitude, longitude, species_commonname) 
sharkLocation <- filter(sharkLocation, catch >= 1) 
sharkLocation <- sharkLocation %>% mutate(across(c('catch'), round, 0)) 

# Create simple versions of the plot
ggplot(sharkLocation, aes(year, catch, color=species_commonname)) + geom_point() + theme_bw() 
ggplot(sharkLocation, aes(year, catch, color=species_commonname)) + geom_hex() + theme_bw() 

# Extract all necessary information to sum up all caught sharks in a given species and year for plot
blacktipShark2012 <- filter(sharkLocation, species_commonname == "BLACKTIP SHARK" & year == 2012) 
blacktipShark2012 <- sum(blacktipShark2012$catch) 
blacktipShark2013 <- filter(sharkLocation, species_commonname == "BLACKTIP SHARK" & year == 2013) 
blacktipShark2013 <- sum(blacktipShark2013$catch) 
blacktipShark2014 <- filter(sharkLocation, species_commonname == "BLACKTIP SHARK" & year == 2014) 
blacktipShark2014 <- sum(blacktipShark2014$catch) 
blacktipShark2015 <- filter(sharkLocation, species_commonname == "BLACKTIP SHARK" & year == 2015) 
blacktipShark2015 <- sum(blacktipShark2015$catch) 
blacktipShark2016 <- filter(sharkLocation, species_commonname == "BLACKTIP SHARK" & year == 2016) 
blacktipShark2016 <- sum(blacktipShark2016$catch) 
blacktipShark2017 <- filter(sharkLocation, species_commonname == "BLACKTIP SHARK" & year == 2017) 
blacktipShark2017 <- sum(blacktipShark2017$catch) 
blacktipShark2018 <- filter(sharkLocation, species_commonname == "BLACKTIP SHARK" & year == 2018) 
blacktipShark2018 <- sum(blacktipShark2018$catch) 
blacktipShark2019 <- filter(sharkLocation, species_commonname == "BLACKTIP SHARK" & year == 2019) 
blacktipShark2019 <- sum(blacktipShark2019$catch) 
blacktipShark2020 <- filter(sharkLocation, species_commonname == "BLACKTIP SHARK" & year == 2020) 
blacktipShark2020 <- sum(blacktipShark2020$catch) 

blueShark2012 <- filter(sharkLocation, species_commonname == "BLUE SHARK" & year == 2012) 
blueShark2012 <- sum(blueShark2012$catch) 
blueShark2013 <- filter(sharkLocation, species_commonname == "BLUE SHARK" & year == 2013) 
blueShark2013 <- sum(blueShark2013$catch) 
blueShark2014 <- filter(sharkLocation, species_commonname == "BLUE SHARK" & year == 2014) 
blueShark2014 <- sum(blueShark2014$catch) 
blueShark2015 <- filter(sharkLocation, species_commonname == "BLUE SHARK" & year == 2015) 
blueShark2015 <- sum(blueShark2015$catch) 
blueShark2016 <- filter(sharkLocation, species_commonname == "BLUE SHARK" & year == 2016) 
blueShark2016 <- sum(blueShark2016$catch) 
blueShark2017 <- filter(sharkLocation, species_commonname == "BLUE SHARK" & year == 2017) 
blueShark2017 <- sum(blueShark2017$catch) 
blueShark2018 <- filter(sharkLocation, species_commonname == "BLUE SHARK" & year == 2018) 
blueShark2018 <- sum(blueShark2018$catch) 
blueShark2019 <- filter(sharkLocation, species_commonname == "BLUE SHARK" & year == 2019) 
blueShark2019 <- sum(blueShark2019$catch) 
blueShark2020 <- filter(sharkLocation, species_commonname == "BLUE SHARK" & year == 2020) 
blueShark2020 <- sum(blueShark2020$catch) 

hammerheadShark2012 <- filter(sharkLocation, species_commonname == "HAMMERHEAD SHARKS NEI" & year == 2012) 
hammerheadShark2012 <- sum(hammerheadShark2012$catch) 
hammerheadShark2013 <- filter(sharkLocation, species_commonname == "HAMMERHEAD SHARKS NEI" & year == 2013) 
hammerheadShark2013 <- sum(hammerheadShark2013$catch) 
hammerheadShark2014 <- filter(sharkLocation, species_commonname == "HAMMERHEAD SHARKS NEI" & year == 2014) 
hammerheadShark2014 <- sum(hammerheadShark2014$catch) 
hammerheadShark2015 <- filter(sharkLocation, species_commonname == "HAMMERHEAD SHARKS NEI" & year == 2015) 
hammerheadShark2015 <- sum(hammerheadShark2015$catch) 
hammerheadShark2016 <- filter(sharkLocation, species_commonname == "HAMMERHEAD SHARKS NEI" & year == 2016) 
hammerheadShark2016 <- sum(hammerheadShark2016$catch) 
hammerheadShark2017 <- filter(sharkLocation, species_commonname == "HAMMERHEAD SHARKS NEI" & year == 2017) 
hammerheadShark2017 <- sum(hammerheadShark2017$catch) 
hammerheadShark2018 <- filter(sharkLocation, species_commonname == "HAMMERHEAD SHARKS NEI" & year == 2018) 
hammerheadShark2018 <- sum(hammerheadShark2018$catch) 
hammerheadShark2019 <- filter(sharkLocation, species_commonname == "HAMMERHEAD SHARKS NEI" & year == 2019) 
hammerheadShark2019 <- sum(hammerheadShark2019$catch) 
hammerheadShark2020 <- filter(sharkLocation, species_commonname == "HAMMERHEAD SHARKS NEI" & year == 2020) 
hammerheadShark2020 <- sum(hammerheadShark2020$catch) 

makoShark2012 <- filter(sharkLocation, species_commonname == "MAKO SHARKS" & year == 2012) 
makoShark2012 <- sum(makoShark2012$catch) 
makoShark2013 <- filter(sharkLocation, species_commonname == "MAKO SHARKS" & year == 2013) 
makoShark2013 <- sum(makoShark2013$catch) 
makoShark2014 <- filter(sharkLocation, species_commonname == "MAKO SHARKS" & year == 2014) 
makoShark2014 <- sum(makoShark2014$catch) 
makoShark2015 <- filter(sharkLocation, species_commonname == "MAKO SHARKS" & year == 2015) 
makoShark2015 <- sum(makoShark2015$catch) 
makoShark2016 <- filter(sharkLocation, species_commonname == "MAKO SHARKS" & year == 2016) 
makoShark2016 <- sum(makoShark2016$catch) 
makoShark2017 <- filter(sharkLocation, species_commonname == "MAKO SHARKS" & year == 2017) 
makoShark2017 <- sum(makoShark2017$catch) 
makoShark2018 <- filter(sharkLocation, species_commonname == "MAKO SHARKS" & year == 2018) 
makoShark2018 <- sum(makoShark2018$catch) 
makoShark2019 <- filter(sharkLocation, species_commonname == "MAKO SHARKS" & year == 2019) 
makoShark2019 <- sum(makoShark2019$catch) 
makoShark2020 <- filter(sharkLocation, species_commonname == "MAKO SHARKS" & year == 2020) 
makoShark2020 <- sum(makoShark2020$catch) 

requiemShark2012 <- filter(sharkLocation, species_commonname == "REQUIEM SHARKS NEI" & year == 2012) 
requiemShark2012 <- sum(requiemShark2012$catch) 
requiemShark2013 <- filter(sharkLocation, species_commonname == "REQUIEM SHARKS NEI" & year == 2013) 
requiemShark2013 <- sum(requiemShark2013$catch) 
requiemShark2014 <- filter(sharkLocation, species_commonname == "REQUIEM SHARKS NEI" & year == 2014) 
requiemShark2014 <- sum(requiemShark2014$catch) 
requiemShark2015 <- filter(sharkLocation, species_commonname == "REQUIEM SHARKS NEI" & year == 2015) 
requiemShark2015 <- sum(requiemShark2015$catch) 
requiemShark2016 <- filter(sharkLocation, species_commonname == "REQUIEM SHARKS NEI" & year == 2016) 
requiemShark2016 <- sum(requiemShark2016$catch) 
requiemShark2017 <- filter(sharkLocation, species_commonname == "REQUIEM SHARKS NEI" & year == 2017) 
requiemShark2017 <- sum(requiemShark2017$catch) 
requiemShark2018 <- filter(sharkLocation, species_commonname == "REQUIEM SHARKS NEI" & year == 2018) 
requiemShark2018 <- sum(requiemShark2018$catch) 
requiemShark2019 <- filter(sharkLocation, species_commonname == "REQUIEM SHARKS NEI" & year == 2019) 
requiemShark2019 <- sum(requiemShark2019$catch) 
requiemShark2020 <- filter(sharkLocation, species_commonname == "REQUIEM SHARKS NEI" & year == 2020) 
requiemShark2020 <- sum(requiemShark2020$catch) 

sharkNEI2012 <- filter(sharkLocation, species_commonname == "SHARKS NEI" & year == 2012) 
sharkNEI2012_total <- sum(sharkNEI2012$catch) 
sharkNEI2013 <- filter(sharkLocation, species_commonname == "SHARKS NEI" & year == 2013) 
sharkNEI2013_total <- sum(sharkNEI2013$catch) 
sharkNEI2014 <- filter(sharkLocation, species_commonname == "SHARKS NEI" & year == 2014) 
sharkNEI2014_total <- sum(sharkNEI2014$catch) 
sharkNEI2015 <- filter(sharkLocation, species_commonname == "SHARKS NEI" & year == 2015) 
sharkNEI2015_total <- sum(sharkNEI2015$catch) 
sharkNEI2016 <- filter(sharkLocation, species_commonname == "SHARKS NEI" & year == 2016) 
sharkNEI2016_total <- sum(sharkNEI2016$catch) 
sharkNEI2017 <- filter(sharkLocation, species_commonname == "SHARKS NEI" & year == 2017) 
sharkNEI2017_total <- sum(sharkNEI2017$catch) 
sharkNEI2018 <- filter(sharkLocation, species_commonname == "SHARKS NEI" & year == 2018) 
sharkNEI2018_total <- sum(sharkNEI2018$catch) 
sharkNEI2019 <- filter(sharkLocation, species_commonname == "SHARKS NEI" & year == 2019) 
sharkNEI2019_total <- sum(sharkNEI2019$catch) 
sharkNEI2020 <- filter(sharkLocation, species_commonname == "SHARKS NEI" & year == 2020) 
sharkNEI2020_total <- sum(sharkNEI2020$catch) 

shortfinMakoShark2012 <- filter(sharkLocation, species_commonname == "SHORTFIN MAKO SHARK" & year == 2012) 
shortfinMakoShark2012 <- sum(shortfinMakoShark2012$catch) 
shortfinMakoShark2013 <- filter(sharkLocation, species_commonname == "SHORTFIN MAKO SHARK" & year == 2013) 
shortfinMakoShark2013 <- sum(shortfinMakoShark2013$catch) 
shortfinMakoShark2014 <- filter(sharkLocation, species_commonname == "SHORTFIN MAKO SHARK" & year == 2014) 
shortfinMakoShark2014 <- sum(shortfinMakoShark2014$catch) 
shortfinMakoShark2015 <- filter(sharkLocation, species_commonname == "SHORTFIN MAKO SHARK" & year == 2015) 
shortfinMakoShark2015 <- sum(shortfinMakoShark2015$catch) 
shortfinMakoShark2016 <- filter(sharkLocation, species_commonname == "SHORTFIN MAKO SHARK" & year == 2016) 
shortfinMakoShark2016 <- sum(shortfinMakoShark2016$catch) 
shortfinMakoShark2017 <- filter(sharkLocation, species_commonname == "SHORTFIN MAKO SHARK" & year == 2017) 
shortfinMakoShark2017 <- sum(shortfinMakoShark2017$catch) 
shortfinMakoShark2018 <- filter(sharkLocation, species_commonname == "SHORTFIN MAKO SHARK" & year == 2018) 
shortfinMakoShark2018 <- sum(shortfinMakoShark2018$catch) 
shortfinMakoShark2019 <- filter(sharkLocation, species_commonname == "SHORTFIN MAKO SHARK" & year == 2019) 
shortfinMakoShark2019 <- sum(shortfinMakoShark2019$catch) 
shortfinMakoShark2020 <- filter(sharkLocation, species_commonname == "SHORTFIN MAKO SHARK" & year == 2020) 
shortfinMakoShark2020 <- sum(shortfinMakoShark2020$catch) 

silkyShark2012 <- filter(sharkLocation, species_commonname == "SILKY SHARK" & year == 2012) 
silkyShark2012 <- sum(silkyShark2012$catch) 
silkyShark2013 <- filter(sharkLocation, species_commonname == "SILKY SHARK" & year == 2013) 
silkyShark2013 <- sum(silkyShark2013$catch) 
silkyShark2014 <- filter(sharkLocation, species_commonname == "SILKY SHARK" & year == 2014) 
silkyShark2014 <- sum(silkyShark2014$catch) 
silkyShark2015 <- filter(sharkLocation, species_commonname == "SILKY SHARK" & year == 2015) 
silkyShark2015 <- sum(silkyShark2015$catch) 
silkyShark2016 <- filter(sharkLocation, species_commonname == "SILKY SHARK" & year == 2016) 
silkyShark2016 <- sum(silkyShark2016$catch) 
silkyShark2017 <- filter(sharkLocation, species_commonname == "SILKY SHARK" & year == 2017) 
silkyShark2017 <- sum(silkyShark2017$catch) 
silkyShark2018 <- filter(sharkLocation, species_commonname == "SILKY SHARK" & year == 2018) 
silkyShark2018 <- sum(silkyShark2018$catch) 
silkyShark2019 <- filter(sharkLocation, species_commonname == "SILKY SHARK" & year == 2019) 
silkyShark2019 <- sum(silkyShark2019$catch) 
silkyShark2020 <- filter(sharkLocation, species_commonname == "SILKY SHARK" & year == 2020) 
silkyShark2020 <- sum(silkyShark2020$catch) 

thresherShark2012 <- filter(sharkLocation, species_commonname == "THRESHER SHARKS NEI" & year == 2012) 
thresherShark2012 <- sum(thresherShark2012$catch) 
thresherShark2013 <- filter(sharkLocation, species_commonname == "THRESHER SHARKS NEI" & year == 2013) 
thresherShark2013 <- sum(thresherShark2013$catch) 
thresherShark2014 <- filter(sharkLocation, species_commonname == "THRESHER SHARKS NEI" & year == 2014) 
thresherShark2014 <- sum(thresherShark2014$catch) 
thresherShark2015 <- filter(sharkLocation, species_commonname == "THRESHER SHARKS NEI" & year == 2015) 
thresherShark2015 <- sum(thresherShark2015$catch) 
thresherShark2016 <- filter(sharkLocation, species_commonname == "THRESHER SHARKS NEI" & year == 2016) 
thresherShark2016 <- sum(thresherShark2016$catch) 
thresherShark2017 <- filter(sharkLocation, species_commonname == "THRESHER SHARKS NEI" & year == 2017) 
thresherShark2017 <- sum(thresherShark2017$catch) 
thresherShark2018 <- filter(sharkLocation, species_commonname == "THRESHER SHARKS NEI" & year == 2018) 
thresherShark2018 <- sum(thresherShark2018$catch) 
thresherShark2019 <- filter(sharkLocation, species_commonname == "THRESHER SHARKS NEI" & year == 2019) 
thresherShark2019 <- sum(thresherShark2019$catch) 
thresherShark2020 <- filter(sharkLocation, species_commonname == "THRESHER SHARKS NEI" & year == 2020) 
thresherShark2020 <- sum(thresherShark2020$catch) 

# Organize data into dataframes by shark species
blacktipShark <- tibble( 
  count= c(blacktipShark2012, blacktipShark2013, blacktipShark2014,
           blacktipShark2015, blacktipShark2016, blacktipShark2017,
           blacktipShark2018, blacktipShark2019, blacktipShark2020), 
  year = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020) 
) 

blueShark <- tibble( 
  count= c(blueShark2012, blueShark2013, blueShark2014, blueShark2015, 
           blueShark2016, blueShark2017, blueShark2018, blueShark2019, 
           blueShark2020), 
  year = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020) 
) 

hammerheadShark <- tibble( 
  count= c(hammerheadShark2012, hammerheadShark2013, hammerheadShark2014, 
           hammerheadShark2015, hammerheadShark2016, hammerheadShark2017, 
           hammerheadShark2018, hammerheadShark2019, hammerheadShark2020), 
  year = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020) 
) 

makoShark <- tibble( 
  count= c(makoShark2012, makoShark2013, makoShark2014, 
           makoShark2015, makoShark2016, makoShark2017, 
           makoShark2018, makoShark2019, makoShark2020), 
  year = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020) 
) 

requiemShark <- tibble( 
  count= c(requiemShark2012, requiemShark2013, requiemShark2014, 
           requiemShark2015, requiemShark2016, requiemShark2017, 
           requiemShark2018, requiemShark2019, requiemShark2020), 
  year = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020) 
) 

NEIShark <- tibble(  
  count= c(sharkNEI2012_total, sharkNEI2013_total, sharkNEI2014_total,  
           sharkNEI2015_total, sharkNEI2016_total, sharkNEI2017_total,  
           sharkNEI2018_total, sharkNEI2019_total, sharkNEI2020_total),  
  year = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)  
) 

shortfinMakoShark <- tibble( 
  count= c(shortfinMakoShark2012, shortfinMakoShark2013, shortfinMakoShark2014, 
           shortfinMakoShark2015, shortfinMakoShark2016, shortfinMakoShark2017, 
           shortfinMakoShark2018, shortfinMakoShark2019, shortfinMakoShark2020), 
  year = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020) 
  
) 

silkyShark <- tibble( 
  count= c(silkyShark2012, silkyShark2013, silkyShark2014, silkyShark2015, 
           silkyShark2016, silkyShark2017, silkyShark2018, silkyShark2019, 
           silkyShark2020), 
  year = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020) 
) 

thresherShark <- tibble( 
  count= c(thresherShark2012, thresherShark2013, thresherShark2014, 
           thresherShark2015, thresherShark2016, thresherShark2017, 
           thresherShark2018, thresherShark2019, thresherShark2020), 
  year = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020) 
) 

# Create a new data frame with all the sharkspecies combined
numSharks <- bind_rows(blacktipShark, blueShark, hammerheadShark, 
                       makoShark, requiemShark, NEIShark, shortfinMakoShark, 
                       silkyShark, thresherShark) 

sharkType <- c("BLACKTIP SHARK", "BLACKTIP SHARK", "BLACKTIP SHARK", 
               "BLACKTIP SHARK", "BLACKTIP SHARK", "BLACKTIP SHARK", 
               "BLACKTIP SHARK", "BLACKTIP SHARK", "BLACKTIP SHARK", 
               "BLUE SHARK", "BLUE SHARK", "BLUE SHARK", "BLUE SHARK", 
               "BLUE SHARK", "BLUE SHARK", "BLUE SHARK", "BLUE SHARK", 
               "BLUE SHARK", "HAMMERHEAD SHARKS NEI", "HAMMERHEAD SHARKS NEI", 
               "HAMMERHEAD SHARKS NEI", "HAMMERHEAD SHARKS NEI", "HAMMERHEAD SHARKS NEI", 
               "HAMMERHEAD SHARKS NEI", "HAMMERHEAD SHARKS NEI", "HAMMERHEAD SHARKS NEI", 
               "HAMMERHEAD SHARKS NEI", "MAKO SHARKS", "MAKO SHARKS", "MAKO SHARKS", 
               "MAKO SHARKS", "MAKO SHARKS", "MAKO SHARKS", "MAKO SHARKS", 
               "MAKO SHARKS", "MAKO SHARKS", "REQUIEM SHARKS NEI", "REQUIEM SHARKS NEI", 
               "REQUIEM SHARKS NEI", "REQUIEM SHARKS NEI", "REQUIEM SHARKS NEI", 
               "REQUIEM SHARKS NEI", "REQUIEM SHARKS NEI", "REQUIEM SHARKS NEI", 
               "REQUIEM SHARKS NEI", "SHARKS NEI", "SHARKS NEI", "SHARKS NEI", 
               "SHARKS NEI", "SHARKS NEI", "SHARKS NEI", "SHARKS NEI", "SHARKS NEI", 
               "SHARKS NEI", "SHORTFIN MAKO SHARK", "SHORTFIN MAKO SHARK", 
               "SHORTFIN MAKO SHARK", "SHORTFIN MAKO SHARK", "SHORTFIN MAKO SHARK", 
               "SHORTFIN MAKO SHARK", "SHORTFIN MAKO SHARK", "SHORTFIN MAKO SHARK", 
               "SHORTFIN MAKO SHARK", "SILKY SHARK", "SILKY SHARK", "SILKY SHARK", 
               "SILKY SHARK", "SILKY SHARK", "SILKY SHARK", "SILKY SHARK", 
               "SILKY SHARK", "SILKY SHARK", "THRESHER SHARKS NEI", "THRESHER SHARKS NEI", 
               "THRESHER SHARKS NEI", "THRESHER SHARKS NEI", "THRESHER SHARKS NEI", 
               "THRESHER SHARKS NEI", "THRESHER SHARKS NEI", "THRESHER SHARKS NEI", 
               "THRESHER SHARKS NEI") 

numSharks  <- numSharks %>% mutate(sharkName = sharkType) 

# Plot organized by color combined line graph
ggplot(numSharks, aes(x = year, y = count, color = sharkName)) + 
  geom_line(linewidth = 1.5, alpha = 0.6) + geom_point(size = 4.5, shape = 1) + 
  labs (title = "Year vs Number of Spotted Sharks", x = "Year", y = "Count", color = "Shark Species") + 
  scale_color_manual(values = c("black", "blue", "gray", "orange", "green", "purple", "red", "pink", "cyan") )+
  theme_bw() 

# Plot separate bar graphs instead
ggplot(numSharks, aes(x = year, y = count, fill = sharkName)) + 
  geom_bar(stat = "identity") + facet_wrap(~ sharkName, ncol = 5) + 
  labs (title = "Year vs Number of Spotted Sharks", x = "Year", 
        y = "Count", fill = "Shark Species") + 
  scale_fill_manual(values = c("black", "blue", "gray", "orange", "green", "purple", "red", "pink", "cyan") )+
  theme_bw() 
