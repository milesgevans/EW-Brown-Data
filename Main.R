##################################################
## Project: EW-Brown-Data
## Script purpose: This script will download and visualize 
## data from the E.W. Brown Energy Storage System provided
## by Louisville Gas & Electric and Kentucky Utilities
## Date: 2020-06-11
## Author: Miles Evans
## Credit: 
##################################################



# Housekeeping ####
rm(list = ls())
graphics.off()
cat("\014")  


# User inputs ####
# What years of data should this tool download
# The first year of data available is 2017
years_to_load = c(2017, 2018, 2019, 2020)
# Where should the data be downloaded to?
datadirectory = "C:/EWBrownData/"



# Libraries ####
# List all packages required by this script
required_packages = c('ggplot2', 'dplyr', 'tidyr', 'data.table', 'lubridate', 'cowplot', 'plotly')

# Identify Missing Packages
missing_packages = required_packages[!(required_packages %in% installed.packages()[,"Package"])]

# Install Missing Packages
if(length(missing_packages)){
  install.packages(missing_packages)
} 

# Load all libraries
lapply(required_packages, require, character.only = TRUE)
rm(required_packages)



# Define Functions ####
downloadoneyear = function(year){
  # This function takes an integer representing any year from 2017-2020
  # and returns a data frame of the E.W. Brown data from that year
  base_url = 'https://m.lkeportal.com/publicsolarbatch/ESS_'
  url_tail = '.txt'
  
  #Generate URL
  url = paste0(base_url,as.character(year),url_tail)
  
  #Download Data
  print(paste0('Downloading data for ', as.character(year)))
  data = fread(url)
  
  # Convert Data types
  data$Timestamp = ymd_hms(data$Timestamp)
  
  print(paste0('Done Downloading ', as.character(year)))
  
  return(data)
}



# Download Data ####

#Download all data
alldata = lapply(years_to_load, downloadoneyear)

#Merge every year of data together
alldata = do.call(rbind,alldata) #NOTE: if column format changes, this will break




# Data Quality ####
# Get number of rows with no power or SOC data
numna = length(which(is.na(alldata$PowerReal) | is.na(alldata$AvgSOC)))
print(paste0("Removing ", as.character(numna), " rows containing missing power or SOC data, representing ", as.character(100*numna/nrow(alldata)), "%  of the data"))
alldata = alldata %>% drop_na(PowerReal, AvgSOC)

# Save Data ####
dir.create(datadirectory)
setwd(datadirectory)
write.csv(alldata, file = paste0('EWBrownData',"_",as.character(min(years_to_load)),"_",as.character(max(years_to_load)),'.csv'))

# Visualize Data ####
#All available data in one plot
alldataplot = ggplot(alldata) +
  geom_step(aes(x=Timestamp,y=PowerReal))+
  theme_bw()+
  xlab('Time') +
  ylab('Real Power (kW)')
ggplotly(alldataplot)
