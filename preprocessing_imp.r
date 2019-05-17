############
# OVERVIEW #
############

# PRE-PROCESSING
## 1) process clean participant data
##    - drop duplicate observations
##    - standardize dataframe
##    - calculate fields: time_gap, cum_time, distance
##    - smooth outliers 
##    - create date_time dataframe 
##    - join participant data to date_time dataframe
##    - impute participant data date_time dataframe
##    - drop rows with time_gap >= 20 mins
##    - recalculate fields: time_gap, cum_time, distance

##### input:
######## - path to directory containing clean participant data files (.../AccuTracking Data/Cleaned)
##### output: 
######## - csv of removed observations (.../AccuTracking Data/Cleaned/processing_sj/Participant XXXX removed observations.csv)
######## - csv of date_time dataframe  (.../AccuTracking Data/Cleaned/processing_sj/Participant XXXX date_time.csv)

#################################
# PACKAGES AND CUSTOM FUNCTIONS #
#################################

# packages
pckgs <- c("geosphere", "dplyr", "zoo","rgdal","dbscan","cluster","RColorBrewer", "geometry","grDevices","sp")
#install.packages(pckgs)
library(geosphere)    # distVincentyEllipsoid
library(dplyr)        # left_join, groupby, between
library(zoo)          # na.approx
library(rgdal)        # project
library(dbscan)       # dbscan
library(cluster)      # silhouette
library(RColorBrewer) # colorRampPalette
library(geometry)     # polyarea
library(grDevices)    # chull
library(sp)           # polygon

# custom_functions
source("S:\\RRTC\\RRTC 2018-2023\\sjaraha\\scripts\\in use\\functions.r")

###############
# PRE-PROCESS #
###############

# set working directory 
setwd("S:\\RRTC\\RRTC 2013-2018\\GPS Study - Field Initiated\\Data\\AccuTracking Data\\Cleaned")

# list of paths to patricipant data files
data_path_list <- list_data_paths("Participant [[:digit:]]{4} Cleaned.csv",FALSE)                                                      

# iterate through participant data files
for (path in data_path_list[1]){
  # participant number and directory name
  participant_number <- substring(path,15,18)
  dir_name <- dirname(path)
  # load data
  data_x <- read.csv(path, stringsAsFactors = FALSE)                           
  print(sprintf("Importing Participant Data: %s",path))
  # record initial row count
  nrow_a <- nrow(data_x)   
  # drop duplicate records
  data_x <- unique(data_x)  
  print(sprintf("%i duplicate records dropped", nrow_a-nrow(data_x)))          
  # launder field names and types
  data_x <- id_subset(df="data_x","Date","Long","Lat")                         
  data_x <- as_POSIXct()         
  # add calculated columns
  data_x <- time_gap_minutes()
  data_x$cum_time <- cumsum(data_x$time_gap)
  data_x <- distance_meters()
  # smooth outliers
  data_x <- smooth_outliers()
  # create date_time dataset
  date_time <- date_time_create()
  # join participant data to date_time dataframe
  date_time <- left_join(date_time, data_x, by=c("Date"="Date"))
  print("imputing longitude and latitude where time_gap <= 20")
  # impute observations with time_gap <= 20
  date_time <- impute("date_time","Long","Lat")
  # record final row count
  nrow_a <- nrow(date_time)
  # drop rows where longitude = NA
  date_time <- date_time[-which(is.na(date_time$Long)),]
  print(sprintf("%i rows dropped where longitude = NA", nrow_a-nrow(date_time)))
  # recalculate calculated columns
  date_time <- time_gap_minutes(df="date_time") 
  date_time$cum_time <- cumsum(date_time$time_gap)
  date_time <- distance_meters(df="date_time")
  # output csv of preprocessed data
  file_name <- paste("Participant",participant_number,"imputed.csv")
  file_path <- paste(getwd(),dir_name,"sj",file_name,sep="/")
  print(sprintf("Writing csv of imputed dataset: %s", file_path))
  write.csv(date_time,file_path, row.names = FALSE)
  # print carriage return after each iteration
  cat("\n")
}








