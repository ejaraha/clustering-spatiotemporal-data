############
# OVERVIEW #
############

# STDBSCAN
## 1) project coordinates in preprocessed data
## 2) run STDBSCAN algorithm on preprocessed data

##### input:
######## 1) path to directory containing clean, preprocessed participant data files (.../AccuTracking Data/Cleaned)
##### output:
######## 1) csv of clustered data (.../AccuTracking Data/STDBSCAN/stdb_sj/Participant XXXX imputed STDBSCAN [eps] [eps2] [minpts].csv)

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

############
# STDBSCAN #
############

# **set working directory**   
setwd("S:\\RRTC\\RRTC 2013-2018\\GPS Study - Field Initiated\\Data\\AccuTracking Data\\Cleaned") 

# make list of paths to preprocessed files
data_path_list <- list_data_paths("Participant [[:digit:]]{4} imputed.csv",TRUE) 

# directory for exporting stdbscan csv
out_dir <- "S:\\RRTC\\RRTC 2013-2018\\GPS Study - Field Initiated\\Data\\AccuTracking Data\\STDBSCAN"

# iterate through preprocessed files
for (path in data_path_list[1]){
  # import data
  print(sprintf("Importing Participant Data: %s",path))            
  data_x <- read.csv(path, stringsAsFactors = FALSE, colClasses = c("Date" = "POSIXct"))  
  # project coordinates
  print("projecting coordinates")
  data_x <- project_coordinates()                                                        
  ########### memory error when using full data_time dataset
  if (nrow(data_x)>=12000){
    data_x <- data_x[1:12000,]
  }
  ############ set parameters here so it updates STDBSCAN and the filename
  param <- data.frame(eps=200,eps2=20,minpts=10)            
  # define output file name and path
  participant_number <- substring(path,15,18)  
  file_name <- paste("Participant",participant_number,"imputed STDBSCAN",param$eps,param$eps2,paste(param$minpts,".csv",sep="")) 
  file_path <- paste(out_dir,participant_number,"sj",file_name,sep="/")
  # run stdbscan
  print("running stddbscan with the following parameters:")
  print(param)
  stdb <- stdbscan(x=data_x$Long_proj, y=data_x$Lat_proj,                             
                   time=data_x$cum_time, eps=param$eps, eps2=param$eps2, minpts=param$minpts) 
  # append cluster field to participant data     
  stdb <- as.data.frame(cbind(data_x,stdb_cluster=stdb$cluster))
  # output csv of results
  print(sprintf("writing csv of clustered data (STDBSCAN): %s", file_path))      
  write.csv(stdb, file_path, row.names = FALSE)                                                               
}


