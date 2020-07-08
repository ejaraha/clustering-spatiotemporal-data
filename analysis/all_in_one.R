# packages
pckgs <- c("geosphere", "dplyr", "zoo","rgdal","dbscan","cluster","RColorBrewer", "geometry","grDevices","sp")
# install.packages(pckgs)
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

# functions
source("S:\\RRTC\\RRTC 2018-2023\\sjaraha\\scripts\\in use\\functions.r")

#######################
# PREPROCESSING
#######################

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

#######################
# STDBSCAN
#######################

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

#######################
# POSTPROCESSING and DBSCAN
#######################


# set working directory
setwd("S:\\RRTC\\RRTC 2013-2018\\GPS Study - Field Initiated\\Data\\AccuTracking Data\\STDBSCAN\\")

# make list of paths to data files
data_path_list <- list_data_paths("Participant [[:digit:]]{4} imputed STDBSCAN [[:digit:]]{3} [[:digit:]]{2,4} [[:digit:]]{1,2}.csv",TRUE)

# for exporting dbscan csv
out_dir <- "S:\\RRTC\\RRTC 2013-2018\\GPS Study - Field Initiated\\Data\\AccuTracking Data\\DBSCAN"

for (path in data_path_list[1]){
  
  participant_number <- substr(path,3,6)
  dir_name <- dirname(path)
  cat("PARTICIPANT",participant_number)
  # import data
  data_x <- read.csv(path, stringsAsFactors = FALSE, colClasses = c("Date" = "POSIXct"))
  # record number of unique stdb_clusters before postprocessing
  clust_pre <- sort(unique(data_x$stdb_cluster))
  num_clust_pre <- length(clust_pre)
  # launder stdb_clusters
  data_x <- scenario_1()
  data_x <- scenario_2()
  data_x <- scenario_3()
  # calculate stdb_cluster centroids and time ranges
  cat("\n","...calculating stdb_cluster centroids and time ranges","\n",sep="")
  cluster_chars_x <- stdb_cluster_chars()
  # find stdb_clusters to be combined
  cat("...finding adjacent stdb_clusters that are directly density reachable","\n",sep="")
  stdb_clusters_to_combine_x <- stdb_to_combine()
  # combine stdb_clusters 
  data_x <- scenario_4()
  # record number of unique stdb_clusters after postprocessing
  clust_post <- sort(unique(data_x$stdb_cluster))
  num_clust_post <- length(clust_post)
  # print messages
  cat("\n\nPostprocessing complete:")
  cat("\nnumber of stdb_clusters before postprocessing: ",num_clust_pre)
  cat("\nIDs of stdb_clusters before postprocessing: ")
  cat(clust_pre,sep=",")
  cat("\n","number of stdb_clusters after postprocessing: ",num_clust_post,sep="")
  cat("\nIDs of stdb_clusters after postprocessing: ")
  cat(clust_post,sep=",")
  # output csv of postprocessed data
  file_name <- paste("Participant", participant_number,"postprocessed.csv") 
  file_path <- paste(getwd(),dir_name,file_name,sep="/")
  cat("\n\n","Writing csv of postpocessed dataset: ",file_path,sep="")
  write.csv(data_x,file_path,row.names = FALSE)
  # project stdb_centroid coordinates
  cluster_chars_x <- project_coordinates(df="cluster_chars_x",x="long_centroid",y="lat_centroid")
  xy <- as.matrix(cluster_chars_x[,c("Long_proj","Lat_proj")])
  # run dbscan
  cat("\n\nRunning DBSCAN...")
  db <- dbscan(xy, eps = 200, minPts = 2)
  cat("\ndbscan_clusters: ",sort(unique(db$cluster)))
  # add dbscan clusters to cluster_chars_x dataframe as a new dataframe called dbscan_x
  dbscan_x <- as.data.frame(cbind(cluster_chars_x,dbscan_cluster=db$cluster))
  colnames(dbscan_x) <- c("stdb_cluster","long_centroid_stdb","lat_centroid_stdb","cluster_begin_time_stdb",
                          "cluster_end_time_stdb","long_centroid_projected_stdb","lat_centroid_projected_stdb","dbscan_cluster")
  # output csv of dbscan-clustered data
  file_name <- paste("Participant", participant_number,"DBSCAN.csv") 
  file_path <- paste(out_dir,participant_number,"sj",file_name,sep="/")
  cat("\n\n","Writing csv of clustered data (DBSCAN): ",file_path,sep="")
  write.csv(dbscan_x,file_path,row.names = FALSE)
}
