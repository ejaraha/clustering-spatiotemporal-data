############
# OVERVIEW #
############

# POST-PROCESSING and DBSCAN
## scenario 1: signal lost upon arrival at location, signal regained upon departure 
## scenario 2: recovering stdb_cluster points from missing data
## scenario 3: recovering stdb_cluster points from missing data
## scenario 4: combine stdb_clusters that were separated due to lost signal/dead battery (ex. phone dies overnight)
## DBSCAN : cluster destinations that were visited more than once

##### input:
######## 1) path to directory containing STDBSCAN participant data files (.../AccuTracking Data/STDBSCAN)
##### output:
######## 1) csv of post-processed data (.../AccuTracking Data/STDBSCAN/sj/Participant XXXX postprocessed.csv)

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

##############################
# POST-PROCESSING and DBSCAN #
##############################

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
