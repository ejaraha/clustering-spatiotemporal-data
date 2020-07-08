list_data_paths <- function(pattern, rec){
  # searches working directory for files that match the specified pattern
  # on match, adds file path to a list
  # returns list the list of matching file paths
  ## pattern (str): regex pattern to match
  ## rec (boolean): recurse into directories (True) or don't (False)
  
  # initialize list
  data_path_list <- c()
  # loop through directories
  for (pd in list.dirs(recursive = rec)){
    # loop through files in directories
    for (f in list.files(pd)){
      # find files that match the pattern
      if (grepl(pattern, f, ignore.case = FALSE)==TRUE){
        # construct path to matching file
        data_path <- paste(pd,f, sep="/")
        # add path to list
        data_path_list <- c(data_path_list,data_path)
      }}}
  # return list of paths to matching files
  return(data_path_list)
}

id_subset <- function(df="data_x", ...){
  # standardizes participant dataframes by
  # 1) keeping only necessary columns
  # 2) adding ID column
  # returns standardized dataframe
  ## df(str): name of dataframe 
  ## ...(str): name(s) of columns to keep
  
  # get dataframe from global environment    
  df <- get(df)                                
  # create ID column
  id <- as.data.frame(1:nrow(df))              
  # list of column names to keep
  keep_cols <- c(...) 
  # subset dataframe
  df <- df[,keep_cols]
  # add ID column
  df <- cbind(id, df)
  # assign column names
  colnames(df) <- c("ID", keep_cols)
  # return standardized dataframe
  return(df)
}

as_POSIXct <- function(df="data_x", column="Date", pattern="%m/%d/%Y %H:%M"){
  # converts date-time column to POSIXct
  # returns new dataframe
  ## df(str): name of dataframe 
  ## column (str): name of column to be converted 
  ## pattern (str): date-time pattern 
  
  # get dataframe from global environment
  df <- get(df)
  # convert to POSIXct
  df[,column] <- as.POSIXct(strptime(as.character(df[,column]), pattern))   
  # return df with converted column
  return(df)
}

time_gap_minutes <- function(df="data_x",calc_col="time_gap", from_col="Date" ){
  # calculates minutes elapsed between observations (time_gap)
  # returns df with column containing time_gaps in minutes
  ## df(str): name of dataframe 
  ## calc_col (str): name of column to be calculated
  ## from_col (str): name of column to use to calculate calc_col 
  
  # get dataframe from global environment    
  df <- get(df)                                        
  tg_list <- c(0)
  # calculate sequential time difference using difftime base function
  for (i in seq(2,nrow(df))){
    from_time <-df[i-1,from_col]
    to_time <- df[i,from_col]
    tg_list <- c(tg_list,difftime(to_time, from_time,units="mins"))
  }
  df[,calc_col]<- tg_list
  # return dataframe with time_gap column
  return(df)              
}

distance_meters <- function(df="data_x", calc_col="distance", from_col1="Long", from_col2="Lat"){
  # calculates distance between adjacent observations
  # returns df with column containing distance traveled between observations (meters)
  ## df(str): name of dataframe 
  ## calc_col (str): name of column to be calculated
  ## from_col1, from_col2 (str): name of columns to use to calculate calc_col
  
  df <- get(df)
  long_lat <- cbind(df[,from_col1],df[,from_col2])
  df[,calc_col] <- c(0, distVincentyEllipsoid(long_lat))  
  return(df)
}

smooth_outliers <- function(df="data_x", distnc="distance",tg="time_gap",long="Long", lat="Lat", id="ID"){
  # redefines long/lat as the average of prev/next long/lats for observations
  # IF:prev time_gap <= 1 min
  #  & prev distance > 200 meters
  #  & next time_gap <= 1 min
  #  & next distance > 200 meters
  #  & prev/next time_gap <= 2 min
  #  & prev/next distance < 200 meters
  ## df(str): name of dataframe 
  ## distnc (str): name of column containing distance in meters
  ## tg (str): name of column containing time_gap in minutes
  ## long (str): name of column containing longitude in degrees
  ## lat (str): name of column containing latitude in degrees
  ## id (str): name of column containing observation id
  
  # get dataframe from global environment    
  df <- get(df)     
  # initialize outlier count
  outlier_count <- 0
  long_lat <- cbind(df[,long],df[,lat])   
  # check for criteria
  for (i in seq(2,nrow(df)-1)){
    if (df[i,tg] <= 1
        & df[i,distnc] > 200
        & df[i+1,tg] <= 1
        & df[i+1,distnc] > 200
        & df[i,tg]+df[i+1,tg] <= 2
        & distVincentyEllipsoid(long_lat[i-1,],long_lat[i+1,]) < 200){
      # smooth outliers
      df[i,long] <- mean(c(df[i-1,long],df[i+1,long]))                       
      df[i,lat] <- mean(c(df[i-1,lat],df[i+1,lat]))
      # print id of altered observation
      print(sprintf("Smoothing outlier at observation id %d", df[i,id]))    
      outlier_count <- outlier_count + 1
    }}
  print(sprintf('%i outliers smoothed',outlier_count))      
  # return smoothed df
  return(df)       
}

date_time_create <- function(df="data_x", num_days=13, date_col = "Date"){
  # returns a nX1 dataframe with a timestamp for every minute over a specified period of time
  ## df(str): name of dataframe to use
  ## num_days (numeric): number of days to include in date_time dataframe
  ## date_col (str): name of column in df containing timestamps as POSIXct
  
  # get dataframe from global environment 
  df <- get(df)
  # define start date 
  start_date <- df[1,date_col]
  # define end date
  end_date <- start_date+86400*num_days
  # create dataframe
  date_time <- data.frame("Date" = seq.POSIXt(start_date,end_date,"min"))   
  # return dataframe
  return(date_time)
}

impute <- function(df="date_time", ...){
  # imputes specified columns
  # does NOT impute over gaps > 20 observations
  ## df(str): name of dataframe 
  ## ...(str): name of column(s) to impute
  
  # get dataframe from global environment
  df <- get(df)
  imp_cols <- c(...)
  # impute columns
  df[imp_cols] <- na.approx(df[imp_cols],index(df),maxgap = 20, na.rm=FALSE)
  return(df)
}



########################################################################
# ST-DBSCAN : An algorithm for clustering spatial-temporal data        #
# (Birant and Kut, 2006)                                               #   
# Application on a trajectory                                          #
########################################################################

########################################################################
# INPUTS :                                                             #
# traj = traj gps (x, y and time)                                      #
# eps = distance minimum for longitude and latitude                    #
# eps2 =  distance minimum for date                                    #
# minpts = number of points to consider a cluster                      #
########################################################################

stdbscan = function (traj,
                     x,
                     y,
                     time,
                     eps,
                     eps2,
                     minpts,
                     cldensity = TRUE) {
  
  countmode = 1:length(x)
  seeds = TRUE
  
  data_spatial<- as.matrix(dist(cbind(y, x)))
  data_temporal<- as.matrix(dist(time))
  n <- nrow(data_spatial)
  
  classn <- cv <- integer(n)
  isseed <- logical(n)
  cn <- integer(1)
  
  for (i in 1:n) {
    if (i %in% countmode)
      #cat("Processing point ", i, " of ", n, ".\n")
      unclass <- (1:n)[cv < 1]
    
    if (cv[i] == 0) {
      reachables <- intersect(unclass[data_spatial[i, unclass] <= eps],  unclass[data_temporal[i, unclass] <= eps2])
      if (length(reachables) + classn[i] < minpts)
        cv[i] <- (-1)                    
      else {
        cn <- cn + 1                   
        cv[i] <- cn
        isseed[i] <- TRUE
        reachables <- setdiff(reachables, i)
        unclass <- setdiff(unclass, i)       
        classn[reachables] <- classn[reachables] + 1
        while (length(reachables)) {
          cv[reachables] <- cn           
          ap <- reachables                           
          reachables <- integer()
          
          for (i2 in seq(along = ap)) {
            j <- ap[i2]
            
            jreachables <- intersect(unclass[data_spatial[j, unclass] <= eps], unclass[data_temporal[j, unclass] <= eps2])
            
            if (length(jreachables) + classn[j] >= minpts) {
              isseed[j] <- TRUE
              cv[jreachables[cv[jreachables] < 0]] <- cn
              reachables <- union(reachables, jreachables[cv[jreachables] == 0])
            }
            classn[jreachables] <- classn[jreachables] + 1
            unclass <- setdiff(unclass, j)
          }
        }
      }
    }
    if (!length(unclass))
      break
    
  }
  
  
  if (any(cv == (-1))) {
    cv[cv == (-1)] <- 0
  }
  out <- list(cluster = cv, eps = eps, minpts = minpts, density = classn)
  rm(classn)
  if (seeds && cn > 0) {
    out$isseed <- isseed
  }
  class(out) <- "stdbscan"
  return(out)
}

project_coordinates <- function(df="data_x", x="Long", y="Lat"){
  # returns a dataframe with two new columns containing projected coordinates to NAD83
  ## df(str): dataframe to use
  ## x (str): name of column containing Longitude in degrees
  ## y (str): name of column containing Latitude in degrees
  df <- get(df)
  proj <- "+proj=lcc +lat_1=39.93333333333333 +lat_2=40.96666666666667 +lat_0=39.33333333333334 +lon_0=-77.75 +x_0=600000.0000000001 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 no_defs"
  lat_long_proj <- project(as.matrix(cbind(df[x],df[y])),proj)
  df["Long_proj"] <-lat_long_proj[,1]
  df["Lat_proj"] <- lat_long_proj[,2]
  return(df)
}

scenario_1 <- function(df="data_x",max_dist=200, time_gap_from=20, time_gap_to=720){
  # assigns clusters to unclustered observations that fulfill the criteria for scenario 1
  ## df (str): name of dataframe to use
  ## max_dist (numeric): max distance between observations to be clustered
  ## time_gap_from (numeric): minimum time_gap between observations to be clustered
  ## time_gap_to (numeric): maximum time_gap between observations to be clustered
  
  df <- get(df)
  cluster_list <- c()
  for (i in seq(2,nrow(df))){
    if(between(df$time_gap[i],time_gap_from,time_gap_to)  # if 20 < time_gap < 720 mins
       & df$distance[i] < max_dist                        # and distance < 200
       & df$stdb_cluster[i]==0                            # and current stdb_cluster = 0
       & df$stdb_cluster[i-1]==0                          # and prev stdb_cluster = 0
       & is.na(df$ID[i])==FALSE                           # and current obs is not imputed
       & is.na(df$ID[i-1])==FALSE){                       # and prev obs is not imputed
      stdb_cluster_id <- max(df$stdb_cluster)+1           # create new stdb_cluster number
      df$stdb_cluster[i] <- stdb_cluster_id               # assign new stdb_cluster number to current obs
      df$stdb_cluster[i-1] <- stdb_cluster_id             # assign new stdb_cluster number to prev obs 
      cluster_list<- c(cluster_list,stdb_cluster_id)
    }
  }                                  
  cat("","Scenario 1:", sprintf("Number of stdb_clusters created: %d",length(cluster_list)),sep="\n")
  cat("ID(s) of stdb_clusters created:",cluster_list)
  return(df)
}


scenario_2 <- function(df="data_x",max_dist=200, time_gap_from=20, time_gap_to=720){
  # adds unclustered observation (p) to the stdb_cluster of its nearest clustered point (q) 
  # if p and q fulfill eps1 and minpts requirements, and are within 20 - 720 minutes of eachother. (p PRECEDES q)
  ## df (str): name of dataframe to use
  ## max_dist (numeric): max distance between p and q
  ## time_gap_from (numeric): minimum time_gap between p and q
  ## time_gap_to (numeric): maximum time_gap between p and q
  
  df <- get(df)
  clustered_obs <- 0
  cat("\n","Scenario 2:",sep="")
  for (i in seq(1,nrow(df)-1)){
    if(between(df$time_gap[i],time_gap_from,time_gap_to) # if 20 < time_gap < 720 mins
       & df$distance[i] < max_dist                       # and distance < 200
       & df$stdb_cluster[i]==0                           # and current stdb_cluster = 0
       & df$stdb_cluster[i+1]!=0){                       # and next stdb_cluster != 0
      stdb_cluster_id <- df$stdb_cluster[i+1]                      
      df$stdb_cluster[i] <- stdb_cluster_id              # add current obs to stdb_cluster of next obs
      cat("\n","Adding observation ID",df$ID[i],"to stdb_cluster",stdb_cluster_id)
      clustered_obs <- clustered_obs + 1
    }
  }
  if (clustered_obs == 0){
    cat("\nstdb_clusters unchanged")
  }
  return(df)
} 


scenario_3 <- function(df="data_x",max_dist=200, time_gap_from=20, time_gap_to=720){
  # adds unclustered observation (p) to the stdb_cluster of its nearest clustered point (q) 
  # if p and q fulfill eps1 and minpts requirements, and are within 20 - 720 minutes of eachother. (p FOLLOWS q)
  ## df (str): name of dataframe to use
  ## max_dist (numeric): max distance between p and q
  ## time_gap_from (numeric): minimum time_gap between p and q
  ## time_gap_to (numeric): maximum time_gap between p and q
  
  df <- get(df)
  clustered_obs <- 0
  cat("\n","Scenario 3:",sep="")
  for (i in seq(2,nrow(df))){
    if(between(df$time_gap[i],time_gap_from,time_gap_to) # if 20 < time_gap < 720 mins
       & df$distance[i] < max_dist                       # and distance < 200
       & df$stdb_cluster[i]==0                           # and current stdb_cluster = 0
       & df$stdb_cluster[i-1]!=0){                       # and prev stdb_cluster != 0
      stdb_cluster_id <- df$stdb_cluster[i-1]
      df$stdb_cluster[i] <- stdb_cluster_id              # add current obs to stdb_cluster of prev obs
      cat("\n","Adding observation ID ",df$ID[i]," to stdb_cluster ",stdb_cluster_id,sep="")
      clustered_obs <- clustered_obs + 1
    }
  }
  if (clustered_obs == 0){
    cat("\nstdb_clusters unchanged")
  }
  return(df)
}


stdb_cluster_chars <- function(df="data_x", lat_col="Lat", long_col="Long", date_col="Date"){
  # returns a dataframe containing centroid coordinates and time range for every stdb_cluster
  ## df(str): name of dataframe to use
  ## lat_col (str): name of column containing latitude 
  ## long_col (str): name of column containing longitude 
  ## date_col (str): name of column containing timestamps
  
  df <- get(df)
  # initialize dataframe
  stdb_centroid_coords_all <- data.frame("stdb_cluster_id"=c(), "long_centroid"=c(),"lat_centroid"=c(), "begin"=c(),"end"=c())
  # make three lists: long,lat, and timestamps for all points in cluster_x
  for (cluster_x in unique(df$stdb_cluster)){
    lat_list <- c()          
    long_list <- c()
    time_list <- c()
    for (i in seq(1,nrow(df))){
      if (df$stdb_cluster[i]==cluster_x){
        lat_list <- c(lat_list,df[[i,lat_col]])
        long_list <- c(long_list,df[[i,long_col]])
        time_list <- c(time_list,df[[i,date_col]])
      }
    }
    # use lists to make a dataframe with the avg long, avg lat, start time, and end time for cluster_x
    stdb_centroid_coords_x <- data.frame("stdb_cluster_id"=cluster_x 
                                         ,"long_centroid" = sum(long_list)/length(long_list)
                                         ,"lat_centroid"=sum(lat_list)/length(lat_list)
                                         ,"begin"=as.POSIXct(min(time_list),origin="1970-01-01")
                                         ,"end"=as.POSIXct(max(time_list),origin="1970-01-01"))
    # bind centroid_coords_x df to centroid_coords_all to make a dataframe with centroids and time ranges for all stdb_clusters
    stdb_centroid_coords_all <- rbind(stdb_centroid_coords_all,stdb_centroid_coords_x)
  }
  return(stdb_centroid_coords_all)
}


stdb_to_combine <- function(df="cluster_chars_x",max_dist=200, time_gap_from=20, time_gap_to=720){
  # calculates distance between centroids and time_gap for every stdb_cluster combination
  # then determines if stdb_clusters are within 200 meters and 20-720 minutes of each other
  # returns a dataframe of stdb_clusters that fulfill the requirements, with cluster distance and cluster time_gap calculations
  ## df (str): name of dataframe to use
  ## max_dist (numeric): max distance between stdb_clusters
  ## time_gap_from (numeric): minimum time_gap between stdb_clusters
  ## time_gap_to (numeric): maximum time_gap between stdb_clusters
  
  df <- get(df)
  # create a duplicate of df with different column names
  df_dup <- df
  colnames(df_dup) <- c("stdb_cluster_id_b","long_centroid_b","lat_centroid_b","begin_b","end_b")
  # cross join df with df_dup to get all stdb_cluster combos
  all_combos <- merge(df,df_dup,by=NULL)
  # drop duplicate combinations (4/5 vs 5/4)
  all_combos <- all_combos[which(all_combos$stdb_cluster_id < all_combos$stdb_cluster_id_b),]
  # calculate stdb_cluster distances and time_gaps 
  for (i in seq(1,nrow(all_combos))){
    xy_a <- c(all_combos[i,"long_centroid"],all_combos[i,"lat_centroid"])
    xy_b <- c(all_combos[i,"long_centroid_b"],all_combos[i,"lat_centroid_b"])
    all_combos[i,"clust_dist"]<-distVincentyEllipsoid(xy_a,xy_b)
    
    begin_b <- all_combos[i,"begin_b"]
    end <- all_combos[i,"end"]
    all_combos[i,"clust_time_gap"]<- as.integer(difftime(begin_b,end, units="mins"))
  }
  # mark stdb_clusters that are directly density reachable and adjacent
  for (i in seq(1,nrow(all_combos))){
    if (all_combos$clust_dist[i] <= max_dist
        & between(all_combos$clust_time_gap[i],time_gap_from,time_gap_to)
        & all_combos$stdb_cluster_id[i] == (all_combos$stdb_cluster_id_b[i]-1)){
      all_combos[i,"ddr_adjacent"] <- 1
    }else{all_combos[i,"ddr_adjacent"] <- 0}
  }
  # remove redundant rows and columns
  all_combos <- all_combos[-which(all_combos$stdb_cluster_id==all_combos$stdb_cluster_id_b 
                                  | all_combos$ddr_adjacent==0),]
  all_combos <- all_combos[,c("stdb_cluster_id","stdb_cluster_id_b","clust_dist","clust_time_gap","ddr_adjacent")]
  # return the df of directly density reachable stdb_clusters, ordered by stdb_cluster_id
  return(all_combos[order(all_combos$stdb_cluster_id),])
}


scenario_4 <- function(df="data_x",df2="stdb_clusters_to_combine_x"){
  # combines stdb_clusters if centroids are directly density reachable (ddr) and adjacent
  # calculates distance between centroids and time_gap for every stdb_cluster combination
  # then determines if stdb_clusters are ddr and adjacent
  # returns a dataframe of adjacent ddr stdb_clusters, with distance and time_gap calculations
  ## df (str): name of dataframe containing participant data
  ## df2(str): name of dataframe containing ddr stdb_clusters
  df <- get(df)
  df2<- get(df2) 
  cat("Scenario 4:",sep="")
  
  if (nrow(df2)==0){
    cat("\nstdb_clusters unchanged")
  }else{
    # combine stdb_clusters
    for (ddr in seq(1,nrow(df2))){
      cat("\ncombining stdb_cluster",df2$stdb_cluster_id[ddr],"and",df2$stdb_cluster_id_b[ddr],sep=" ")
      for (i in seq(1,nrow(df))){
        if(df$stdb_cluster[i]==df2$stdb_cluster_id[ddr]){   # match on earlier stdb_cluster_id
          df$stdb_cluster[i] <- df2$stdb_cluster_id_b[ddr]  # combine as later stdb_cluster_id
        }}}}                                                         
  return(df)                                 
  
}


cluster_plot <- function(postprocessed_path, dbscan_path, output_path){
  # generates a pdf containing a plot of stdbscan cluster centroids, dbscan clusters, and noise points
  ## postprocessed_path (str): path to participant's postprocessed file (ex. Participant 1001 postprocessed.csv)
  ## dbscan_path (str): path to participant's DBSCAN file (ex. Participant 1001 DBSCAN.csv)
  ## output_path (str): path to pdf (ex. ./1001/cluster_plot.pdf)
  
  # import data
  data_x <- read.csv(postprocessed_path, stringsAsFactors = FALSE)
  dbscan_x <- read.csv(dbscan_path, stringsAsFactors = FALSE)
  # participant number
  participant_number <- substr(postprocessed_path,104,107) 
  # centroids of non-noise stdbscan clusters
  stdb_no_noise <- which(dbscan_x[,"stdb_cluster"]!=0)
  xy_cols <- c("long_centroid_projected_stdb","lat_centroid_projected_stdb")
  stdb_xy <- dbscan_x[stdb_no_noise,xy_cols]
  # coordinates of non-noise dbscan clusters
  dbscan_no_noise <- which(dbscan_x[,"dbscan_cluster"]!=0)
  dbscan_xy <- dbscan_x[dbscan_no_noise,xy_cols]
  # coordinates of noise points from stdbscan clustering
  stdb_noise <- which(data_x[,"stdb_cluster"]==0)
  noise_xy <- data_x[stdb_noise,c("Long_proj","Lat_proj")]
  # intialize pdf 
  pdf(output_path)
  # plot transit points (noise)
  plot(noise_xy,type="p",col="gray", pch="-", main=sprintf("Participant %s Clustered Data",participant_number))
  # add stdb cluster centroids (non-noise)
  points(stdb_xy,col="blue", pch=15)
  # add dbscan cluster centroids (non-noise)
  points(dbscan_xy,col="red", pch=20)
  # add legend
  legend("bottomleft", legend=c("noise", "stdbscan clusters", "dbscan clusters"),
         col=c("gray", "blue", "red"), fill=c("gray", "blue", "red"))
  # close pdf
  dev.off()
}

silhouette_dbscan <- function(dbscan_path,output_path){
  # generates a pdf containing a silhouette plot of dbscan clusters
  ## dbscan_path (str): path to participants DBSCAN file (ex. Participant 1001 DBSCAN.csv)
  ## output_path (str): path to pdf (ex. ./1001/silhouette_dbscan.pdf)
  
  df <- read.csv(dbscan_path, stringsAsFactors=FALSE)
  # participant number
  participant_number <- substr(dbscan_path,102,105) 
  # remove noise points
  dbscan_x_no_noise <- df[df[,"dbscan_cluster"]!=0,]
  # create distance object
  xy_cols <- c("long_centroid_projected_stdb","lat_centroid_projected_stdb")
  xy <- dbscan_x_no_noise[,xy_cols]
  dist <- dist(xy,"euclidian")
  # define cluster vector
  clusters <- dbscan_x_no_noise$dbscan_cluster
  # run silhouette analysis
  sil <- silhouette(clusters,dist)
  # define plot colors
  cluster_count <- length(unique(clusters))
  cluster_col <- brewer.pal(9,"Set1")
  cluster_col = colorRampPalette(cluster_col)(cluster_count)
  # initialize pdf
  pdf(output_path)
  # define plot
  plot(sil, border=NA,col=cluster_col[sort(clusters)],main=sprintf("Participant %s: silhouette plot of dbscan_clusters",participant_number))
  # close pdf
  dev.off()
  
}

chull_area_plot <- function(postprocessed_path, output_path){
  # generates a pdf containing a plot of the convex hull for the specified participant
  ## postprocessed_path (str): path to participants postprocessed file (ex. Participant 1001 postprocessed.csv)
  ## output_path (str): path to pdf (ex. ./1001/chull plot.pdf)
  
  # participant number
  participant_number <- substr(postprocessed_path,104,107) 
  # load data
  data_x <- read.csv(postprocessed_path, stringsAsFactors = FALSE)
  # extract projected coordinates
  xy<-data_x[,c("Long_proj","Lat_proj")]
  # return indexes corresponding coordinates of convex hull
  chull_pts <- chull(xy)
  # append first index to end of list created above (to create a closed loop)
  chull_pts <- c(chull_pts, chull_pts[1])
  # extract chull coordinates
  chull_pts <- xy[chull_pts,]
  # calculate area of chull
  chull_area <- polyarea(chull_pts[,1],chull_pts[,2])
  # initialize pdf
  pdf(output_path)
  # initialize plot
  plot(chull_pts, type="n", main=sprintf("Participant %s: Convex Hull Area = %.0f meters squared",participant_number,chull_area))
  # add polygon to plot
  polygon(chull_pts)
  # close pdf
  dev.off()
}

