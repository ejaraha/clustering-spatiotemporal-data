# clustering spatiotemporal data

overview:
- preprocessing: remove duplicates and outliers, impute data points, create calculated fields
- cluster analysis: ST-DBSCAN algorithm
- postprocessing: handle special-case observations, cluster centroids using DBSCAN algorithm

data:
- GPS data collected as part of an ongoing study on community participation for individuals with serious mental illnesses
- see PDF: 2016 - Brusilovskiy, Klein, Salzer - Using GPS To Study Health-Related Mobility and Participation.pdf

tools:
- R (geosphere, dplyr, zoo,rgdal,dbscan,cluster,RColorBrewer,geometry,grDevices,sp)
- RStudio
- RMarkdown
