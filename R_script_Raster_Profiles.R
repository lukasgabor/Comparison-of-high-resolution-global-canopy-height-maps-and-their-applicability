
########################################################################################
#Code Author:
# Vitezslav Moudry - https://www.gisdata.cz

# Date: 16/11/2022
########################################################################################

# The purpose of this script is to generate profiles such as the examples in Figure 1b in
# the manuscript. This allows the reader to assess the accuracy of global canopy height maps
# also at other locations within the three study areas.


# The evaluated global canopy height maps are:

# Global Forest Canopy Height Map (GFCH): https://glad.umd.edu/dataset/gedi
# and
# A high-resolution canopy height model of the Earth (HRCH): https://langnico.github.io/globalcanopyheight/

# Licenses:

# Both products are provided free of charge, without restriction of use under Creative Commons Attribution 4.0 International License. 
# Publications, models and data products that make use of these datasets must include proper acknowledgement.

# P. Potapov, X. Li, A. Hernandez-Serna, A. Tyukavina, M.C. Hansen, A. Kommareddy, A. Pickens, S. Turubanova,
# H. Tang, C.E. Silva, J. Armston, R. Dubayah, J. B. Blair, M. Hofton (2021) Mapping and monitoring global 
# forest canopy height through integration of GEDI and Landsat data. Remote Sensing of Environment, 112165.
# https://doi.org/10.1016/j.rse.2020.112165

# Lang, N., Jetz, W., Schindler, K., & Wegner, J. D. (2022). A high-resolution canopy height model of the Earth. 
# arXiv preprint arXiv:2204.08322.

# -----------------------------------------------------------
# Load packages
# -----------------------------------------------------------
library(raster)
library(sp)
library(terra)
library(lidR)
library(ggplot2)

# -----------------------------------------------------------
# Read data
# -----------------------------------------------------------

# Set path to the data
setwd("PAthToYourData")

# ALS LiDAR
  ALS <- rast("EBR_ALS_CHM_10m.tif") 
  #plot(ALS, asp=T)
  #ALS

# HRCH Model (Lang et al. 2022)
  HRCH <- rast("EBR_HRCH_10m.tif") 
  #plot(HRCH, asp=T)
  #HRCH

# GFCH Model (Potapov et al. 2021)
  GFCH <- rast("EBR_GFCH_30m.tif") 
  #plot(GFCH, asp=T)
  #GFCH 

# -----------------------------------------------------------
# Design transect line by long/lat
# -----------------------------------------------------------
  #Read Coordinate system
  CRS <- crs(ALS)
  
  # Here specify the Latitude and Longitude of start and end point of the transect (do not use all study area extent)
  # The extent of the study areas are:
  # Entlebuch Biosphere Reserve; Trinity Alps Wilderness;  Mount Richmond Forest           
  #  Top    1215995               Top    4569000            Top    5500002
  #  Left   2630995               Left   448509             Left   1625998
  #  Right  2656995               Right  523449             Right  1721998
  #  Bottom 1177995               Bottom 4494350            Bottom 5375992
  
  Top    <- 4535594
  Left   <- 503135
  Right  <- 503935
  Bottom <- 4535594
    
  lon <- c(Left, Right)
  lat <- c(Top,Bottom)
  lonlat <- cbind(id=1, part=1, lon, lat)
  lonlat
  lns <- vect(lonlat, type="lines", crs=CRS)
  #plot(lns, add = TRUE)

  # If needed you can export the profile as a shapefile
  # writeVector(lns, "Profile", filetype=NULL, layer=NULL, insert=FALSE,
  # overwrite=FALSE, options="ENCODING=UTF-8")

# -----------------------------------------------------------
# Extract canopy height from all CHM
# -----------------------------------------------------------

  # EXTRACT ALS CHM
  elevations_ALS <- extract(ALS, lns, xy=TRUE)
  names(elevations_ALS)[names(elevations_ALS) == 'ALS_CHM_10m'] <- 'z'
  elevations_ALS$x <- elevations_ALS$x - Left
  elevations_ALS$ID <- "ALS"
  
  # EXTRACT HRCH
  elevations_HRCH <- extract(HRCH, lns, xy=TRUE)
  names(elevations_HRCH)[names(elevations_HRCH) == 'HRCH_10m'] <- 'z'
  elevations_HRCH$x <- elevations_HRCH$x - Left
  elevations_HRCH$ID <- "HRCH"
  
  # EXTRACT GFCH
  elevations_GFCH <- extract(GFCH, lns, xy=TRUE)
  names(elevations_GFCH)[names(elevations_GFCH) == 'GFCH_30m'] <- 'z'
  elevations_GFCH$x <- elevations_GFCH$x - Left
  elevations_GFCH$ID <- "GFCH"
  # Note that GFCH raster also contains values: 101 Water; 102 Snow/ice; 103 No data
  elevations_GFCH<-subset(elevations_GFCH, z <= 100) # Removal of above mentioned values
  
  # Merging the datasets
  elevations <- rbind(elevations_ALS,elevations_HRCH,elevations_GFCH)
  
# -----------------------------------------------------------
# If you download also ALS point clouds you can extract it to
# see the vegetation structure.
# -----------------------------------------------------------  
  #las <- readLAS("PathToYourLasFile")
  #p1 <- c(Left, Top)
  #p2 <- c(Right, Bottom)
  #las_tr <- clip_transect(las, p1, p2, width = 10, xz = TRUE)
  
# -----------------------------------------------------------  
# Plot profile
# ----------------------------------------------------------- 
  GRAF <- ggplot(NULL, aes(x,z, colour=ID)) + 
    # geom_point(data =las_tr@data, size = 0.5, col="gray60")+ # Only if you have also ALS point cloud
    geom_point(data = elevations, size = 3) + 
    scale_colour_manual("", 
                        breaks = c("ALS", "HRCH", "GFCH"),
                        values = c("#7570b3", "#d95f02", "#1b9e77")) +
    coord_fixed(ratio=1.7)+
    theme_bw()
  GRAF
