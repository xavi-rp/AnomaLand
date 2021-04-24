

## setting working directory

if(Sys.info()[4] == "MacBook-MacBook-Pro-de-Xavier.local"){
  dirctry <- "/Users/xavi_rp/Documents/D6_LPD/Anomalies/Anomalies_Kenya/"
}else if (Sys.info()[4] == "L2100346RI"){   #new laptop
  dirctry <- "C:/Users/rotllxa/D6_UseCases/Anomalies/Anomalies_Kenya/"
}else{
  dirctry <- "E:/rotllxa/D6_UseCases/Anomalies/Anomalies_Kenya/"   # PC JRC
}
if(!dir.exists(dirctry)) dir.create(dirctry)
setwd(dirctry)



## Coordinate for subsetting
coords4subset <- c(33, 43, -6, 6)  # Kenya
coords4subset <- c(21, 53, -6.5, 22)  # Horn of Africa


## Creating a RasterLayer with adjusted coordinates for LTS (1999-2019) ####
library(ncdf4)
library(raster)

all_files <- list.files(pattern = "nc$", full.names = TRUE)
all_files <- all_files[grepl("LTS", all_files)]
all_files

dekads <- c("0301", "0311", "0321")
all_files1 <- all_files
all_files1 <- gsub("./c_gls_NDVI-LTS_1999-2019-", "", all_files1)
all_files1 <- gsub("_GLOBE_VGT-PROBAV_V3.0.1.nc", "", all_files1)
dekads <- all_files1
dekads


for(fl in 1:length(all_files)){
  #nc <- nc_open(all_files[fl])
  #lon <- ncvar_get(nc, "lon")
  #lat <- ncvar_get(nc, "lat")
  #
  ## variable 1: average
  #ndvi_lts_1km <- ncvar_get(nc, "mean")
  #dim(ndvi_lts_1km)
  ## variable 2: standard deviation
  #ndvi_lts_1km_sd <- ncvar_get(nc, "stdev")
  #dim(ndvi_lts_1km_sd)
  #
  ## Copernicus nc files have lat/long belonging to the centre of the pixel, and R uses 
  ## upper/left corner. So coordiinates need to be adjusted
  ## For 1km products 
  #lon <- lon - (1/112)/2
  #lat <- lat + (1/112)/2  
  
  ## mean
  ##ndvi_lts_1km_rstr <- raster(t(ndvi_lts_1km[, seq(dim(ndvi_lts_1km)[2], 1, -1)]))
  #ndvi_lts_1km_rstr <- raster(t(ndvi_lts_1km))
  #extent(ndvi_lts_1km_rstr) <- c(range(lon)[1], (range(lon)[2] + (1/112)),
  #                               (range(lat)[1] - (1/112)), range(lat)[2])
  #crs(ndvi_lts_1km_rstr) <- CRS('+init=EPSG:4326')
  ##plot(ndvi_lts_1km_rstr)
  #
  #
  ## SD
  #ndvi_lts_1km_rstr_sd <- raster(t(ndvi_lts_1km_sd))
  #extent(ndvi_lts_1km_rstr_sd) <- c(range(lon)[1], (range(lon)[2] + (1/112)),
  #                                  (range(lat)[1] - (1/112)), range(lat)[2])
  #crs(ndvi_lts_1km_rstr_sd) <- CRS('+init=EPSG:4326')
  #ndvi_lts_1km_rstr_sd
  #
  #
  #ndvi_lts_1km_rstr <- brick(ndvi_lts_1km_rstr, ndvi_lts_1km_rstr_sd)
  #names(ndvi_lts_1km_rstr) <- c("mean", "sd")
  #
  ##writeRaster(ndvi_lts_1km_rstr, filename = paste0("ndvi_lts_1km_rstr_", dekads[fl], ".tif"), overwrite = TRUE)
  #nc_close(nc)
  
  ndvi_lts_1km_rstr <- raster(all_files[fl], varname = "mean")
  ndvi_lts_1km_rstr_sd <- raster(all_files[fl], varname = "stdev")
  ndvi_lts_1km_rstr <- brick(ndvi_lts_1km_rstr, ndvi_lts_1km_rstr_sd)
  names(ndvi_lts_1km_rstr) <- c("mean", "sd")
  
  
  
  ## Subsetting to reduce raster's dimensions (see https://github.com/xavi-rp/ResampleTool_notebook)
  #coords4subset <- c(33, 43, -6, 6)
  my_extent <- extent(coords4subset)
  
  # Adjusting my_extent to coordinates belonging to the 1km grid
  # The following vectors contain Long and Lat coordinates, respectively, of the 1km grid (cell boundaries):
  x_ext <- seq((-180 - ((1 / 112) / 2)), 180, (1/112))
  y_ext <- seq((80 + ((1 / 112) / 2)), - 60, - (1/112))
  
  if(!all(round(my_extent[1], 7) %in% round(x_ext, 7) &
          round(my_extent[2], 7) %in% round(x_ext, 7) &
          round(my_extent[3], 7) %in% round(y_ext, 7) &
          round(my_extent[4], 7) %in% round(y_ext, 7))){
    # The given extent from raster or coordinate vector does not fit into the 1km PROBA-V grid, so we are going to adjust it
    for(crd in 1:length(as.vector(my_extent))){
      if(crd <= 2){
        my_extent[crd] <- x_ext[order(abs(x_ext - my_extent[crd]))][1]
      }else{
        my_extent[crd] <- y_ext[order(abs(y_ext - my_extent[crd]))][1]
      }
    }
    
    # Now we can crop the 300m raster
    ndvi_lts_1km_rstr <- crop(ndvi_lts_1km_rstr, my_extent)
  }
  
  
  
  ndvi_lts_1km_rstr_clean <- ndvi_lts_1km_rstr$mean
  ndvi_lts_1km_rstr_clean[ndvi_lts_1km_rstr_clean >= 0.9359999] <- NA # it should be 0.936, but the floating point inconsistences when raster() nc files
                                                                      # makes that in has to be 0.9359999
  
  ndvi_lts_1km_rstr_clean_sd <- ndvi_lts_1km_rstr$sd
  ndvi_lts_1km_rstr_clean_sd[ndvi_lts_1km_rstr_clean_sd >= 1.016] <- NA
  

  ndvi_lts_1km_rstr_clean <- brick(ndvi_lts_1km_rstr_clean, ndvi_lts_1km_rstr_clean_sd)
  names(ndvi_lts_1km_rstr_clean) <- c("mean", "sd")
  
  
  writeRaster(ndvi_lts_1km_rstr_clean, filename = paste0("ndvi_lts_1km_rstr_clean_", dekads[fl], ".tif"), overwrite = TRUE)
  
}




## Creating a RasterLayer with adjusted coordinates for actual dekads ####
# They need to be resampled from 333m to 1km

all_files <- list.files(pattern = "nc$", full.names = TRUE)
all_files <- all_files[!grepl("LTS", all_files)]
all_files


for(fl in 1:length(all_files)){

  ndvi_300m_rstr <- raster(all_files[fl])
  
  ## Subsetting to reduce raster's dimensions and resample to 1km (see https://github.com/xavi-rp/ResampleTool_notebook)
  #coords4subset <- c(33, 43, -6, 6)
  my_extent <- extent(coords4subset)
  
  # Adjusting my_extent to coordinates belonging to the 1km grid
  # The following vectors contain Long and Lat coordinates, respectively, of the 1km grid (cell boundaries):
  x_ext <- seq((-180 - ((1 / 112) / 2)), 180, (1/112))
  y_ext <- seq((80 + ((1 / 112) / 2)), - 60, - (1/112))
  
  if(!all(round(my_extent[1], 7) %in% round(x_ext, 7) &
          round(my_extent[2], 7) %in% round(x_ext, 7) &
          round(my_extent[3], 7) %in% round(y_ext, 7) &
          round(my_extent[4], 7) %in% round(y_ext, 7))){
    # The given extent from raster or coordinate vector does not fit into the 1km PROBA-V grid, so we are going to adjust it
    for(crd in 1:length(as.vector(my_extent))){
      if(crd <= 2){
        my_extent[crd] <- x_ext[order(abs(x_ext - my_extent[crd]))][1]
      }else{
        my_extent[crd] <- y_ext[order(abs(y_ext - my_extent[crd]))][1]
      }
    }
    
    # Now we can crop the 300m raster
    ndvi_300m_rstr <- crop(ndvi_300m_rstr, my_extent)
  }
  
  #plot(ndvi_300m_rstr)
  
  
  ndvi_300m_rstr_clean <- ndvi_300m_rstr
  ndvi_300m_rstr_clean[ndvi_300m_rstr_clean >= 0.9359999] <- NA   # it should be 0.936, but the floating point inconsistences when raster() nc files
                                                                  # makes that in has to be 0.9359999
  #plot(ndvi_300m_rstr_clean)
  writeRaster(ndvi_300m_rstr_clean, filename = paste0("ndvi_300m_rstr_clean_", dekads[fl], ".tif"), overwrite = TRUE)
  
  
  ## Resampling using the 'Aggregation' method
  
  mean_w.cond <- function(x, ...){ # mean including condition 'minimum 5 valid pixels'
    n_valid <- sum(!is.na(x)) # number of cells with valid value
    if(n_valid > 4){
      dts <- list(...)
      if(is.null(dts$na_rm)) dts$na_rm <- TRUE
      x_mean <- mean(x, na.rm = dts$na_rm)
      return(x_mean)
    }else{
      x_mean <- NA
      return(x_mean)
    }
  }
  
  aggr_method <- "mean_w.cond"
  r300m_resampled1km_Aggr <- aggregate(ndvi_300m_rstr_clean,
                                       fact = 3, # from 333m to 1km  
                                       fun = aggr_method, 
                                       na.rm = TRUE, 
                                       filename = paste0("ndvi_1km_rstr_clean_", dekads[fl], ".tif"),
                                       overwrite = TRUE)
  
}




## Averaging Actual dekads ####

library(raster)

all_files <- list.files(pattern = "tif$", full.names = TRUE)
all_files <- all_files[!grepl("300", all_files)]
all_files <- all_files[!grepl("lts", all_files)]
all_files <- all_files[!grepl("OND", all_files)]
all_files
 #dekads <- c("0301", "0311", "0321")


ndvi_1km_avrgMarch <- raster(all_files[1])
for(fl in 2:length(all_files)){
  ndvi_1km_avrgMarch <- stack(ndvi_1km_avrgMarch, raster(all_files[fl]))
}

beginCluster(3)
ndvi_1km_avrgMarch <- clusterR(ndvi_1km_avrgMarch, calc, args = list(fun = mean))
endCluster()

writeRaster(ndvi_1km_avrgMarch, filename = paste0("ndvi_1km_avrg_", "OND2020", ".tif"), overwrite = TRUE)




## Averaging Long Term Statistics ####

library(raster)

all_files <- list.files(pattern = "tif$", full.names = TRUE)
all_files <- all_files[grepl("lts", all_files)]
all_files <- all_files[!grepl("OND", all_files)]
all_files
#dekads <- c("0301", "0311", "0321")

## Mean
ndvi_lts_1km_mean_avrgMarch <- stack(all_files[1])[[1]]
for(fl in 2:length(all_files)){
  ndvi_lts_1km_mean_avrgMarch <- stack(ndvi_lts_1km_mean_avrgMarch, stack(all_files[fl])[[1]])
}


beginCluster(3)
ndvi_lts_1km_mean_avrgMarch <- clusterR(ndvi_lts_1km_mean_avrgMarch, calc, args = list(fun = mean))
endCluster()

writeRaster(ndvi_lts_1km_mean_avrgMarch, filename = paste0("ndvi_lts_1km_mean_avrg_", "OND2020", ".tif"), overwrite = TRUE)


## SD
SD_avrge <- function(x) {sqrt((sum(x^2)/length(x)))}   # square root of the pooled (or weighted) variances
                                                       # http://www.talkstats.com/threads/an-average-of-standard-deviations.14523/

ndvi_lts_1km_sd_avrgMarch <- stack(all_files[1])[[2]]
for(fl in 2:length(all_files)){
  ndvi_lts_1km_sd_avrgMarch <- stack(ndvi_lts_1km_sd_avrgMarch, stack(all_files[fl])[[2]])
}
ndvi_lts_1km_sd_avrgMarch

beginCluster(3)
ndvi_lts_1km_sd_avrgMarch <- clusterR(ndvi_lts_1km_sd_avrgMarch, calc, args = list(fun = SD_avrge))
endCluster()

writeRaster(ndvi_lts_1km_sd_avrgMarch, filename = paste0("ndvi_lts_1km_sd_avrg_", "OND2020", ".tif"), overwrite = TRUE)




