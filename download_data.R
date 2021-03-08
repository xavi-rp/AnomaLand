
## setting working directory

if(Sys.info()[4] == "MacBook-MacBook-Pro-de-Xavier.local"){
  dirctry <- "/Users/xavi_rp/Documents/D6_LPD/Anomalies/"
}else if (Sys.info()[4] == "L2100346RI"){   #new laptop
  dirctry <- "C:/Users/rotllxa/D6_UseCases/Anomalies/"
}else{
  dirctry <- "E:/rotllxa/D6_UseCases/Anomalies/"
}
if(!dir.exists(dirctry)) dir.create(dirctry)
setwd(dirctry)

library(raster)

## Setting parameters

land_product <- "ndvi"
current_year <- 2019
dekad2use <- "08/01"   # month/day



## Download Long Term Statistics 

library(RCGLS)
# Copernicus Land user/password
load("~/Google Drive/CopernicusCredentials/copernicus_credentials.RData")
load("E:/rotllxa/copernicus_credentials.RData")

USERNAME   <- copernicus_usr
PASSWORD   <- copernicus_pwrd
TIMEFRAME  <- decade2use
PRODUCT    <- paste0(land_product, "-lts")
RESOLUTION <- "1km"
VERSION    <- "v2"


setwd("E:/rotllxa/D6_UseCases/Anomalies/")

download_CGLS_data(username = USERNAME, password = PASSWORD, 
                   timeframe = TIMEFRAME, product = PRODUCT, 
                   resolution = RESOLUTION, version = VERSION)

# Creating a RasterLayer with adjusted coordinates
library(ncdf4)
library(raster)
nc <- nc_open("ndvi-lts_v2_1km_c_gls_NDVI-LTS_1999-2017-0801_GLOBE_VGT-PROBAV_V2.2.1.nc")
nc$var$mean
nc$var$stdev

lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")
# if you have time series
#time <- ncvar_get(nc, "time") # need to find the right name in 'nc'

# variable 1: average
ndvi_lts_1km <- ncvar_get(nc, "mean")
dim(ndvi_lts_1km)

# variable 2: standard deviation
ndvi_lts_1km_sd <- ncvar_get(nc, "stdev")
dim(ndvi_lts_1km_sd)


# Copernicus nc files have lat/long belonging to the centre of the pixel, and R uses 
# upper/left corner. So coordiinates need to be adjusted
# For 1km products 
lon <- lon - (1/112)/2
lat <- lat + (1/112)/2  

# mean
#ndvi_lts_1km_rstr <- raster(t(ndvi_lts_1km[, seq(dim(ndvi_lts_1km)[2], 1, -1)]))
ndvi_lts_1km_rstr <- raster(t(ndvi_lts_1km))
extent(ndvi_lts_1km_rstr) <- c(range(lon)[1], (range(lon)[2] + (1/112)),
                               (range(lat)[1] - (1/112)), range(lat)[2])
crs(ndvi_lts_1km_rstr) <- CRS('+init=EPSG:4326')
ndvi_lts_1km_rstr


# SD
ndvi_lts_1km_rstr_sd <- raster(t(ndvi_lts_1km_sd))
extent(ndvi_lts_1km_rstr_sd) <- c(range(lon)[1], (range(lon)[2] + (1/112)),
                                  (range(lat)[1] - (1/112)), range(lat)[2])
crs(ndvi_lts_1km_rstr_sd) <- CRS('+init=EPSG:4326')
ndvi_lts_1km_rstr_sd

ndvi_lts_1km_rstr <- brick(ndvi_lts_1km_rstr, ndvi_lts_1km_rstr_sd)
names(ndvi_lts_1km_rstr) <- c("mean", "sd")

writeRaster(ndvi_lts_1km_rstr, filename = "ndvi_lts_1km_rstr.tif", overwrite = TRUE)
ndvi_lts_1km_rstr <- brick("ndvi_lts_1km_rstr.tif")
names(ndvi_lts_1km_rstr) <- c("mean", "sd")


plot(ndvi_lts_1km_rstr[[1]])
plot(ndvi_lts_1km_rstr[[2]])
nc_close(nc)#


ndvi_lts_1km_rstr_vals <- getValues(ndvi_lts_1km_rstr[[1]])
sum(is.na(ndvi_lts_1km_rstr_vals))
sum(ndvi_lts_1km_rstr_vals >= 0.936, na.rm = TRUE)
max(ndvi_lts_1km_rstr_vals, na.rm = TRUE)

ndvi_lts_1km_rstr_vals <- getValues(ndvi_lts_1km_rstr[[2]])
sum(ndvi_lts_1km_rstr_vals >= 1.016, na.rm = TRUE)


ndvi_lts_1km_rstr_clean <- ndvi_lts_1km_rstr$mean
ndvi_lts_1km_rstr_clean[ndvi_lts_1km_rstr_clean >= 0.936] <- NA

ndvi_lts_1km_rstr_clean_sd <- ndvi_lts_1km_rstr$sd
ndvi_lts_1km_rstr_clean_sd[ndvi_lts_1km_rstr_clean_sd >= 1.016] <- NA
plot(ndvi_lts_1km_rstr_clean_sd)

ndvi_lts_1km_rstr_clean <- brick(ndvi_lts_1km_rstr_clean, ndvi_lts_1km_rstr_clean_sd)
names(ndvi_lts_1km_rstr_clean) <- c("mean", "sd")


writeRaster(ndvi_lts_1km_rstr_clean, filename = "ndvi_lts_1km_rstr_clean.tif", overwrite = TRUE)
ndvi_lts_1km_rstr_clean <- brick("ndvi_lts_1km_rstr_clean.tif")
names(ndvi_lts_1km_rstr_clean) <- c("mean", "sd")

plot(ndvi_lts_1km_rstr_clean[[1]])
plot(ndvi_lts_1km_rstr_clean[[2]])



## Download "current" product 
TIMEFRAME <- paste0(current_year, "/", dekad2use) 
PRODUCT   <- land_product

download_CGLS_data(username = USERNAME, password = PASSWORD, 
                   timeframe = TIMEFRAME, product = PRODUCT, 
                   resolution = RESOLUTION, version = VERSION)

nc <- nc_open("ndvi_v2_1km_c_gls_NDVI_201908010000_GLOBE_PROBAV_V2.2.1.nc")
nc$var$NDVI

lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")
# if you have time series
#time <- ncvar_get(nc, "time") # need to find the right name in 'nc'
# variable 1
ndvi_1km <- ncvar_get(nc, "NDVI")
dim(ndvi_1km)

# Copernicus nc files have lat/long belonging to the centre of the pixel, and R uses 
# upper/left corner. So coordiinates need to be adjusted
# For 1km products 
lon <- lon - (1/112)/2
lat <- lat + (1/112)/2  

#ndvi_lts_1km_rstr <- raster(t(ndvi_lts_1km[, seq(dim(ndvi_lts_1km)[2], 1, -1)]))
ndvi_1km_rstr <- raster(t(ndvi_1km))
extent(ndvi_1km_rstr) <- c(range(lon)[1], (range(lon)[2] + (1/112)),
                           (range(lat)[1] - (1/112)), range(lat)[2])
crs(ndvi_1km_rstr) <- CRS('+init=EPSG:4326')
ndvi_1km_rstr

writeRaster(ndvi_1km_rstr, filename = "ndvi_1km_rstr.tif", overwrite = TRUE)
ndvi_1km_rstr <- raster("ndvi_1km_rstr.tif")

plot(ndvi_1km_rstr)
nc_close(nc)#

ndvi_1km_rstr_vals <- getValues(ndvi_1km_rstr)
sum(ndvi_1km_rstr_vals >= 0.936, na.rm = TRUE)
max(ndvi_1km_rstr_vals, na.rm = TRUE)

ndvi_1km_rstr_clean <- ndvi_1km_rstr
ndvi_1km_rstr_clean[ndvi_1km_rstr_clean >= 0.936] <- NA

writeRaster(ndvi_1km_rstr_clean, filename = "ndvi_1km_rstr_clean.tif")
ndvi_1km_rstr_clean <- raster("ndvi_1km_rstr_clean.tif")
plot(ndvi_1km_rstr_clean)



## Download Land Cover (100m)

# 2015
lc_map_file <- "https://zenodo.org/record/3939038/files/PROBAV_LC100_global_v3.0.1_2015-base_Discrete-Classification-map_EPSG-4326.tif?download=1"

# 2016
lc_map_file <- "https://zenodo.org/record/3518026/files/PROBAV_LC100_global_v3.0.1_2016-conso_Discrete-Classification-map_EPSG-4326.tif?download=1"

# 2017
lc_map_file <- "https://zenodo.org/record/3518036/files/PROBAV_LC100_global_v3.0.1_2017-conso_Discrete-Classification-map_EPSG-4326.tif?download=1"

# 2018
lc_map_file <- "https://zenodo.org/record/3518038/files/PROBAV_LC100_global_v3.0.1_2018-conso_Discrete-Classification-map_EPSG-4326.tif?download=1"

# 2019
lc_map_file <- "https://zenodo.org/record/3939050/files/PROBAV_LC100_global_v3.0.1_2019-nrt_Discrete-Classification-map_EPSG-4326.tif?download=1"

download.file(url = lc_map_file, destfile = paste0("lc_map_", current_year, ".tif"))



lc_map_rtsr <- raster(paste0("lc_map_", current_year))
lc_map_rtsr




# Resampling LC from 100m to 1km
# The method has to be nearest neighbor (ngb) because the variable is categorical.
# It takes the value of the nearest pixel. This is quite poor when resampling from 100m to 1km

t0 <- Sys.time()
lc_map_1km <- resample(x = lc_map_rtsr, y = ndvi_1km_rstr_clean, method = "ngb", 
                       filename = paste0("lc_map_", current_year, "_1km.tif"),
                       overwrite = TRUE)
Sys.time() - t0

lc_map_1km
plot(lc_map_1km)


