

land_product <- "ndvi"
current_year <- 2019
decade2use <- "08/01"   # month/day


## Download Long Term Statistics 

library(RCGLS)
# Copernicus Land user/password
load("~/Google Drive/CopernicusCredentials/copernicus_credentials.RData")

USERNAME   <- copernicus_usr
PASSWORD   <- copernicus_pwrd
TIMEFRAME  <- decade2use
PRODUCT    <- paste0(land_product, "-lts")
RESOLUTION <- "1km"
VERSION    <- "v2"


setwd("/Users/xavi_rp/Documents/D6_UseCases/Anomalies/")

download_CGLS_data(username = USERNAME, password = PASSWORD, 
                   timeframe = TIMEFRAME, product = PRODUCT, 
                   resolution = RESOLUTION, version = VERSION)

# Creating a RasterLayer with adjusted coordinates
library(ncdf4)
library(raster)
nc <- nc_open("ndvi_v2_1km_c_gls_NDVI_201908010000_GLOBE_PROBAV_V2.2.1.nc")
nc$var$NDVI

lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")
# if you have time series
#time <- ncvar_get(nc, "time") # need to find the right name in 'nc'
# variable 1
ndvi1km <- ncvar_get(nc, "NDVI")
dim(ndvi1km)

# Copernicus nc files have lat/long belonging to the centre of the pixel, and R uses 
# upper/left corner. So coordiinates need to be adjusted
# For 1km products 
lon <- lon - (1/112)/2
lat <- lat + (1/112)/2  

ndvi_1km_rstr <- raster(t(ndvi1km[, seq(dim(ndvi1km)[2], 1, -1)]))
extent(ndvi_1km_rstr) <- c(range(lon)[1], (range(lon)[2] + (1/112)),
                           (range(lat)[1] - (1/112)), range(lat)[2])
crs(ndvi_1km_rstr) <- CRS('+init=EPSG:4326')


#

## Download "current" product 
TIMEFRAME <- paste0(current_year, "/", decade2use) 
PRODUCT   <- land_product

download_CGLS_data(username = USERNAME, password = PASSWORD, 
                   timeframe = TIMEFRAME, product = PRODUCT, 
                   resolution = RESOLUTION, version = VERSION)



## Download Land Cover

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

download.file(url = lc_map_file, destfile = paste0("lc_map_", current_year))