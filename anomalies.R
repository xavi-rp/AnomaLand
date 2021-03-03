
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



## Setting parameters and loading data

# Use 'download_data.R' to get the data sets and clean them
library(raster)
library(rworldmap)


# Thresholds for anomalies:
anom1 <- 0.05
anom2 <- 0.125

anom1 <- 0.105
anom2 <- 0.225



# Select country
cntry <- NULL
cntry <- "Europe"
cntry <- "Italy"
cntry <- "France"
cntry <- c("Italy", "France")
cntry <- "Africa"
avoid_cntry <- "Russia"


# World map
wrld_map <- getMap()
#head(wrld_map)
#unique(wrld_map$continent)
#unique(wrld_map$REGION)
#unique(wrld_map$NAME)

if(!is.null(cntry)) selectedMap <- wrld_map[(wrld_map$REGION %in% cntry | wrld_map$NAME %in% cntry) &
                                               !wrld_map$NAME %in% avoid_cntry, ]
#sort(unique(selectedMap$NAME))
#extent(selectedMap)
#extent(wrld_map)


# Actual NDVI product: dekad 08/01/2019 (month/day/year)

ndvi_1km_rstr_clean <- raster("ndvi_1km_rstr_clean.tif")

if(!is.null(cntry)){
  ndvi_1km_rstr_clean <- crop(ndvi_1km_rstr_clean, extent(selectedMap))
  ndvi_1km_rstr_clean <- mask(ndvi_1km_rstr_clean, selectedMap)
  #plot(ndvi_1km_rstr_clean)
}



# Long Term statistics for the same dekad
ndvi_lts_1km_rstr_clean <- brick("ndvi_lts_1km_rstr_clean.tif")
names(ndvi_lts_1km_rstr_clean) <- c("mean", "sd")

if(!is.null(cntry)){
  ndvi_lts_1km_rstr_clean <- crop(ndvi_lts_1km_rstr_clean, extent(selectedMap))
  ndvi_lts_1km_rstr_clean <- mask(ndvi_lts_1km_rstr_clean, selectedMap)
 # plot(ndvi_lts_1km_rstr_clean)
}




# Calculating anomalies: difference with reference period (LTS)
t0 <- Sys.time()
ndvi_1km_anomalies <- ndvi_1km_rstr_clean - ndvi_lts_1km_rstr_clean$mean
Sys.time() - t0

ndvi_1km_anomalies
plot(ndvi_1km_anomalies)

writeRaster(ndvi_1km_anomalies, "ndvi_1km_anomalies.tif", overwrite = TRUE)


# Plotting a map
jpeg("ndvi_1km_anomalies_Fr_It1.jpg", width = 22, height = 16.5, units = "cm", res = 300)
if(!is.null(cntry)){
   lgnd_pos <- "right"
   inst <- -0.35
}else{ 
   lgnd_pos <- "bottom"
   inst <- 0.01
}


par(mar = c(6, 4, 3, 9), bty = "n")
cuts <- c(minValue(ndvi_1km_anomalies), -anom2, -anom1, anom1, anom2, maxValue(ndvi_1km_anomalies))
pal <- colorRampPalette(c("springgreen4", "springgreen2", "khaki2", "orange2", "red3"))
par(xpd = FALSE)
plot(ndvi_1km_anomalies, breaks = cuts, col = pal(5), legend = FALSE)
par(xpd = TRUE)
legend(lgnd_pos,
       legend = c(paste0("High positive anomaly (> ", anom2, ")"),
                  paste0("Low positive anomaly (> ", anom1, ")"),
                  "No anomaly",
                  paste0("Low negative anomaly (< -", anom1, ")"),
                  paste0("High negative anomaly (< -", anom2, ")")),
       fill = pal(5),
       cex = 0.8,
       #inset = 0.01
       inset = inst
       )
title(main = "NDVI ANOMALIES",
      #outer = TRUE,
      #line = - 3.5,
      cex.main = 2)

mtext("Current layer: 'ndvi_v2_1km_c_gls_NDVI_201908010000_GLOBE_PROBAV_V2.2.1.nc'", 
      side = 1, line = 3, 
      #at = 5,
      adj = 0,
      cex = 0.8)
mtext("Reference layer: 'ndvi-lts_v2_1km_c_gls_NDVI-LTS_1999-2017-0801_GLOBE_VGT-PROBAV_V2.2.1.nc'", 
      side = 1, line = 4, 
      #at = 5,
      adj = 0,
      cex = 0.8)

dev.off()


