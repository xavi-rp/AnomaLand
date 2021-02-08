# Use 'download_data.R' to get the data sets and clean them
library(raster)

# Thresholds for anomalies:
anom1 <- 0.05
anom2 <- 0.125

# Actual NDVI product: dekad 08/01/2019 (month/day/year)
setwd("E:/rotllxa/D6_UseCases/Anomalies/")

ndvi_1km_rstr_clean <- raster("ndvi_1km_rstr_clean.tif")

# Long Term statistics for the same dekad
ndvi_lts_1km_rstr_clean <- brick("ndvi_lts_1km_rstr_clean.tif")
names(ndvi_lts_1km_rstr_clean) <- c("mean", "sd")


# Calculating anomalies: difference with reference period (LTS)
t0 <- Sys.time()
ndvi_1km_anomalies <- ndvi_1km_rstr_clean - ndvi_lts_1km_rstr_clean$mean
Sys.time() - t0

ndvi_1km_anomalies
plot(ndvi_1km_anomalies)

writeRaster(ndvi_1km_anomalies, "ndvi_1km_anomalies.tif", overwrite = TRUE)


# Plotting a map
jpeg("ndvi_1km_anomalies.jpg", width = 22, height = 16.5, units = "cm", res = 300)
par(mar = c(6, 3, 1, 2), bty = "n")
cuts <- c(minValue(ndvi_1km_anomalies), -anom2, -anom1, anom1, anom2, maxValue(ndvi_1km_anomalies))
pal <- colorRampPalette(c("springgreen4", "springgreen2", "khaki2", "orange2", "red3"))
par(xpd = FALSE)
plot(ndvi_1km_anomalies, breaks = cuts, col = pal(5), legend = FALSE)
par(xpd = TRUE)
legend("bottom",
       legend = c(paste0("High positive anomaly (> ", anom2, ")"),
                  paste0("Low positive anomaly (> ", anom1, ")"),
                  "No anomaly",
                  paste0("Low negative anomaly (< -", anom1, ")"),
                  paste0("High negative anomaly (< -", anom2, ")")),
       fill = pal(5),
       cex = 1,
       inset = 0.01)
title(main = "NDVI ANOMALIES",
      outer = TRUE,
      line = - 3.5,
      cex.main = 2.5)

mtext("Current layer: 'ndvi_v2_1km_c_gls_NDVI_201908010000_GLOBE_PROBAV_V2.2.1.nc'", 
      side = 1, line = 3, 
      #at = 5,
      adj = 0,
      cex = 1)
mtext("Reference layer: 'ndvi-lts_v2_1km_c_gls_NDVI-LTS_1999-2017-0801_GLOBE_VGT-PROBAV_V2.2.1.nc'", 
      side = 1, line = 4, 
      #at = 5,
      adj = 0,
      cex = 1)

dev.off()


