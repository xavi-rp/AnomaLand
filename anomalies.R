# Use 'download_data.R' to get the data sets and clean them

# Thresholds for anomalies:
anom1 <- 0.05
anom2 <- 0.125

# Actual NDVI product: dekad 08/01/2019 (month/day/year)
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


# Plotting a map
pdf("ndvi_1km_anomalies.pdf")
par(xpd = TRUE)
cuts <- c(minValue(ndvi_1km_anomalies), -anom2, -anom1, anom1, anom2, maxValue(ndvi_1km_anomalies))
pal <- colorRampPalette(c("red3", "orange2", "khaki2", "springgreen2", "springgreen4"))
plot(ndvi_1km_anomalies, breaks = cuts, col = pal(5), legend = FALSE)
legend("bottom", 
       legend = c(paste0("High positive anomaly (> ", anom2, ")"),
                  paste0("Low positive anomaly (> ", anom1, ")"),
                  "No anomaly",
                  paste0("Low negative anomaly (< -", anom1, ")"),
                  paste0("High negative anomaly (< -", anom2, ")")),
       fill = pal(5),
       inset = 0.07)

dev.off()


