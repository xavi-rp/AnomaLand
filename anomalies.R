
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
library(data.table)


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
#plot(ndvi_1km_anomalies)

writeRaster(ndvi_1km_anomalies, "ndvi_1km_anomalies.tif", overwrite = TRUE)
#ndvi_1km_anomalies <- raster("ndvi_1km_anomalies.tif")

# Plotting a map
#jpeg("ndvi_1km_anomalies_Fr_It1.jpg", width = 22, height = 16.5, units = "cm", res = 300)
jpeg("ndvi_1km_anomalies_Fr1.jpg", width = 22, height = 16.5, units = "cm", res = 300)
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



## Statistics by land cover class
current_year <- 2019

lc_map_1km <- raster(paste0("lc_map_", current_year, "_1km.tif"))

if(!is.null(cntry)){ lc_map_1km <- crop(lc_map_1km, ndvi_1km_anomalies) }


# 0         <- NA
# 100       <- Moss and lichen
# 111 - 116 <- Closed forests
# 121 - 126 <- Open forests
# 20        <- Shrubs
# 30        <- Herbaceous vegetation
# 90        <- Herbaceous wetland
# 60        <- Bare / spare vegetation
# 40        <- Cropland
# 50        <- Urban / Built up
# 70        <- Snow / ice
# 80        <- Permanent water bodies
# 200       <- Open sea


#lc_map_1km_vals <- getValues(lc_map_1km)
#table(lc_map_1km_vals)
#sum(is.na(lc_map_1km_vals))              #     56066
#sum(lc_map_1km_vals == 0, na.rm = TRUE)  #   1717987      
#sum(lc_map_1km_vals == 200, na.rm =TRUE) # 441255181



ndvi_1km_anomalies_LC <- brick(ndvi_1km_anomalies, lc_map_1km)
ndvi_1km_anomalies_LC_dt <- as.data.frame(ndvi_1km_anomalies_LC)
ndvi_1km_anomalies_LC_dt <- data.table(ndvi_1km_anomalies_LC_dt, keep.rownames = FALSE)
ndvi_1km_anomalies_LC_dt[, rwnms := c(1:nrow(ndvi_1km_anomalies_LC_dt))]
ndvi_1km_anomalies_LC_dt


# removing pixels with NA
ndvi_1km_anomalies_LC_dt <- ndvi_1km_anomalies_LC_dt[complete.cases(ndvi_1km_anomalies_LC_dt), ]
ndvi_1km_anomalies_LC_dt


# reclassifying anomalies

if ("layer" %in% colnames(ndvi_1km_anomalies_LC_dt)) setnames(ndvi_1km_anomalies_LC_dt, "layer", "ndvi_1km_anomalies")

ndvi_1km_anomalies_LC_dt[, anomalies := "No_Anomalies"][ndvi_1km_anomalies >= anom2, anomalies := "High_Pos"]
ndvi_1km_anomalies_LC_dt[ndvi_1km_anomalies >= anom1 & ndvi_1km_anomalies < anom2, anomalies := "Low_Pos"]
ndvi_1km_anomalies_LC_dt[ndvi_1km_anomalies <= - anom2, anomalies := "High_Neg"]
ndvi_1km_anomalies_LC_dt[ndvi_1km_anomalies <= - anom1 & ndvi_1km_anomalies > - anom2, anomalies := "Low_Neg"]

ndvi_1km_anomalies_LC_dt

# Proportion of each anomalies group (over total)
(table(ndvi_1km_anomalies_LC_dt$anomalies) * 100) / nrow(ndvi_1km_anomalies_LC_dt)


# Proportion of each anomalies group by Land Cover Class
#anomalies_LC <- ndvi_1km_anomalies_LC_dt[, .(count = .N), by = .(lc_map_2019_1km, anomalies)]

anomalies_LC <- ndvi_1km_anomalies_LC_dt[,{totwt = .N 
                                           .SD[, .(prop = (.N / totwt) * 100), 
                                               by = anomalies]}, by = lc_map_2019_1km]
setkeyv(anomalies_LC, "lc_map_2019_1km")                                           
anomalies_LC

anomalies_LC_wide <- dcast(anomalies_LC, anomalies ~ lc_map_2019_1km, value.var = "prop")
srt <- c("High_Pos", "Low_Pos", "No_Anomalies", "Low_Neg", "High_Neg")
anomalies_LC_wide <- anomalies_LC_wide[order(match(anomalies, srt))]
if ("0" %in% colnames(anomalies_LC_wide)) anomalies_LC_wide[, "0" := NULL]

setcolorder(anomalies_LC_wide, neworder = c(colnames(anomalies_LC_wide)[1], sort(colnames(anomalies_LC_wide)[-1]))) 

anomalies_LC_wide[is.na(anomalies_LC_wide)] <- 0
anomalies_LC_wide

#apply(anomalies_LC_wide[, -1], 2, sum) 

#colnames(anomalies_LC_wide) <- c("NA",
#                                 "Moss and lichen",
#                                 "Closed forests",
#                                 "Open forests",
#                                 "Shrubs",
#                                 "Herbaceous vegetation",
#                                 "Herbaceous wetland",
#                                 "Bare / spare vegetation",
#                                 "Cropland",
#                                 "Urban / Built up",
#                                 "Snow / ice",
#                                 "Permanent water bodies",
#                                 "Open sea")

# plotting barplot
#jpeg("ndvi_anomalies_LC_Fr.jpg", width = 32, height = 16.5, units = "cm", res = 300)
jpeg("ndvi_anomalies_LC_Fr1.jpg", width = 32, height = 16.5, units = "cm", res = 300)
par(mar = c(13, 6, 3, 16.5), bty = "n")
par(xpd = TRUE)
barplot(as.matrix(anomalies_LC_wide[, - 1]),
        ylab = "Percentage (%)",
        col = pal(5),
        #legend = c(paste0("High positive anomaly (> ", anom2, ")"),
        #           paste0("Low positive anomaly (> ", anom1, ")"),
        #           "No anomaly",
        #           paste0("Low negative anomaly (< -", anom1, ")"),
        #           paste0("High negative anomaly (< -", anom2, ")")), 
        beside = TRUE,
        las = 2,
        horiz = FALSE
        #space = c(0, 1),
        #width = rep(30, ncol(anomalies_LC_wide[, - 1])),
        #xpd = TRUE
        )

legend("right",
       title = "Anomalies",
       legend = c(paste0("High positive anomaly (> ", anom2, ")"),
                  paste0("Low positive anomaly (> ", anom1, ")"),
                  "No anomaly",
                  paste0("Low negative anomaly (< -", anom1, ")"),
                  paste0("High negative anomaly (< -", anom2, ")")),
       fill = pal(5),
       inset = - 0.35
       )

legend("bottom",
       title = "Land Cover Classes",
       ncol = 2,
       legend = c(#"0               <- NA",
                  "100 <- Moss and lichen",
                  "111-116 <- Closed forests",
                  "121-126 <- Open forests",
                  "20 <- Shrubs",
                  "200 <- Open sea",
                  "30 <- Herbaceous vegetation",
                  "40 <- Cropland",
                  "50 <- Urban / Built up",
                  "60 <- Bare / spare vegetation",
                  "70 <- Snow / ice",
                  "80 <- Permanent water bodies",
                  "90 <- Herbaceous wetland"),
       #fill = pal(5),
       inset = - 0.7
       )

title(main = "Percentages of Anomalies by Land Cover Class",
      #outer = TRUE,
      #line = - 3.5,
      cex.main = 1.5)

dev.off()




