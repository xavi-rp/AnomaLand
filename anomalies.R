
## setting working directory

if(Sys.info()[4] == "MacBook-MacBook-Pro-de-Xavier.local"){
   dirctry <- "/Users/xavi_rp/Documents/D6_UseCases/Anomalies/Anomalies_Kenya/"
}else if (Sys.info()[4] == "L2100346RI"){   #new laptop
   dirctry <- "C:/Users/rotllxa/D6_UseCases/Anomalies/Anomalies_Kenya/"
}else{
   dirctry <- "E:/rotllxa/D6_UseCases/Anomalies/Anomalies_Kenya/"   # PC JRC
   dirctry <- "E:/rotllxa/D6_UseCases/Anomalies/Anomalies_Kenya/OND2020"   # PC JRC
   dirctry <- "E:/rotllxa/D6_UseCases/Anomalies/Anomalies_Kenya/december2020"   # PC JRC
   dirctry <- "E:/rotllxa/D6_UseCases/Anomalies/Anomalies_Kenya/dekad20201221"   # PC JRC
   dirctry <- "E:/rotllxa/D6_UseCases/Anomalies/Anomalies_Kenya/march2021"   # PC JRC
   dirctry <- "E:/rotllxa/D6_UseCases/Anomalies/Anomalies_Kenya/april2021"   # PC JRC
   dirctry <- "E:/rotllxa/D6_UseCases/Anomalies/Anomalies_Kenya/dekad20210301"   # PC JRC
   dirctry <- "E:/rotllxa/D6_UseCases/Anomalies/Anomalies_Kenya/dekad20210321"   # PC JRC
   dirctry <- "E:/rotllxa/D6_UseCases/Anomalies/Anomalies_Kenya/dekad20210421"   # PC JRC
   
}
if(!dir.exists(dirctry)) dir.create(dirctry)
setwd(dirctry)



## Setting parameters and loading data

# Use 'download_data.R' to get the data sets and clean them
library(raster)
library(rworldmap)



# Anomalies method:
anom_method <- "simple"
anom_method <- "zscore"


# Thresholds for anomalies classification:
anom1 <- 0.05
anom2 <- 0.125

anom1 <- 0.105
anom2 <- 0.225

anom1 <- "1*SD"   # in the form value*SD
anom2 <- "2*SD"   # in the form value*SD



# Select country
cntry <- NULL
cntry <- "Europe"
cntry <- "Italy"
cntry <- "France"
cntry <- c("Italy", "France")
cntry <- "Asia"
cntry <- "Kenya"
cntry <- NULL

# World map
wrld_map <- getMap()
head(wrld_map)
#unique(wrld_map$continent)
unique(wrld_map$REGION)
unique(wrld_map$NAME)

selectedMap <- wrld_map[wrld_map$REGION %in% cntry | wrld_map$NAME %in% cntry, ]
sort(unique(selectedMap$NAME))

extent(selectedMap)
extent(wrld_map)


## Actual NDVI product: dekad 08/01/2019 (month/day/year)

#ndvi_1km_rstr_clean <- raster("ndvi_1km_rstr_clean.tif")
#ndvi_1km_rstr_clean <- raster("ndvi_1km_avrg_March2021.tif")
#ndvi_1km_rstr_clean <- raster("ndvi_1km_avrg_december2020.tif")
#ndvi_1km_rstr_clean <- raster("ndvi_1km_rstr_clean_1221.tif")
#ndvi_1km_rstr_clean <- raster("ndvi_1km_avrg_april2021.tif")
#ndvi_1km_rstr_clean <- raster("ndvi_1km_avrg_OND2020.tif")
#ndvi_1km_rstr_clean <- raster("../april2021/ndvi_1km_rstr_clean_0421.tif")
ndvi_1km_rstr_clean <- raster("../march2021/ndvi_1km_rstr_clean_0321.tif")


if(!is.null(cntry)){
  ndvi_1km_rstr_clean <- crop(ndvi_1km_rstr_clean, extent(selectedMap))
  ndvi_1km_rstr_clean <- mask(ndvi_1km_rstr_clean, selectedMap)
  #plot(ndvi_1km_rstr_clean)
}



## Long Term statistics for the same dekad

#ndvi_lts_1km_rstr_clean <- brick("ndvi_lts_1km_rstr_clean.tif")
#names(ndvi_lts_1km_rstr_clean) <- c("mean", "sd")
#ndvi_lts_1km_rstr_clean <- brick("ndvi_lts_1km_mean_avrg_March2021.tif")
#ndvi_lts_1km_rstr_clean <- brick("ndvi_lts_1km_mean_avrg_december2020.tif")
#ndvi_lts_1km_rstr_clean <- brick("ndvi_lts_1km_rstr_clean_1221.tif")[[1]]
#ndvi_lts_1km_rstr_clean <- brick("ndvi_lts_1km_mean_avrg_april2021.tif")
#ndvi_lts_1km_rstr_clean <- brick("ndvi_lts_1km_mean_avrg_OND2020.tif")
ndvi_lts_1km_rstr_clean <- brick("../march2021/ndvi_lts_1km_rstr_clean_0321.tif")[[1]]
names(ndvi_lts_1km_rstr_clean) <- c("mean")

if(!is.null(cntry)){
  ndvi_lts_1km_rstr_clean <- crop(ndvi_lts_1km_rstr_clean, extent(selectedMap))
  ndvi_lts_1km_rstr_clean <- mask(ndvi_lts_1km_rstr_clean, selectedMap)
 # plot(ndvi_lts_1km_rstr_clean)
}


#ndvi_lts_1km_sd_rstr_clean <- brick("ndvi_lts_1km_sd_avrg_March2021.tif")
#ndvi_lts_1km_sd_rstr_clean <- brick("ndvi_lts_1km_sd_avrg_december2020.tif")
#ndvi_lts_1km_sd_rstr_clean <- brick("ndvi_lts_1km_rstr_clean_1221.tif")[[2]]
#ndvi_lts_1km_sd_rstr_clean <- brick("ndvi_lts_1km_sd_avrg_april2021.tif")
#ndvi_lts_1km_sd_rstr_clean <- brick("ndvi_lts_1km_sd_avrg_OND2020.tif")
ndvi_lts_1km_sd_rstr_clean <- brick("../march2021/ndvi_lts_1km_rstr_clean_0321.tif")[[2]]
names(ndvi_lts_1km_sd_rstr_clean) <- c("sd")
if(!is.null(cntry)){
  ndvi_lts_1km_sd_rstr_clean <- crop(ndvi_lts_1km_sd_rstr_clean, extent(selectedMap))
  ndvi_lts_1km_sd_rstr_clean <- mask(ndvi_lts_1km_sd_rstr_clean, selectedMap)
}




## Calculating anomalies: difference with reference period (LTS)

if (anom_method == "simple"){
  ## Absolute differences with respect to the corresponding long-term average (Meroni, et al., 2014; https://doi.org/10.1080/01431161.2014.883090)
  ndvi_1km_anomalies <- ndvi_1km_rstr_clean - ndvi_lts_1km_rstr_clean$mean
  
}else if (anom_method == "zscore"){
  ## z-score:
  # see also Meroni et al. (2019; https://doi.org/10.1016/j.rse.2018.11.041)
  # Z = (NDVI ??? NDVImean)/NDVIsd
  ndvi_1km_anomalies <- (ndvi_1km_rstr_clean - ndvi_lts_1km_rstr_clean$mean) / ndvi_lts_1km_sd_rstr_clean$sd
  
}else{
  stop("define 'anom_method' as 'simple' or 'zscore")
}


ndvi_1km_anomalies
plot(ndvi_1km_anomalies)

writeRaster(ndvi_1km_anomalies, "ndvi_1km_anomalies.tif", overwrite = TRUE)

ndvi_1km_anomalies <- raster("ndvi_1km_anomalies.tif")
# Coordinate for subsetting
coords4subset <- c(33, 43, -6, 6)  # Kenya
coords4subset <- c(21, 53, -6.5, 22)  # Horn of Africa

ndvi_1km_anomalies <- crop(ndvi_1km_anomalies, extent(coords4subset))  # Horn
ndvi_1km_anomalies <- crop(ndvi_1km_anomalies, extent(c(32, 40, 1, 8)))  # Turkana  


if(is.null(cntry)){
  name1 <- "HornAfrica"
  name1 <- "TurkanaLake"
}else{
  name1 <- cntry
}

## Applying thresholds

if(is.numeric(anom1)){
  name2 <- ""
  
  cuts <- c(minValue(ndvi_1km_anomalies), -anom2, -anom1, anom1, anom2, maxValue(ndvi_1km_anomalies))
  reclass_mtrx <- as.data.frame(cuts)
  names(reclass_mtrx) <- "from"
  reclass_mtrx$to <- c(reclass_mtrx$from[-1], 1)
  reclass_mtrx <- reclass_mtrx[1:(nrow(reclass_mtrx) - 1), ]
  reclass_mtrx$becomes <- 1:5
  
  ndvi_1km_anomalies1 <- reclassify(ndvi_1km_anomalies, rcl = reclass_mtrx, filename = '', include.lowest = TRUE, right = FALSE)
  
}else{
  name2 <- "_sd"
  ndvi_1km_anomalies1 <- ndvi_1km_anomalies
  
  anom_val2 <- as.numeric(sub("\\*SD$", "", anom2))
  if(anom1 == "SD"){
    anom_val1 <- 1
  }else{
    anom_val1 <- as.numeric(sub("\\*SD$", "", anom1))
  }

  if (anom_method == "simple"){
    ndvi_1km_anomalies1[ndvi_1km_anomalies < (-anom_val2 * ndvi_lts_1km_sd_rstr_clean)]   <- 1 
    ndvi_1km_anomalies1[ndvi_1km_anomalies >= (-anom_val2 * ndvi_lts_1km_sd_rstr_clean)
                        & ndvi_1km_anomalies < (-anom_val1 * ndvi_lts_1km_sd_rstr_clean)] <- 2 
    ndvi_1km_anomalies1[ndvi_1km_anomalies >= (-anom_val1 * ndvi_lts_1km_sd_rstr_clean)
                        & ndvi_1km_anomalies < (anom_val1 * ndvi_lts_1km_sd_rstr_clean)]  <- 3 
    ndvi_1km_anomalies1[ndvi_1km_anomalies >= (anom_val1 * ndvi_lts_1km_sd_rstr_clean)
                        & ndvi_1km_anomalies < (anom_val2 * ndvi_lts_1km_sd_rstr_clean)]  <- 4 
    ndvi_1km_anomalies1[ndvi_1km_anomalies >= (anom_val2 * ndvi_lts_1km_sd_rstr_clean)]   <- 5
  
  }else if (anom_method == "zscore"){
    ndvi_1km_anomalies1[ndvi_1km_anomalies < -anom_val2]   <- 1 
    ndvi_1km_anomalies1[ndvi_1km_anomalies >= -anom_val2
                        & ndvi_1km_anomalies < -anom_val1] <- 2 
    ndvi_1km_anomalies1[ndvi_1km_anomalies >= -anom_val1 
                        & ndvi_1km_anomalies < anom_val1]  <- 3 
    ndvi_1km_anomalies1[ndvi_1km_anomalies >= anom_val1
                        & ndvi_1km_anomalies < anom_val2]  <- 4 
    ndvi_1km_anomalies1[ndvi_1km_anomalies >= anom_val2]   <- 5
  }
}  


# Plotting a map
#name1 <- "HornAfrica"
#jpeg(paste0("ndvi_1km_dekad20201221_", name1, "_anomalies_", anom_method, name2, ".jpg"), width = 22, height = 16.5, units = "cm", res = 300)
#jpeg(paste0("ndvi_1km_March2021_", name1, "_anomalies_", anom_method, name2, ".jpg"), width = 22, height = 16.5, units = "cm", res = 300)
#jpeg(paste0("ndvi_1km_december2020_", name1, "_anomalies_", anom_method, name2, ".jpg"), width = 22, height = 16.5, units = "cm", res = 300)
#jpeg(paste0("ndvi_1km_April2021_", name1, "_anomalies_", anom_method, name2, ".jpg"), width = 22, height = 16.5, units = "cm", res = 300)
jpeg(paste0("ndvi_1km_dekad20210321_", name1, "_anomalies_", anom_method, name2, ".jpg"), width = 22, height = 16.5, units = "cm", res = 300)
#par(mar = c(6, 3, 1, 2), bty = "n")
par(mar = c(6, 3, 4, 8), bty = "n")
#cuts <- c(minValue(ndvi_1km_anomalies), -anom2, -anom1, anom1, anom2, maxValue(ndvi_1km_anomalies))
#pal <- colorRampPalette(c("springgreen4", "springgreen2", "khaki2", "orange2", "red3"))
pal <- colorRampPalette(c("red3", "orange2" , "khaki2", "springgreen2", "springgreen4"))
par(xpd = FALSE)
plot(ndvi_1km_anomalies1, #breaks = cuts, 
     col = pal(5), legend = FALSE)
plot(wrld_map, add = TRUE, border = "grey47")
par(xpd = TRUE)
legend(#"bottom",
       "right",
       title = paste0("Method: ", anom_method),
       #legend = c(paste0("High positive anomaly (> ", anom2, ")"),
       #           paste0("Low positive anomaly (> ", anom1, ")"),
       #           "No anomaly",
       #           paste0("Low negative anomaly (< -", anom1, ")"),
       #           paste0("High negative anomaly (< -", anom2, ")")),
       legend = c(paste0("High negative anomaly (< -", anom2, ")"),
                  paste0("Low negative anomaly (< -", anom1, ")"),
                  "No anomaly",
                  paste0("Low positive anomaly (> ", anom1, ")"),
                  paste0("High positive anomaly (> ", anom2, ")")),
       fill = pal(5),
       cex = 1,
       #inset = 0.01)
       inset = - 0.3)
if(name1 == "HornAfrica") name1 <- "Horn of Africa"
if(name1 == "TurkanaLake") name1 <- "Lake Turkana Area"
#title(main = paste0(name1, ": NDVI ANOMALIES\nOctober-November-December 2020"),
#title(main = paste0(name1, ": NDVI ANOMALIES\nDecember 2020"),
#title(main = paste0(name1, ": NDVI ANOMALIES\nDecember 2020"),
#title(main = paste0(name1, ": NDVI ANOMALIES\nDekad 20201221"),
title(main = paste0(name1, ": NDVI ANOMALIES\nDekad 20210321"),
#title(main = paste0(name1, ": NDVI ANOMALIES\nApril 2021"),
#title(main = paste0(name1, ": NDVI ANOMALIES\nMarch 2021"),
      outer = TRUE,
      line = - 3.5,
      #cex.main = 2.5)
      cex.main = 1.5)

mtext(#"Current layer: 'ndvi_v2_1km_c_gls_NDVI_201908010000_GLOBE_PROBAV_V2.2.1.nc'", 
      #"Current layer: Average April 2021. 'c_gls_NDVI300_202104000000_GLOBE_OLCI_V2.0.1.nc'",
      "Current layer: Dekad 21 March 2021. 'c_gls_NDVI300_202103210000_GLOBE_OLCI_V2.0.1.nc'",
      side = 1, line = 3, 
      #at = 5,
      adj = 0,
      #cex = 1)
      cex = 0.8)
mtext(#"Reference layer: 'ndvi-lts_v2_1km_c_gls_NDVI-LTS_1999-2017-0801_GLOBE_VGT-PROBAV_V2.2.1.nc'", 
      #"Reference layer: Average April LTS (1999-2019) 'c_gls_NDVI-LTS_1999-2019-0400_GLOBE_VGT-PROBAV_V3.0.1.nc'",
      "Reference layer: Dekad 21 March LTS (1999-2019) 'c_gls_NDVI-LTS_1999-2019-0321_GLOBE_VGT-PROBAV_V3.0.1.nc'",
      side = 1, line = 4, 
      #at = 5,
      adj = 0,
      #cex = 1)
      cex = 0.8)

dev.off()


