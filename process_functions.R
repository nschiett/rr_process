library(habtools)
library(sp)
library(sf)
library(raster)
library(dplyr)
library(stringr)

## functions

reproject_dem <- function(file, lat_stake, lon_stake) {
  
  # name
  name <- strsplit(file, split = "\\.")[[1]][1]
  # load
  dem <- raster(paste0("data/rasters/", file))
  
  # New projection using the stake as center
  sr <- paste0("+proj=tmerc +lat_0=", lat_stake, " +lon_0=", lon_stake, 
               " +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
  
  dem2 <- projectRaster(dem, crs = sr)
  dem3 <- dem_crop(dem2, 0, 0, L = 8)
  
  writeRaster(dem3, paste0("output/rasters/", name, "_reproj_square.tif"), overwrite = TRUE)
  
}


reproject_ortho <- function(file, lat_stake, lon_stake) {
  
  # name
  name <- strsplit(file, split = "\\.")[[1]][1]
  # load
  ortho <- stack(paste0("data/rasters/", file))
  
  # New projection using the stake as center
  sr <- paste0("+proj=tmerc +lat_0=", lat_stake, " +lon_0=", lon_stake, 
               " +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
  
  ortho2 <- projectRaster(ortho, crs = sr)
  rm(ortho)
  
  ortho <- dem_crop(ortho2, 0, 0, L = 8)
  
  writeRaster(ortho, paste0("output/rasters/", name, "_reproj_square.tif"),
              format = "GTiff",  options="INTERLEAVE=BAND", overwrite = TRUE)
  
  # split in 2x2m squares
  ortho_list <- dem_split(ortho, 2, parallel = TRUE, ncores = 4)
  
  # get center of 2x2m squares
  mids <- lapply(ortho_list, mid_find) %>% dplyr::bind_rows()
  
  # make plots in pdf
  
  pdf(paste0("output/pdf/", name, "_squares_2m.pdf"),    # File name
      width = 8, height = 12, # Width and height in inches
      bg = "white",          # Background color
      colormodel = "cmyk",    
      paper = "A4")  
  
  par(mfrow = c(2, 1), mar=rep(1,4),oma=rep(1,4), xaxt="n", yaxt="n")

  lapply(c(1,3, 6, 8, 9, 11, 14, 16), function(i){
    plotRGB(ortho_list[[i]], alpha = 230, axes = TRUE)
    title(paste(i))
  })
  dev.off() 
  
  # bigsquare
  
  pdf(paste0("output/pdf/", name, "_square_8m.pdf"),    # File name
      width = 8, height = 12, # Width and height in inches
      bg = "white",          # Background color
      colormodel = "cmyk",    
      paper = "A4")  
  
  par(mfrow = c(1, 1), mar=rep(2,4),oma=rep(1,4), xaxt="n", yaxt="n")
  
  plotRGB(ortho, alpha = 220, axes = TRUE, maxpixels = 1000000)
  title(name)
  
  lapply(c(1,3, 6, 8, 9, 11, 14, 16), function(i){
    
    rect(xleft = mids$x_mid[i] - 1, xright = mids$x_mid[i] + 1, 
         mids$y_mid[i] - 1, mids$y_mid[i] + 1, border = "white")
    text(x =  mids$x_mid[i], y =  mids$y_mid[i], labels = paste(i))
  })
  
  dev.off() 
  
}


#### apply to all ###
files <- list.files("data/rasters")
files_dem <- str_subset(files, "DEM")
files_ortho <- str_subset(files, "ortho")

stakes <- read.csv("data/rr_kbay_stakes_coord.csv")

### dems
for (i in 1:length(files_dem)) {
  print(i)
  f <- files_dem[i]
  site <- strsplit(f, "_")[[1]][3]
  print(site)
  lat <- stakes[stakes$site == site, "latitude"]
  long <- stakes[stakes$site == site, "longitude"]
  reproject_dem(f, lat, long)
}

### ortho
for (i in 1:length(files_ortho)) {
  print(i)
  f <- files_ortho[i]
  site <- strsplit(f, "_")[[1]][3]
  print(site)
  lat <- stakes[stakes$site == site, "latitude"]
  long <- stakes[stakes$site == site, "longitude"]
  reproject_ortho(f, lat, long)
}






