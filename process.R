library(habtools)
library(sp)
library(sf)
library(raster)
library(dplyr)

######## 2B #########
# center
lat_stake <- 21.459503292
lon_stake <- -157.797440841

## DEM 
dem <- raster("data/rr_2023_2b_DEM.tif")
plot(dem)

# New projection using the stake as center
sr <- paste0("+proj=tmerc +lat_0=", lat_stake, " +lon_0=", lon_stake, 
             " +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
dem2 <- projectRaster(dem, crs = sr)
plot(dem2)
points(0, 0)

dem3 <- dem_crop(dem2, 0, 0, L = 8)
plot(dem3)
points(0,0)
res(dem3)

writeRaster(dem3, "output/rr_2023_2b_DEM_reproj.tif")

rm(dem, dem2)

# split in 2x2m squares
dem_list2 <- dem_split(dem3, 2, parallel = TRUE, ncores = 4)
# get center of 2x2m squares
squares <- lapply(dem_list2, mid_find) %>% dplyr::bind_rows() %>%
  mutate(id = 1:16) %>%
  mutate(bl_x = x_mid - 0.75, bl_y = y_mid - 0.75,
         br_x = x_mid + 0.75, br_y = y_mid - 0.75,
         tl_x = x_mid - 0.75, tl_y = y_mid + 0.75,
         tr_x = x_mid + 0.75, tr_y = y_mid + 0.75)

plot(dem3)
lapply(1:16, function(i){
  rect(xleft = squares$x_mid[i] - 0.75, xright = squares$x_mid[i] + 0.75, 
       squares$y_mid[i] - 0.75, squares$y_mid[i] + 0.75, border = "white")
  text(x =  squares$x_mid[i], y =  squares$y_mid[i], labels = squares$id[i])
})

# get complexity metrics
dem_list <- dem_crop(dem3, x0 = squares$x_mid, y0 = squares$y_mid, L = 1.5)
r <- sapply(dem_list, rg)
d <- sapply(dem_list, fd, method = "sd", lvec = 1.5/c(1,2,4,8,16), parallel = T)
h <- sapply(dem_list, hr)

squares <- squares %>%
  mutate(r = r, d = d, h = h)

readr::write_csv(squares, "output/Site2B-squares.csv")

## Reference points transform
ref <- readr::read_csv("data/Site2B-refs.csv")
ref_sf <- st_as_sf(x = ref, 
                        coords = c("x", "y"),
                        crs = crs(dem))
ref_trans <- st_transform(ref_sf, sr)
ref_new <- st_coordinates(ref_trans) %>%
  as.data.frame() %>%
  rename(x_trans = X, y_trans = Y) %>%
  cbind(ref)

readr::write_csv(ref_new, "output/Site2B-refs-transformed.csv")

## ORTHO
ortho <- stack("data/rr_2023_2b_ortho.tif")
plotRGB(ortho, alpha = 210)
ortho2 <- projectRaster(ortho, crs = sr)
rm(ortho)

ortho3 <- dem_crop(ortho2, 0, 0, L = 8)

writeRaster(ortho3, "output/rr_2023_2b_ortho_reproj.tif")

ortho <- stack("output/rr_2023_2b_ortho_reproj.tif")


# split in 2x2m squares
ortho_list <- dem_split(ortho, 2, parallel = TRUE, ncores = 4)

# get center of 2x2m squares
mids <- lapply(ortho_list, mid_find) %>% dplyr::bind_rows()

# make plots in pdf

pdf(paste0("output/squares_2b.pdf"),    # File name
    width = 8, height = 12, # Width and height in inches
    bg = "white",          # Background color
    colormodel = "cmyk",    
    paper = "A4")  

par(mfrow= c(1,1)) 

lapply(1:16, function(i){
  plotRGB(ortho_list[[i]], alpha = 230)
  rect(xleft = mids$x_mid[i] - 0.75, xright = mids$x_mid[i] + 0.75, 
       mids$y_mid[i] - 0.75, mids$y_mid[i] + 0.75, border = "white")
  title(paste(i))
})

dev.off() 

# bigsquare
pdf(paste0("output/bigsquare_2b.pdf"),    # File name
    width = 8, height = 12, # Width and height in inches
    bg = "white",          # Background color
    colormodel = "cmyk",    
    paper = "A4")  

plotRGB(ortho, alpha = 210)

lapply(1:16, function(i){
  
  rect(xleft = mids$x_mid[i] - 0.75, xright = mids$x_mid[i] + 0.75, 
       mids$y_mid[i] - 0.75, mids$y_mid[i] + 0.75, border = "white")
  text(x =  mids$x_mid[i], y =  mids$y_mid[i], labels = paste(i))
})

dev.off() 








