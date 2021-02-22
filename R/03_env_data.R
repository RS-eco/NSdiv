library(terra); library(ggplot2); library(sf)
library(dplyr); library(dtplyr); library(magrittr)

files <- list.files("data", pattern=".tif", full.names=T)[1:3]

# Get AWZ outline
library(geodat)
data("eez")
awz <- eez %>% filter(Country == "Germany")
ggplot() + geom_sf(data=awz)
st_bbox(awz); rm(eez)

# Load data
dat <- terra::rast(files)

# Crop by awz extent
dat_awz <- terra::crop(dat, awz)
writeRaster(dat_awz, filename="data/marspec_awz.nc", compression=9, overwrite=T)
plot(dat_awz)
