rm(list=ls()); gc()

# Load packages
library(raster); library(sf)
library(dplyr); library(dtplyr); library(magrittr); library(tidyr)
library(ggplot2); library(patchwork); library(scico)

# Get AWZ outline
library(geodat)
data("eez")
awz <- eez %>% filter(Country == "Germany"); rm(eez); gc()
ggplot() + geom_sf(data=awz)

#awz <- st_transform(awz, crs=5652)
#ggplot() + geom_sf(data=awz)

awz_split <- st_cast(awz, "POLYGON")
awz_split$id <- 1:4

awz_bs <- awz_split[1:3,]
awz_ns <- awz_split[4,]
ggplot() + geom_sf(data=awz_ns)
ggplot() + geom_sf(data=awz_bs)

# Load env data
env_dat <- raster::stack("data/marspec_awz.nc")
#env_dat <- projectRaster(env_dat, crs="+proj=tmerc +lat_0=0 +lon_0=9 +k=0.9996 +x_0=32500000 +y_0=0 +ellps=GRS80 +units=m +no_defs")

# Mask by AWZ outline
env_dat <- raster::mask(env_dat, awz)
plot(env_dat)

# Transform data into correct units
env_dat[[2]] <- env_dat[[2]]/100 
env_dat[[3]] <- env_dat[[3]]/100

env_dat_bs <- raster::mask(env_dat, as(awz_split[1:3,], "Spatial"))
# Turn into data.frame
names(env_dat_bs ) <- c("Tiefe", "SSS", "SST")
env_dat_bs  <- as.data.frame(x = env_dat_bs , xy=T) %>% tidyr::drop_na()

env_dat_ns <- raster::mask(env_dat, as(awz_split[4,], "Spatial"))
# Turn into data.frame
names(env_dat_ns ) <- c("Tiefe", "SSS", "SST")
env_dat_ns  <- as.data.frame(x = env_dat_ns , xy=T) %>% tidyr::drop_na()

# Turn into data.frame
names(env_dat) <- c("Tiefe", "SSS", "SST")
env_dat <- as.data.frame(x = env_dat, xy=T) %>% tidyr::drop_na()

# Plot env data
quantiles <- (0:8)/8                           # how many quantiles we want to map 
colours <- scico(8, palette = 'roma')    # 7 evenly interpolated colors 
quantile.vals <- round(quantile(env_dat$Tiefe, quantiles, names=F), 2) # the values for each quantile
val.remap <- (quantile.vals - min(env_dat$Tiefe)) / 
  diff(range(env_dat$Tiefe))                                # The values corresponding to the quantiles
p1 <- ggplot() + geom_tile(data=env_dat, aes(x=x, y=y, fill=Tiefe)) + 
  geom_sf(data=awz, fill=NA) + coord_sf() + 
  scale_fill_gradientn(
    colours=colours,
    values=val.remap,
    breaks=quantile.vals,# Necessary to get legend values spread appropriately
    guide = guide_legend(reverse = TRUE)) +       # Necessary to get legend values spread appropriately
  theme_bw() + theme(axis.text.x = element_blank(),
                     axis.title = element_blank(),
                     legend.key.height = unit(0.5, 'cm'))
quantile.vals <- round(quantile(env_dat$SSS, quantiles, names=F), 2)# the values for each quantile
val.remap <- (quantile.vals - min(env_dat$SSS)) / 
  diff(range(env_dat$SSS))  
p2 <- ggplot() + geom_tile(data=env_dat, aes(x=x, y=y, fill=SSS)) + 
  geom_sf(data=awz, fill=NA) + coord_sf() + 
  scale_fill_gradientn(
    colours=colours,
    values=val.remap,
    breaks=quantile.vals,# Necessary to get legend values spread appropriately
    guide = guide_legend(reverse = TRUE)) +       # Necessary to get legend values spread appropriately
  theme_bw() + theme(axis.text.x = element_blank(),
                     axis.title = element_blank(),
                     legend.key.height = unit(0.5, 'cm'))
quantile.vals <- round(quantile(env_dat$SST, quantiles, names=F), 2)# the values for each quantile
val.remap <- (quantile.vals - min(env_dat$SST)) / 
  diff(range(env_dat$SST)) 
p3 <- ggplot() + geom_tile(data=env_dat, aes(x=x, y=y, fill=SST)) + 
  geom_sf(data=awz, fill=NA) + coord_sf() + 
  scale_fill_gradientn(
    colours=rev(colours),
    values=val.remap,
    breaks=quantile.vals,# Necessary to get legend values spread appropriately
    guide = guide_legend(reverse = TRUE)) +       # Necessary to get legend values spread appropriately
  theme_bw() + theme(axis.title = element_blank(),
                     legend.key.height = unit(0.5, 'cm'))
p1 / p2 / p3
ggsave(filename="figures/env_dat.png", width=7, height=7, dpi=1000)

# Plot env data North Sea
quantiles <- (0:8)/8                           # how many quantiles we want to map 
colours <- scico(8, palette = 'roma')    # 7 evenly interpolated colors 
quantile.vals <- quantile(env_dat_ns$Tiefe, quantiles, names=F) # the values for each quantile
val.remap <- (quantile.vals - min(env_dat_ns$Tiefe)) / diff(range(env_dat_ns$Tiefe))  # The values corresponding to the quantiles
#val.remap2 <- scales::rescale(quantile.vals)
p1 <- ggplot() + geom_tile(data=env_dat_ns, aes(x=x, y=y, fill=Tiefe)) + 
  geom_sf(data=awz_ns, fill=NA) + coord_sf() + 
  scale_fill_gradientn(colours=colours,
                       values=val.remap,
                       breaks=quantile.vals,# Necessary to get legend values spread appropriately
                       guide = guide_legend(reverse = TRUE)
  ) +       # Necessary to get legend values spread appropriately
  theme_bw() + theme(axis.title = element_blank(), legend.position=c(0.11,0.365),
                     legend.key.size = unit(0.3, 'cm'), #change legend key size
                     legend.key.height = unit(0.3, 'cm'), #change legend key height
                     legend.key.width = unit(0.3, 'cm'), #change legend key width
                     legend.title = element_text(size=12), #change legend title font size
                     legend.text = element_text(size=8),
                     legend.background = element_blank())
quantile.vals <- round(quantile(env_dat_ns$SSS, quantiles, names=F), 2)# the values for each quantile
val.remap <- (quantile.vals - min(env_dat_ns$SSS)) / 
  diff(range(env_dat_ns$SSS))  
p2 <- ggplot() + geom_tile(data=env_dat_ns, aes(x=x, y=y, fill=SSS)) + 
  geom_sf(data=awz_ns, fill=NA) + coord_sf() + 
  scale_fill_gradientn(
    colours=colours,
    values=val.remap,
    breaks=quantile.vals,# Necessary to get legend values spread appropriately
    guide = guide_legend(reverse = TRUE)) +       # Necessary to get legend values spread appropriately
  theme_bw() + theme(axis.text.y = element_blank(),
                     axis.title = element_blank(), legend.position=c(0.11,0.365),
                     legend.key.size = unit(0.3, 'cm'), #change legend key size
                     legend.key.height = unit(0.3, 'cm'), #change legend key height
                     legend.key.width = unit(0.3, 'cm'), #change legend key width
                     legend.title = element_text(size=12), #change legend title font size
                     legend.text = element_text(size=8),
                     legend.background = element_blank())
quantile.vals <- round(quantile(env_dat_ns$SST, quantiles, names=F), 2)# the values for each quantile
val.remap <- (quantile.vals - min(env_dat_ns$SST)) / 
  diff(range(env_dat_ns$SST)) 
p3 <- ggplot() + geom_tile(data=env_dat_ns, aes(x=x, y=y, fill=SST)) + 
  geom_sf(data=awz_ns, fill=NA) + coord_sf() + 
  scale_fill_gradientn(
    colours=rev(colours),
    values=val.remap,
    breaks=quantile.vals,# Necessary to get legend values spread appropriately
    guide = guide_legend(reverse = TRUE)) +       # Necessary to get legend values spread appropriately
  theme_bw() + theme(axis.text.y = element_blank(),
                     axis.title = element_blank(), legend.position=c(0.11,0.365),
                     legend.key.size = unit(0.3, 'cm'), #change legend key size
                     legend.key.height = unit(0.3, 'cm'), #change legend key height
                     legend.key.width = unit(0.3, 'cm'), #change legend key width
                     legend.title = element_text(size=12), #change legend title font size
                     legend.text = element_text(size=8),
                     legend.background = element_blank())
p1 + p2 + p3
ggsave(filename="figures/env_dat_ns.png", width=12, height=3, dpi=1000)

# Plot env data Baltic Sea
quantiles <- (0:8)/8                           # how many quantiles we want to map 
colours <- scico(8, palette = 'roma')    # 7 evenly interpolated colors 
quantile.vals <- quantile(env_dat_bs$Tiefe, quantiles, names=F)# the values for each quantile
val.remap <- (quantile.vals - min(env_dat_bs$Tiefe)) / 
  diff(range(env_dat_bs$Tiefe))                                # The values corresponding to the quantiles
p1 <- ggplot() + geom_tile(data=env_dat_bs, aes(x=x, y=y, fill=Tiefe)) + 
  geom_sf(data=awz_bs, fill=NA) + coord_sf() + 
  scale_fill_gradientn(
    colours=colours,
    values=val.remap,
    breaks=quantile.vals,# Necessary to get legend values spread appropriately
    guide = guide_legend(reverse = TRUE)) +       # Necessary to get legend values spread appropriately
  theme_bw() + theme(axis.text.x = element_blank(),
                     axis.title = element_blank(),
                     legend.key.height = unit(0.5, 'cm')) #change legend key height
quantile.vals <- round(quantile(env_dat_bs$SSS, quantiles, names=F), 2)# the values for each quantile
val.remap <- (quantile.vals - min(env_dat_bs$SSS)) / 
  diff(range(env_dat_bs$SSS))  
p2 <- ggplot() + geom_tile(data=env_dat_bs, aes(x=x, y=y, fill=SSS)) + 
  geom_sf(data=awz_bs, fill=NA) + coord_sf() + 
  scale_fill_gradientn(
    colours=colours,
    values=val.remap,
    breaks=quantile.vals,# Necessary to get legend values spread appropriately
    guide = guide_legend(reverse = TRUE)) +       # Necessary to get legend values spread appropriately
  theme_bw() + theme(axis.text.x = element_blank(),
                     axis.title = element_blank(),
                     legend.key.height = unit(0.5, 'cm')) #change legend key height
quantile.vals <- round(quantile(env_dat_bs$SST, quantiles, names=F), 2)# the values for each quantile
val.remap <- (quantile.vals - min(env_dat_bs$SST)) / 
  diff(range(env_dat_bs$SST)) 
p3 <- ggplot() + geom_tile(data=env_dat_bs, aes(x=x, y=y, fill=SST)) + 
  geom_sf(data=awz_bs, fill=NA) + coord_sf() + 
  scale_fill_gradientn(
    colours=rev(colours),
    values=val.remap,
    breaks=quantile.vals,# Necessary to get legend values spread appropriately
    guide = guide_legend(reverse = TRUE)) +       # Necessary to get legend values spread appropriately
  theme_bw() + theme(axis.title = element_blank(),
                     legend.key.height = unit(0.5, 'cm')) #change legend key height
p1 / p2 / p3
ggsave(filename="figures/env_dat_bs.png", width=6, height=6.5, dpi=1000)