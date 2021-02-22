rm(list=ls()); gc()

# Load packages
library(sf)
library(dplyr); library(magrittr); library(tidyr)
library(ggplot2); library(patchwork); library(scico)

# Get AWZ outline
library(geodat)
data("eez")
awz <- eez %>% filter(Country == "Germany"); rm(eez); gc()

# Load gridded data
df_dat <- vroom::vroom("data/obis_awz_grid_10km.csv.xz") %>%
  pivot_longer(cols=-c(x,y), names_to = "species", values_to = "count") %>% drop_na()
df_dat$presence <- 1
sr_dat <- df_dat %>% group_by(x,y) %>% summarise(sr=sum(presence), sampling_effort=sum(count))

# Calculate SR for different phyla
sp_phyl <- vroom::vroom("data/robis_awz_18022021.csv.xz") %>% dplyr::select(phylum, scientificName) %>% distinct(); invisible(gc())
sr_phyl <- df_dat %>% left_join(sp_phyl, by=c("species"="scientificName")) %>%
  group_by(x,y,phylum) %>% summarise(sr=sum(presence))
sr_phyl_wide <- sr_phyl %>% spread(phylum, sr)

sp_example <- raster::stack("data/obis_awz_test.nc")
env_dat <- raster::stack("data/marspec_awz.nc")
env_dat <- raster::extend(env_dat, sp_example)
sp_example <- raster::extend(sp_example, env_dat)
env_dat <- raster::resample(env_dat, sp_example)
env_dat <- raster::stack(sp_example[[1]], env_dat) 
env_dat <- raster::mask(env_dat, awz)
raster::plot(env_dat)

#env_dat <- raster::aggregate(env_dat, fact=10)

env_dat %<>% raster::rasterToPoints() %>% as.data.frame()
colnames(env_dat) <- c("x", "y", "X1.1", "bathy_30s", "biogeo08_30s", "biogeo13_30s")
env_dat %<>% select(-c("X1.1"))

ggplot() + geom_tile(data=env_dat, aes(x=x, y=y, fill=bathy_30s)) + 
  geom_sf(data=awz, fill=NA) + coord_sf() + 
  scale_fill_scico(palette="roma", na.value="transparent")

all_dat <- left_join(env_dat, sr_dat) %>% left_join(sr_phyl_wide, by=c("x", "y"))
colnames(all_dat)
summary(all_dat$Echinodermata)

ggplot() + geom_tile(data=all_dat, aes(x=x, y=y, fill=bathy_30s)) + 
  geom_sf(data=awz, fill=NA) + coord_sf() + 
  scale_fill_scico(palette="roma", na.value="transparent")

ggplot() + geom_tile(data=all_dat, aes(x=x, y=y, fill=sr)) + 
  geom_sf(data=awz, fill=NA) + coord_sf() + 
  scale_fill_gradientn(colours = scico(255, palette = 'roma'), na.value="transparent")

ggplot() + geom_tile(data=all_dat, aes(x=x, y=y, fill=Arthropoda)) + 
  geom_sf(data=awz, fill=NA) + coord_sf() + 
  scale_fill_gradientn(colours = scico(255, palette = 'roma'), na.value="transparent")

p1 <- ggplot(data=all_dat, aes(x=bathy_30s, y=sr)) + geom_point() + geom_smooth() + theme_bw() + labs(x="Bathymetrie", y="Species Richness")
p2 <- ggplot(data=all_dat, aes(x=biogeo08_30s/100, y=sr)) + geom_point() + geom_smooth() + theme_bw() + labs(x="SSS", y="")
p3 <- ggplot(data=all_dat, aes(x=biogeo13_30s/100, y=sr)) + geom_point() + geom_smooth() + theme_bw() + labs(x="SST", y="")
p1 + p2 + p3
ggsave(filename="figures/obis_awz_sr_env.png", width=10, height=5, dpi=1000)

phyl_dat <- left_join(env_dat, sr_phyl) %>% filter(phylum %in% c("Annelida", "Arthropoda", "Chordata", "Cnidaria", "Echinodermata", "Mollusca"))

p1 <- ggplot(data=phyl_dat, aes(x=bathy_30s, y=sr, colour=phylum)) + geom_point() + geom_smooth() + 
  scale_colour_manual(values=ggsci::pal_d3("category10")(6)) + theme_bw() + theme(legend.position = "none") + 
  scale_y_continuous(limits=c(0,NA), expand=c(0,1)) + labs(x="Bathymetrie", y="Species Richness")
p2 <- ggplot(data=phyl_dat, aes(x=biogeo08_30s/100, y=sr, colour=phylum)) + geom_point() + geom_smooth() + 
  scale_colour_manual(values=ggsci::pal_d3("category10")(6)) + theme_bw() + theme(legend.position = "none") + 
  scale_y_continuous(limits=c(0,NA), expand=c(0,1)) + labs(x="SSS", y="")
p3 <- ggplot(data=phyl_dat, aes(x=biogeo13_30s/100, y=sr, colour=phylum)) + geom_point() + geom_smooth() + 
  scale_colour_manual(name="Phylum", values=ggsci::pal_d3("category10")(6)) + 
  scale_y_continuous(limits=c(0,NA), expand=c(0,1)) + theme_bw() + labs(x="SST", y="")
p1 + p2 + p3
ggsave(filename="figures/obis_awz_sr_phyl_env.png", width=10, height=5, dpi=1000)
