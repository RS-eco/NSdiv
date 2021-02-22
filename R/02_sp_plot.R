rm(list=ls()); gc()

# Load packages
library(raster); library(sf)
library(dplyr); library(magrittr); library(tidyr)
library(ggplot2); library(patchwork); library(scico)

# Get AWZ outline
library(geodat)
data("eez")
awz <- eez %>% filter(Country == "Germany"); rm(eez); gc()

obis_awz <- vroom::vroom("data/robis_awz_18022021.csv.xz")
colnames(obis_awz)

ggplot() + 
  geom_point(data=obis_awz, aes(x=lon, y=lat)) + 
  geom_sf(data=awz, fill=NA) + coord_sf()
ggsave(filename="figures/obis_awz_map.png", width=9, height=4, dpi=1000)

# Check time period
sort(unique(obis_awz$year))

# Species & associated phyla
sp_phyl <- obis_awz %>% dplyr::select(phylum, scientificName) %>% distinct()

# Get number of observations per phylum
(phyl_ob <- obis_awz %>% as.data.frame() %>% filter(year %in% 1998:2018) %>%
    group_by(phylum) %>% summarise(n=n()) %>% tidyr::drop_na() %>%
    arrange(desc(n)))

# Plot number of observations over time
obis_awz %>% filter(year %in% 1998:2018) %>%
  left_join(phyl_ob) %>% filter(n >= 5000) %>% 
  ggplot() + geom_bar(aes(x = as.numeric(year), fill = phylum)) +
  scale_fill_manual(values=ggsci::pal_d3("category20")(13)) + 
  labs(x="Jahr", y="Anzahl an Beobachtungen")
ggsave(filename="figures/obis_awz_observations.png", width=8, height=6, dpi=1000)

# Check taxa
length(unique(obis_awz$scientificName))
length(unique(obis_awz$family))
length(unique(obis_awz$order))
length(unique(obis_awz$class))
length(unique(obis_awz$phylum))

unique(obis_awz$phylum)
obis_nophyla <- obis_awz %>% filter(is.na(phylum))
unique(obis_nophyla$scientificName)

# Load gridded data
df_dat <- vroom::vroom("data/obis_awz_grid_10km.csv.xz") %>%
  pivot_longer(cols=-c(x,y), names_to = "species", values_to = "count") %>% drop_na()
df_dat$presence <- 1
sr_dat <- df_dat %>% group_by(x,y) %>% summarise(sr=sum(presence), sampling_effort=sum(count))

# Plot species richness
sr_dat %>% 
  ggplot() + geom_tile(aes(x=x, y=y, fill=sr)) + 
  scale_fill_scico(palette="roma", direction=-1) + 
  geom_sf(data=awz, fill=NA) + coord_sf() + theme_bw() + 
  theme(axis.title = element_blank())
ggsave(filename="figures/obis_awz_sr_map.png", width=10, height=4, dpi=1000)

# Plot sampling effort
ggplot() + geom_tile(data=sr_dat, aes(x=x, y=y, fill=sampling_effort)) + 
  geom_sf(data=awz, fill=NA) + coord_sf() + 
  scale_fill_scico(palette="roma", direction=-1) + theme_bw() + 
  theme(axis.title = element_blank())
ggsave(filename="figures/obis_awz_samplingeffort_map.png", width=10, height=4, dpi=1000)

# Plot species richness per phyla
sr_phyl <- df_dat %>% left_join(sp_phyl, by=c("species"="scientificName")) %>%
  group_by(x,y,phylum) %>% summarise(sr=sum(presence))
sr_phyl %>% filter(phylum %in% c("Annelida", "Arthropoda", "Chordata", "Cnidaria", "Echinodermata", "Mollusca")) %>%
  # Dropped "Chaetognatha", "Ctenophora", "Porifera", "Bryozoa", "Nematoda", "Hemichordata"
  ggplot() + geom_tile(aes(x=x, y=y, fill=sr)) + 
  facet_wrap(.~phylum) + 
  scale_fill_scico(palette="roma", direction=-1) + 
  geom_sf(data=awz, fill=NA) + coord_sf() + theme_bw() + 
  theme(axis.title = element_blank())
ggsave(filename="figures/obis_awz_sr_phyl_map.png", width=8, height=3, dpi=1000)

# Plot occurrence of single species
df_dat %>% filter(species == "Limanda limanda") %>% 
  ggplot() + geom_tile(aes(x=x, y=y, fill=presence)) + 
  geom_sf(data=awz, fill=NA) + coord_sf() + theme_bw() + 
  theme(legend.position = "none", axis.title = element_blank())
