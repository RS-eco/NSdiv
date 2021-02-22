# Benthos GetCapabilities
#a <- "https://www.geoseaportal.de/wss/service/BIO_Benthos_Density/guest?SERVICE=WMS&REQUEST=GetCapabilities"

# INSPIRE-Thema Höhe (Tiefenlinien)
#b <- "https://www.geoseaportal.de/inspire/services/ELV_INSPIRE_VS?VERSION=1.3.0&SERVICE=WMS&REQUEST=GetCapabilities"

# INSPIRE - Existierende Bodennutzung (CONTIS)
#c <- "https://www.geoseaportal.de/inspire/services/ELU_INSPIRE_VS?VERSION=1.3.0&SERVICE=WMS&REQUEST=GetCapabilities"

# Benthos GetMap
# is still not correct
#https://www.geoseaportal.de/wss/service/BIO_Benthos_Density/guest?SERVICE=WMS&REQUEST=GetMap&CRS=CRS:84&bbox=8.470934,53.545298,3.347934,56.033708&width=760&height=360&layers=1&styles=default&format=image/png

# GetCapabilities - ESRI Example
#http://sampleserver1.arcgisonline.com/ArcGIS/services/Specialty/ESRI_StatesCitiesRivers_USA/MapServer/WMSServer?version=1.3.0&amp;request=GetCapabilities&amp;service=WMS

# GetMap - ESRI Example
#http://sampleserver1.arcgisonline.com/ArcGIS/services/Specialty/ESRI_StatesCitiesRivers_USA/MapServer/WMSServer?version=1.3.0&request=GetMap&CRS=CRS:84&bbox=-178.217598,18.924782,-66.969271,71.406235&width=760&height=360&layers=0&styles=default&format=image/png

# Load packages
library(robis)
library(dplyr); library(dtplyr); library(ggplot2); library(magrittr)
library(geodat); library(sf)

# Get AWZ data
data(eez)
awz <- eez %>% filter(Country == "Germany"); rm(eez)
awz2 <- st_buffer(awz, dist=0.1)
ggplot() + geom_sf(data=awz, fill=NA) + geom_sf(data=awz2, col="red", fill=NA)

awz_split <- st_cast(awz2, "POLYGON")
awz_split$id <- 1:2

awz_bs <- awz_split[1,]
awz_ns <- awz_split[2,]

st_bbox(awz_bs); st_bbox(awz_ns)

# Download AWZ data for all species
obis_awz_bs <- occurrence(geometry = "POLYGON ((9 53, 9 56, 15 56, 15 53, 9 53))")
obis_awz_bs %<>% select(c(country, year, scientificName, decimalLatitude, basisOfRecord, terrestrial, occurrenceStatus,
                       maximumDepthInMeters, day, order, decimalLongitude, month, brackish, absence, 
                       marine, minimumDepthInMeters, class, sex, phylum, lifeStage, subphylum, family, category, sss,
                       shoredistance, sst, bathymetry, subfamily, individualCount, locality, suborder, depth, hab,
                       occurrenceRemarks, type))
obis_awz_bs <- sf::st_as_sf(obis_awz_bs, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326); gc()
# Subset OBIS data by AWZ outline
obis_awz_bs <- st_intersection(obis_awz_bs, st_geometry(awz_bs))
saveRDS(obis_awz_bs, file="data/robis_awz_bs_18022021.rds", compress="xz")

obis_awz_ns <- occurrence(geometry = "POLYGON ((3 53, 3 57, 10 57, 10 53, 3 53))")
obis_awz_ns %<>% select(c(country, year, scientificName, decimalLatitude, basisOfRecord, terrestrial, occurrenceStatus,
                       maximumDepthInMeters, day, order, decimalLongitude, month, brackish, absence, 
                       marine, minimumDepthInMeters, class, sex, phylum, lifeStage, subphylum, family, category, sss,
                       shoredistance, sst, bathymetry, subfamily, individualCount, locality, suborder, depth, hab,
                       occurrenceRemarks, type))
obis_awz_ns <- sf::st_as_sf(obis_awz_ns, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326); gc()
# Subset OBIS data by AWZ outline
obis_awz_ns <- st_intersection(obis_awz_ns, st_geometry(awz_ns))
saveRDS(obis_awz_ns, file="data/robis_awz_ns_18022021.rds", compress="xz")

obis_awz_ns <- readRDS("data/robis_awz_ns_18022021.rds")
obis_awz_coords <- do.call(rbind, sf::st_geometry(obis_awz_ns)) %>% 
  as.data.frame() %>% setNames(c("lon","lat"))
obis_awz_ns <- obis_awz_ns %>% as.data.frame() %>% select(-geometry) %>% bind_cols(obis_awz_coords)
colnames(obis_awz_ns); head(obis_awz_ns)

obis_awz_bs <- readRDS("data/robis_awz_bs_18022021.rds")
obis_awz_coords <- do.call(rbind, sf::st_geometry(obis_awz_bs)) %>% 
  as.data.frame() %>% setNames(c("lon","lat"))
obis_awz_bs <- obis_awz_bs %>% as.data.frame() %>% select(-geometry) %>% bind_cols(obis_awz_coords)
colnames(obis_awz_bs); head(obis_awz_bs)

obis_awz <- bind_rows(obis_awz_ns, obis_awz_bs)
vroom::vroom_write(obis_awz, "data/robis_awz_18022021.csv.xz")
#saveRDS(obis_awz, "data/robis_awz_18022021.rds", compress="xz")

obis_awz <- vroom::vroom("data/robis_awz_18022021.csv.xz")
colnames(obis_awz)

# Species & associated phyla
sp_phyl <- obis_awz %>% dplyr::select(phylum, scientificName) %>% distinct()

# Subset data
obis_sub <- obis_awz %>% filter(year %in% 1998:2018) %>%
  filter(phylum %in% c("Annelida", "Arthropoda", "Chordata", "Echinodermata", "Mollusca", "Cnidaria", "Chaetognatha", "Ctenophora",
                       "Porifera", "Bryozoa", "Nematoda", "Hemichordata")) %>% 
  filter(basisOfRecord == "Occurrence") %>%
  dplyr::select(year, scientificName, lon, lat) %>% tidyr::drop_na(scientificName)
rm(obis_awz); gc()

# List through species and turn data into raster
r <- raster::raster(res=c(0.08333333, 0.08333333), xmn=3.35, xmx=14.75, ymn=53.225, ymx=55.91667,
                    crs="+proj=longlat +datum=WGS84 +no_defs")
r[] <- 1
r
df <- raster::as.data.frame(r, xy=T)
df2 <- as.data.frame(raster::rasterToPoints(r))

length(unique(obis_sub$scientificName))

r_dat <- lapply(unique(obis_sub$scientificName), function(x){
  dat <- obis_sub %>% filter(scientificName == x) %>% dplyr::select(lon, lat) %>% 
    dplyr::group_by(lon, lat) %>% dplyr::summarise(n=n()) %>% ungroup()
  if(nrow(dat) > 0){
    sp::coordinates(dat) <- ~lon+lat
    dat <- raster::rasterize(x=dat, y=r, field="n", fun="last")
    return(dat)
  } else{
    return(NULL)
  }
})
# Remove NULLs from List
which(sapply(r_dat, is.null))
r_dat <- Filter(Negate(is.null), r_dat)
r_dat <- raster::stack(r_dat)
r_dat
names(r_dat) <- unique(obis_sub$scientificName)
writeRaster(r_dat[[1:10]], filename="data/obis_awz_test.nc", compression=9, overwrite=T)
df_dat <- as.data.frame(raster::rasterToPoints(r_dat)); rm(r_dat); gc()
colnames(df_dat) <- c("x", "y", unique(obis_sub$scientificName))
head(df_dat[1:10,1:10])
vroom::vroom_write(df_dat, "data/obis_awz_grid_10km.csv.xz")
