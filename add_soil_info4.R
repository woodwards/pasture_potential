# coastline data from 
# http://naturalearthdata.com/downloads/10m-physical-vectors

rm(list=ls()) # remove all variables

# library(rgdal)
# library(sp)
library(tidyverse)
library(sf)
library(cowplot) # applies to all plots

# define coordinate systems
# http://spatialreference.org 
# WGS84 - World Geodetic System 1984 - Commonly used datum. Changes with time.
# NZGD2000 - NZ Geodetic Datum 2000 - very close to WGS84. Static with time. Generally assumed = WGS84.
# NZMG = NZGD1949 - origin 41S 173E - obsolete
# NZTM2000 - NZ Tranverse Mercator - replaced NZMG - origin 0S 173E
nztm2000string <- "+proj=tmerc +lat_0=0 +lon_0=173 +k=0.9996 +x_0=1600000 +y_0=10000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
wgs84string <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

#### read daiybase data and tidy up ####
file_name <- "DairyBaseForPowerBI.csv"
farms <- read_csv(file_name, na=c("","NaN","NA"), col_types=cols(Concatbest="c")) # force "Concatbest" to chr

# read farm locations
# farms <- read_csv('Farm locations all.csv') %>%
#   select(Longitude, Latitude, SupplyID) %>%
#   drop_na()
farms2 <- st_as_sf(farms, coords=c("long", "lat"), remove=FALSE) # create sf dataframe
st_crs(farms2) <- wgs84string # specify projection (this can be done in st_as_sf(crs=wgs84string))
print(farms2, n=3) # print atributes and first few rows

ggplot() +
  labs(title="Farms") +
  coord_sf() + # ensures consistent projection
  geom_sf(data=farms2, size=0.1, fill='green', color='blue') # size < 1 doesn't work

#### read soil information ####
soils <-  st_read(dsn="./shapefiles", layer="fsl-new-zealand-soil-classification") # using sf
st_crs(soils) <- nztm2000string # specify projection
soils <- st_transform(soils, crs=wgs84string) # convert to WGS84
print(soils, n=3) # print attributes and first few rows

soils2 <- soils[1:10000, ] # take a smaller subset for testing
ggplot() +
  labs(title="NZ Soil Order Map", fill='Soil Order') +
  coord_sf() + # ensures consistent projection
  geom_sf(data=soils2, mapping=aes(fill=nzsc_order), color=NA) 

#### find soil order for each farm location (only a point though) ####
i <- as.integer(st_within(farms2, soils))
farms2$nzsc_order <- soils$nzsc_order[i]

print(paste('Not found =', sum(is.na(farms2$nzsc_order)))) # report failures

ggplot() +
  geom_bar(data=farms2, mapping=aes(x=nzsc_order, fill=nzsc_order))

file_name <- "DairyBaseForPowerBIsoils.csv"
write_csv(farms2, file_name)

