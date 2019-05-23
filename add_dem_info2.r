# Simon attempt to add DEM data to DairyBase file using raster
# Note: raster package can work with data on disk (doesn't need to be in memory)

rm(list=ls()) # remove all variables

library(tidyverse)
library(cowplot) # applies to all plots
library(sf)
library(raster) # see http://neondataskills.org/R/Raster-Data-In-R/
library(elevatr) # access DEM data

# functions
ensnakeify <- function(x) {
  x %>%
    iconv(to="ASCII//TRANSLIT") %>% # remove accents
    str_replace_na() %>% # convert NA to string
    str_to_lower() %>% # convert to lower case
    str_replace_all(pattern="[^[:alnum:]]", replacement=" ") %>% # convert non-alphanumeric to space
    str_trim() %>% # trim leading and trailing spaces
    str_replace_all(pattern="\\s+", replacement="_") # convert remaining spaces to underscore
}

# define coordinate systems
nztm2000string <- "+proj=tmerc +lat_0=0 +lon_0=173 +k=0.9996 +x_0=1600000 +y_0=10000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
wgs84string <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

#### read dairybase data ####
file_name <- "DairyBaseForPowerBIsoils.csv"
farms <- read_csv(file_name, na=c("","NaN","NA"), col_types=cols(Concatbest="c")) # force "Concatbest" to chr
names(farms) <- ensnakeify(names(farms))
farms <- st_as_sf(farms, coords=c("long", "lat"), remove=FALSE, crs=wgs84string) # create sf dataframe
ggplot() +
  labs(title="Farms") +
  coord_sf() + # ensures consistent projection
  geom_sf(data=farms, mapping=aes(color=as.factor(district_council)), size=0.1) # size < 1 doesn't work

# choose some farms and then get the relevant DEM data from Amazon
# https://cran.r-project.org/web/packages/elevatr/vignettes/introduction_to_elevatr.html
sub_farms <- farms %>%
  filter(district_council==20) 
  # filter((long>175) & (long<176) & (lat< -37) & (lat>-38)) 
sub_farms_sp <- as(sub_farms$geometry, "Spatial")
sub_dem <- get_elev_raster(sub_farms_sp, z=10, src="aws") # about 100 m resolution
plot(sub_dem)
points(sub_farms_sp)

# raster package for handling grided data
# aster_dem <- "dem\\ASTGTM2_S38E175\\ASTGTM2_S38E175_dem.tif" # Hamilton-Hauraki Area, 25Mb (WGS84 longlat)
# DEM <- raster(aster_dem) 
# projection(DEM) # it's in wgs84 (longlat)
# plot(DEM, main="Hamilton-Hauraki DEM")

slope <- terrain(sub_dem, opt="slope", unit="degrees") # can handle long-lat-metres
plot(slope, main="Hamilton-Hauraki Slope")
points(sub_farms_sp)

aspect <- terrain(sub_dem, opt="aspect", unit="degrees") # can handle long-lat-metres
plot(aspect, main="Hamilton-Hauraki Aspect")
points(sub_farms_sp)

# extract data
sub_farms$slope  <- raster::extract(slope, sub_farms_sp)
sub_farms$aspect <- raster::extract(aspect, sub_farms_sp)
