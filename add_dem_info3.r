# Simon attempt to add DEM data to DairyBase file using raster
# Note: raster package can work with data on disk (doesn't need to be in memory)

rm(list=ls()) # remove all variables

library(tidyverse)
library(cowplot) # applies to all plots
library(sf)
library(sp)
library(scales)
library(raster) # see http://neondataskills.org/R/Raster-Data-In-R/
library(elevatr) # access DEM data

# define coordinate systems
nztm2000string <- "+proj=tmerc +lat_0=0 +lon_0=173 +k=0.9996 +x_0=1600000 +y_0=10000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
wgs84string <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# ggplot coastline
# nz <- map_data("nz")

#### read dairybase data ####
file_name <- "DairyBaseForPowerBIsoils.csv"
data <- read_csv(file_name, na=c("","NaN","NA"), col_types=cols(Concatbest="c")) %>% # force "Concatbest" to chr
  mutate(slope=NA, aspect=NA, flowdir=NA, tpi=NA)
farms <- data %>% # get farm coordinates
  dplyr::select(DBID, long, lat) %>%
  distinct(DBID, .keep_all=TRUE) %>%
  arrange(DBID)
res <- 0.5 # group locations onto a grid of this resolution
minlong <- min(farms$long)
maxx <- max(floor((farms$long-minlong)/res))
minlat <- min(farms$lat)
maxy <- max(floor((farms$lat-minlat)/res))
farms <- farms %>%
  mutate(grid=floor((long-minlong)/res) + floor((lat-minlat)/res)*(maxx+1))
grids <- farms %>% # grid density
  group_by(grid) %>%
  summarise(count=n()) %>%
  arrange(desc(count)) 
  
# plot farms
farms_sf <- st_as_sf(farms, coords=c("long", "lat"), remove=FALSE, crs=wgs84string) # create sf dataframe
ggplot() + # check
  labs(title="Farms") +
  coord_sf() + # ensures consistent projection
  geom_sf(data=farms_sf, mapping=aes(colour=as.factor(grid)), size=0.1) # size < 1 doesn't work

# choose grid
agrid <- grids$grid[1]
print(paste('Not found =', sum(is.na(data$slope)))) # report progress
for (agrid in grids$grid){
  
# create SpatialPointsDataFrame
farms_sp <- as(filter(farms_sf, grid==agrid), "Spatial")
print(paste("Grid No.", agrid, " Farms =", nrow(farms_sp)))

# farms_sp <- SpatialPointsDataFrame(coords=cbind(farms$long, farms$lat), 
#                                    data=data.frame(DBID=farms$DBID, grid=farms$grid), 
#                                    proj4string=CRS(wgs84string))
  
# choose some farms and then get the relevant DEM data from Amazon
# https://cran.r-project.org/web/packages/elevatr/vignettes/introduction_to_elevatr.html
# https://mapzen.com/documentation/terrain-tiles/data-sources/#what-is-the-ground-resolution
dem <- get_elev_raster(farms_sp, z=10, src="aws", expand=res/10) # about 100 m resolution
plot(dem, main=paste("Elevation, Grid", agrid))
points(farms_sp)

# raster package for handling grided data
# aster_dem <- "dem\\ASTGTM2_S38E175\\ASTGTM2_S38E175_dem.tif" # Hamilton-Hauraki Area, 25Mb (WGS84 longlat)
# DEM <- raster(aster_dem) 
# projection(DEM) # it's in wgs84 (longlat)
# plot(DEM, main="Hamilton-Hauraki DEM")

slope <- terrain(dem, opt="slope", unit="degrees") # can handle long-lat-metres
aspect <- terrain(dem, opt="aspect", unit="degrees") # can handle long-lat-metres
flowdir <- terrain(dem, opt="flowdir", unit="degrees") # can handle long-lat-metres
tpi <- terrain(dem, opt="TPI", unit="degrees") # can handle long-lat-metres

# extract data
farms_sp$slope  <- raster::extract(slope, farms_sp)
farms_sp$aspect <- raster::extract(aspect, farms_sp)
farms_sp$flowdir <- raster::extract(flowdir, farms_sp)
farms_sp$tpi <- raster::extract(tpi, farms_sp)

# write to data frame
i <- match(data$DBID, farms_sp$DBID)
data$slope <- ifelse(is.na(i), data$slope, farms_sp$slope[i])
data$aspect <- ifelse(is.na(i), data$aspect, farms_sp$aspect[i])
data$flowdir <- ifelse(is.na(i), data$flowdir, farms_sp$flowdir[i])
data$tpi <- ifelse(is.na(i), data$tpi, farms_sp$tpi[i])

print(paste('Not found =', sum(is.na(data$slope)))) # report progress

}

p <- ggplot() + # results
  labs(title="Slope") +
  scale_colour_distiller(palette="Spectral", direction=-1) +
  coord_fixed() +
  geom_point(data=data, mapping=aes(x=long, y=lat, colour=slope)) 
print(p)

file_name <- "DairyBaseForPowerBIdem.csv"
write_csv(data, file_name)

