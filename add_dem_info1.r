# Simon attempt to add DEM data to DairyBase file using rsaga

# raster package can work with data on disk (doesn't need to be in memory)
# library(rgdal) # base spatial data package
library(raster) # see http://neondataskills.org/R/Raster-Data-In-R/
library(RSAGA) # http://www.saga-gis.org/en/index.html
# library(maptools)

# define coordinate systems
# http://spatialreference.org 
# WGS84 - World Geodetic System 1984 - Commonly used datum. Changes with time.
# NZGD2000 - NZ Geodetic Datum 2000 - very close to WGS84. Static with time. Generally assumed = WGS84.
# NZMG = NZGD1949 - origin 41S 173E - obsolete
# NZTM2000 - NZ Tranverse Mercator - replaced NZMG - origin 0S 173E
nztm2000string <- "+proj=tmerc +lat_0=0 +lon_0=173 +k=0.9996 +x_0=1600000 +y_0=10000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
wgs84string <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# get some data to play with
# https://data.linz.govt.nz/group/national-elevation/data/tag/land-geodesy/global/oceania/new-zealand/?s=r&v=csv
# Actually got data from ASTER
# see also getData("alt", country='NZL')

# raster package for handling grided data
aster_dem <- "dem\\ASTGTM2_S38E175\\ASTGTM2_S38E175_dem.tif" # Hamilton-Hauraki Area, 25Mb (WGS84 longlat)
DEM <- raster(aster_dem) 
projection(DEM) # it's in wgs84 (longlat)
plot(DEM, main="Hamilton-Hauraki DEM (wgs84)")

# change projection ?
# https://www.rdocumentation.org/packages/raster/versions/1.0.0-1/topics/projectRaster
new_aster_dem <- "dem\\ASTGTM2_S38E175\\ASTGTM2_S38E175_new_dem.tif" 
new_DEM <- projectExtent(DEM, nztm2000string)
res(DEM)
res(new_DEM)
res(new_DEM) <- 50 # metres per cell ? affects memory requirement
new_DEM <- projectRaster(from=DEM, to=new_DEM) 
plot(new_DEM, main="Hamilton-Hauraki DEM (nztm2000)")
writeRaster(new_DEM, new_aster_dem, overwrite=TRUE) 

# RSAGA
# https://www.r-bloggers.com/rsaga-getting-started/
# http://gracilis.carleton.ca/CUOSGwiki/index.php/Introducing_Geoprocessing_Capabilities_of_SAGA_in_R_Environment_using_RSAGA_(Saga_%2B_Rstudio)
# https://gis.stackexchange.com/questions/175791/strange-output-from-rsaga-topographic-wetness-index
# had to put saga_2.2.3_x64/ in C:/Users/WoodwardS/Documents/R/win-library/3.4/RSAGA/SAGA-GIS

# import as SAGA files (writes set of 5 files)
work_env <- rsaga.env()
saga_dem <- "dem\\ASTGTM2_S38E175\\ASTGTM2_S38E175_dem"
rsaga.import.gdal(in.grid=new_aster_dem, out.grid=saga_dem, env=work_env) # use this instead

# how to explore SAGA functionality
# rsaga.get.libraries() # what's available ?
# rsaga.get.modules("grid_filter") # what's available ?
# rsaga.get.usage('grid_filter', 7) # how do you use it ?

# calculate slope of DEM as SAGA files (set of 5 files)
saga_slope <- "dem\\ASTGTM2_S38E175\\ASTGTM2_S38E175_slope"
rsaga.slope(in.dem=saga_dem, out.slope=saga_slope, env=work_env)
slope <- raster(paste(saga_slope, ".sdat", sep="")) # read SAGA files as raster for plotting
projection(slope)
plot(slope, main="Hamilton Hauraki Slope") 

# calculate slope of DEM as SAGA files (set of 5 files)
saga_wetness <- "dem\\ASTGTM2_S38E175\\ASTGTM2_S38E175_wetness"
rsaga.wetness.index(in.dem=saga_dem, out.wetness.index=saga_wetness, env=work_env) # slow!!!
wetness <- raster(paste(saga_wetness, ".sdat", sep=""))
projection(wetness)
plot(wetness, main="Hamilton Hauraki Wetness") # possibly makes more sense on small scale.
