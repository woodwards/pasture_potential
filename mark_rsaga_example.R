#NOTE: IMPORTANT - Install version 2.2.3 of SAGA - most recent version (v6) not compatible with rsaga.
#NOTE: IMPORTANT - either move installation to C:\Program Files\SAGA-GIS after installation (easiest), or change path in Rstudio or windows.
#Note: spaces in path may have a negative effect on functionality according to one source

#Below is example from CRAN documentation of rsaga. https://cran.r-project.org/web/packages/RSAGA/vignettes/RSAGA-landslides.pdf
#Tutorial of some additional use http://gracilis.carleton.ca/CUOSGwiki/index.php/Introducing_Geoprocessing_Capabilities_of_SAGA_in_R_Environment_using_RSAGA_(Saga_%2B_Rstudio)

setwd("C:/Users/nealm/Desktop/dairynz/Pasture")

#install.packages("RSAGA") #once only
library(RSAGA)
library(raster)
library(rgdal)
#rsaga doesn'r store environmental variables like many other packages etc., so create an object to reuse.
rsaga.env()
env <- rsaga.env()
env$version

#this makes the modules available to rsaga, but assumes they were already be installed with saga, which they are with basic download.
#modules live in saga libraries, but saga v6 has a different architecture i think, so that is why it doesn't work with current version of rsaga
rsaga.get.libraries(path = rsaga.env()$modules)
rsaga.get.modules(c("io_grid", "grid_tools", "ta_preprocessor",
                    "ta_morphometry", "ta_lighting", "ta_hydrology"))
#rsaga.get.modules(interactive=TRUE)

#example from documentation
data(landslides)
class(dem)

##Note: you convert dem to saga's format (sgrd) and then operate on that. 
##I assume you then translate back to another format to further manipulate in R (eg shp file, or spatial dataframe)
write.sgrd(data = dem, file = "dem", header = dem$header, env = env) # write.sgrd and read.sgrd use SAGA, and should specify 'env'
?write.sgrd
#warnings()



# By individual function calls:
##note: these provide output files that you can view via SAGA program, don't expect a plot to come up in R!
rsaga.slope("dem", "slope", method = "poly2zevenbergen", env = env)
rsaga.plan.curvature("dem", "cplan", method = "poly2zevenbergen", env = env)
rsaga.profile.curvature("dem", "cprof", method = "poly2zevenbergen", env = env)


#This creates a sgrd from a tif 
rsaga.import.gdal('C:/Users/nealm/Desktop/dairynz/NZmaps/05-auckland-15m-dem-nzsosdem-v10.tif',"waikato")

# By individual function calls
rsaga.slope("waikato", "waikatoslope", method = "poly2zevenbergen", env = env)

#convert to esri and then plot
rsaga.sgrd.to.esri("waikato","WaikatoEsri")
gridmaps <- readGDAL("WaikatoEsri.asc")
plot(gridmaps)
#?readGDAL
#pick.from.saga.grid(landslides, "slope", varname = "slope", env = env,
                                  X.name = "x", Y.name = "y")



# By one function that calculates each of the terrain parameters:
# rsaga.slope.asp.curv("dem", out.slope = "slope",
#                      out.cprof = "cprof", out.cplan = "cplan",
#                      method = "poly")

# rsaga.search.modules("sink")
# rsaga.get.modules('grid_filter')



