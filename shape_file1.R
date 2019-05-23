# coastline data from 
# http://naturalearthdata.com/downloads/10m-physical-vectors

library(rgdal)

shape <- readOGR(dsn="./ne_10m_coastline", layer="ne_10m_coastline") # class "SpatialLinesDataFrame"

plot(shape) # works!

spplot(shape)

# https://cran.r-project.org/web/packages/sp/vignettes/intro_sp.pdf