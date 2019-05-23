# read-write-shapefiles.R #####
# A hodge podge of spatial analysis and mapping
# By Mark Neal, although I reserve all rights to Plausible Deniability

# From Jim Regetz & Rick Reeves
# R code intended as a basic demonstration of how to read and write ESRI
# Shapefiles in R, including points, lines, and polygons.
#
# Input Data
#  * nw-counties.* 
#      Polygon shapefile of counties across five US states (WA, OR, ID,
#      MT, and WY)
#  * nw-centroids.*
#      Point shapefile of county centroids       
#  * nw-rivers.*
#      Line shapefile of major rivers in the region
#
# Output
#  * Two different copies of the county polygon shapefile, written out
#    by rgdal and maptools functions.
#  * Map visualization (PNG format).
#
# Notes
#  * The rgdal and maptools approaches both produce Spatial*DataFrame
#    objects in R, as defined in the 'sp' package.
#  * The PBSmapping approach produces objects in a custom format
#    specific to that package.
# 
# Authors: Jim Regetz & Rick Reeves
# Last modified: 21-Nov-2011
# National Center for Ecological Analysis and Synthesis (NCEAS),
# http://www.nceas.ucsb.edu/scicomp


# ---------- rgdal ---------- #

library(rgdal)
library(rgeos)
library(raster)

# for shapefiles, first argument of the read/write/info functions is the
# directory location, and the second is the file name without suffix

# optionally report shapefile details
#setwd("C:/Users/nealm/Desktop/dairynz/read-write-shapefiles")
setwd("C:/Users/nealm/Desktop/dairynz/NZmaps")
#?ogrInfo
ogrInfo(".", "NZL_adm2")
# Source: ".", layer: "NZL_adm2"
# Driver: ESRI Shapefile number of rows 12 
# Feature type: wkbLineString with 2 dimensions
# +proj=longlat +datum=WGS84 +no_defs  
# Number of fields: 2 
#     name type length typeName
#     1   NAME    4     80   String
#     2 SYSTEM    4     80   String

############################# Read in shapefiles ####

#centroids.rg <- readOGR(".", "nw-centroids")
#rivers.rg <- readOGR(".", "nw-rivers")
#regions.rg <- readOGR(".", "NZL_adm0") #NZ
#regions.rg <- readOGR(".", "NZL_adm1") #16 regions
#regions.rg <- readOGR(".", "NZL_adm2") #81 sub regions
#regions.rg <- readOGR(".", "NZL_adm3") #448 sub-sub regions
#regions.rg <- readOGR(".", "nz-post-postcode-boundaries-june-2011-licensed-only-for-mix-") # postcodes, 1065 (2011, via koordinates)
#regions.rg <- readOGR(".", "nz-territorial-authorities-2012-yearly-pattern") #territorial 2012 via koordinates
regions.rg <- readOGR(".", "nz-territorial-authorities-2006-census") #territorial 2006 via koordinates #This is good!
#regions.rg <- readOGR(".", "fsl-new-zealand-soil-classification") #from Hans
#regions.rg <- readOGR(".", "Freshwater_Management_Units") #Waitaki region from ecan 
# note that readOGR will read the .prj file if it exists
print(proj4string(regions.rg))
# [1] " +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#regions.rg <- readOGR(".", "HEALTHY_RIV_CATCHMENT") #territorial 2006 via koordinates #This is good!
summary(regions.rg@data$nzsc_order)

?reorder
# # read a DEM (Digital Elevation Map) and plot ###########
# # http://neondataskills.org/R/Raster-Data-In-R/
# # DEM from koordinates
# DEM <- raster("05-auckland-15m-dem-nzsosdem-v10.tif")
# DEM
# # " +proj=merc +lon_0=150 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
# cellStats(DEM, min)
# cellStats(DEM, max)
# cellStats(DEM, range)
# plot(DEM, main="Digital Elevation Model, Auckland and Waikato")
# #select on map
# cropbox1 <- drawExtent()
# #crop the raster, then plot the new cropped raster
# DEMcrop1 <- crop(DEM, cropbox1)
# #plot the cropped extent
# plot(DEMcrop1) 
# #You can also manually assign the extent coordinates to be used to crop a raster.  We'll need the extent defined as (`xmin`, `xmax`, `ymin` , `ymax`) to do this.  This is how we'd crop using a GIS shapefile (with a rectangular shape)
# #define the crop extent
# cropbox2 <-c(2810000,2835000,-4550000,-4530000)
# #crop the raster
# DEMcrop2 <- crop(DEM, cropbox2)
# #plot cropped DEM
# plot(DEMcrop2)
# 
# #try to change projection()
# newproj <- "+proj=longlat +datum=WGS84"
# pr2 <- projectRaster(DEMcrop1, crs=newproj)
# plot(pr2)

#make smaller shp files by simplify to reduce file size - optional
regions_sml <- gSimplify(regions.rg, 0.01, topologyPreserve=TRUE)
class(regions_sml)
regions_smlDF <- SpatialPolygonsDataFrame(regions_sml, data=as.data.frame(regions.rg))
writeOGR(regions_smlDF,".","regions_sml",driver="ESRI Shapefile") #makes shape files etc.
testregions.rg <- readOGR(".", "regions_sml") 
plot(testregions.rg,col="red",border="green")

plot(regions.rg,col="red",border="green")
plot(regions_sml)

#?readOGR

class(regions.rg)
#head(regions.rg)
names(regions.rg)
summary(regions.rg)

#save(regions.rg, file = "regionsave.RData")
#load(test, file = "regionsave.RData")


levels(regions.rg$NAME_1)
levels(regions.rg$NAME_2)
sum <- regions.rg@data
sum
write.csv(sum, "sum.csv")
#regionscut.rg <- regions.rg[regions.rg$NAME_1 != "Chatham Islands",]
#droplevels(regionscut.rg$NAME_1)
#levels(regionscut.rg$NAME_1)

# eg world.map@data = world.map@data[world.map@data$NAME != "Antarctica",]
# plot(world.map)

################### Simple map plot #####

# generate a simple map showing all three layers
#?plot
#plot(regionscut.rg, axes=TRUE, border="gray", xlim=c(160,180))
#plot(regionscut.rg, axes=TRUE, border="gray", ylim=c(-46,-36),xlim=c(165,180))
plot(regions.rg, axes=TRUE, border="gray", ylim=c(-47,-35),xlim=c(165,179))
#points(centroids.rg, pch=20, cex=0.8)
#lines(rivers.rg, col="blue", lwd=2.0)

# write out a new shapefile (including .prj component)
#writeOGR(counties.rg, ".", "counties-rgdal", driver="ESRI Shapefile")
#  regions.rg@data

library(ggplot2)

################### Read and make points ####

#make spatial data points from data frame
### All farms
setwd("C:/Users/nealm/Desktop/dairynz/Phys")
df <- read.csv('Farm locations all.csv',stringsAsFactors=FALSE)
head(df)
summary(df)
newdf <- df[!is.na(df$Latitude),] #gets rid of blanks
summary(newdf)
basicnewdf <- newdf              #this keeps a copy as data frame
coordinates(newdf) <- cbind(newdf$Longitude , newdf$Latitude)
proj4string(newdf) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
class(newdf)
head(newdf)

###OAD farms
setwd("C:/Users/nealm/Desktop/dairynz/NZmaps")
OAD <- read.csv('OADdata.csv',stringsAsFactors=FALSE)
head(OAD)
OAD2015 <- subset(OAD, SeasonYear=='2015')
head(OAD2015)
basicOAD2015 <- OAD2015         #this keeps a copy as data frame
coordinates(OAD2015) <- cbind(OAD2015$Longitude , OAD2015$Latitude)
proj4string(OAD2015) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
class(OAD2015)
head(OAD2015)


summary(newdf)
# old string from web, now changed to match regions
# proj4string :
#   [+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0]
summary(regions.rg)
# proj4string :
#   [+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0]
head(regions.rg)

summary(regions.rg)
#  was   "+proj=tmerc +lat_0=0 +lon_0=173 +k=0.9996 +x_0=1600000 +y_0=10000000 +ellps=GRS80 +units=m +no_defs"
crs.default <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")  # googles default?
regions.rg <- spTransform(regions.rg, crs.default)  # spTransform makes the projection
# was   "+proj=tmerc +lat_0=0 +lon_0=173 +k=0.9996 +x_0=1600000 +y_0=10000000 +ellps=GRS80 +units=m +no_defs"
summary(regions.rg)


#################Intersect points and polygons #####
#option 1
#from web, but doesn't work if any points outside polygons
 library(spatialEco)
 new_shape <- point.in.poly(newdf, regions.rg)

#option 2 
#intersects points and polygons - works even if points outside polygons
##all farms
library(sp)
pt.in.poly <- sp::over(newdf, regions.rg, fn = NULL) #do the join
class(pt.in.poly)
class(newdf)
class(basicnewdf)
together <- cbind(basicnewdf,pt.in.poly)
head(together)
## OAD farms
library(sp)
pt.in.poly.OAD <- sp::over(OAD2015, regions.rg, fn = NULL) #do the join
class(pt.in.poly.OAD)
class(OAD2015)
class(basicOAD2015)
togetherOAD <- cbind(basicOAD2015,pt.in.poly.OAD)
head(togetherOAD)

############## summarise by data table ####
library(data.table)
dt <- data.table(together)
head(dt)
#dt[,list(mean=mean(age),sd=sd(age)),by=group] #example from web
#using 16 regions
#dt[,list(count=length(SupplyID)),by=NAME_1]
#head(together[together$NAME_1==NA,])
#usingTA's

#tabulate for All farms
Proc2015Summary <- dt[,list(count=length(SupplyID)),by=TA_NAME]                  #change if using postcodes
#Proc2015Summary <- dt[,list(count=length(SupplyID)),by=POSTCODE]
Proc2015Summary
class(Proc2015Summary)
#head(together[together$TA_NAME==NA,])
write.csv(Proc2015Summary, file="Proc2015Summary.csv")
Total <- (as.data.frame(Proc2015Summary)[,2])
sum(Total)

#tabulate for OAD
do <- data.table(togetherOAD)
head(do)
OAD2015Summary <- do[,list(count=length(Index)),by=TA_NAME]                  #change if using postcodes
#Proc2015Summary <- dt[,list(count=length(SupplyID)),by=POSTCODE]
OAD2015Summary
class(OAD2015Summary)
#head(together[together$TA_NAME==NA,])
write.csv(OAD2015Summary, file="OAD2015Summary.csv")

ProcOAD <- merge(Proc2015Summary,OAD2015Summary,by="TA_NAME",all=TRUE)
names(ProcOAD)[3] <- "OADfreq"
names(ProcOAD)[2] <- "Allfreq"
ProcOAD
sum(as.data.frame(ProcOAD)[2])
ProcOAD$proportion <- ProcOAD$OADfreq/ProcOAD$Allfreq

write.csv(ProcOAD, file="OADDistrict.csv")

class(ProcOAD$OADfreq)
class(ProcOAD$Allfreq)
class(ProcOAD$proportion)
class(ProcOAD)
summary(ProcOAD)
ProcOAD[is.na(ProcOAD)] <- 0
Totals <- colSums(as.data.frame(ProcOAD)[,2:3])
Totals[2]/Totals[1]

###########Dairy stats numbers for comparison #####

setwd("C:/Users/nealm/Desktop/dairynz/Phys")
ds <- read.csv('NZDS.csv',stringsAsFactors=FALSE)
head(ds)
dst <- data.table(ds)
head(dst)
DS2013summary <- dst[Year==2013,.(Total.herds,Total.cows,Total.effective.hectares,Total.milksolids),by=District]
DS2013summary
write.csv(DS2013summary, file="DS2013summary.csv")

###########Dairy Base numbers for comparison ####

setwd("C:/Users/nealm/Desktop/dairynz/Phys")
db <- read.csv('1213csv.csv',stringsAsFactors=FALSE)
head(db)
dbt <- data.table(db)
head(dbt)
#stulevel_agg_3 <- as.data.frame(stulevel[, j=list(mean(ability, na.rm = TRUE),mean(attday, na.rm = TRUE)),by = grade])
#DB2013summary <- as.data.frame(column5, j=list(mean(Peak.cows.milked),mean(Effective.Area...Milking.Platform..Ha.),mean(Milksolids.for.Production.year),count(Peak.cows.milked)),)
#DB2013summary <- dbt[,j=list(mean(Peak.cows.milked),mean(Effective.Area...Milking.Platform..Ha.),mean(Milksolids.for.Production.year),count(Peak.cows.milked)),by=Column5]
#DB2013summary
#write.csv(DB2013summary, file="DB2013summary.csv")



############ Centroids for regions ####
#create R centroids
#from http://gis.stackexchange.com/questions/43543/how-to-calculate-polygon-centroids-in-r-for-non-contiguous-shapes
class(regions.rg)
trueCentroids = gCentroid(regions.rg,byid=TRUE)
trueCentroids
plot(regions.rg, axes=TRUE, border="gray", ylim=c(-47,-35),xlim=c(165,179))
#plot(regions.rg, axes=TRUE, border="gray", ylim=c(-44,-42),xlim=c(170,175))
#plot(regions.rg[c(7,8,64,66),], axes=TRUE, border="red", ylim=c(-40,-35),xlim=c(175,179))
points(trueCentroids,pch=2)
points(newdf,pch=4, cex=.1)
points(trueCentroids[24],pch=2)


############### Make a ggplot cloropleth ####
#eg from web
library(ggplot2)

#?fortify
regfort <- fortify(regions.rg)
head(regfort)
regfortd <- cbind(regfort, regions.rg@data[regfort$id,])
head(regfortd)
class(regfortd)

regfortd$herd.count <-Proc2015Summary$count[match(regfortd$TA_NAME,Proc2015Summary$TA_NAME)]    #Change if using postcodes
#regfortd$herd.count <-Proc2015Summary$count[match(regfortd$POSTCODE,Proc2015Summary$POSTCODE)]
head(regfortd)
class(regfortd$sum)

# ggplot(c10d, aes(long, lat, group = group)) +
#   geom_polygon(aes(fill = factor(P0010001)), colour = alpha("white", 1/2), size = 0.2) +
#   scale_fill_brewer(pal = "PuRd")

ggplot(regfortd, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = regfortd$herd.count))

##################join dairy stats to spatial regions or vice versa #####
#doesn't work as intended not consitently many to one, and not reversible.

setwd("C:/Users/nealm/Desktop/dairynz/Phys")
linktoDS <- read.csv('SpatialDistrictsToDairyStats.csv',stringsAsFactors=FALSE)
head(linktoDS)
summary(linktoDS)
linktoDS[is.na(linktoDS)] <- 0
head(linktoDS)
colnames(linktoDS)
dim(linktoDS)
length(linktoDS)
linkDSmat <- as.matrix(linktoDS[,2:length(linktoDS)])
#?as.matrix
head(linkDSmat)
class(linkDSmat)
dim(linkDSmat)

linktoReg <- read.csv('DairyStatsToSpatialDistricts.csv',stringsAsFactors=FALSE)
head(linktoReg)
summary(linktoReg)
linktoReg[is.na(linktoReg)] <- 0
head(linktoReg)
colnames(linktoReg)
dim(linktoReg)
length(linktoReg)
linkRegmat <- as.matrix(linktoReg[,2:length(linktoReg)])
#?as.matrix
head(linkRegmat)
tail(linkRegmat)
class(linkRegmat)
dim(linkRegmat)




#### Processor data
head(Proc2015Summary)
Proc2015SummaryO <- Proc2015Summary[order(Proc2015Summary$TA_NAME),]
head(Proc2015SummaryO)
length(Proc2015SummaryO$count)
head(Proc2015SummaryO$count)

#### Dairy stats data
head(DS2013summary)
DS2013summaryO <- DS2013summary[order(DS2013summary$District),]
head(DS2013summaryO)
length(DS2013summaryO$Total.herds)
head(DS2013summaryO$Total.herds)
class(DS2013summaryO)
testDT <- data.frame(DS2013summaryO)
class(testDT)
testcut <- as.matrix(testDT[,2:5])
dim(testcut)
class(testcut)
#test <- data.frame(t(linkmat) %*% DS2013summaryO$Total.herds)
test <- data.frame(t(linkRegmat) %*% testcut)
test
head(test)
class(test)
dim(test)
rownames(test) <- Proc2015SummaryO$TA_NAME
head(test)
test$region <- Proc2015SummaryO$TA_NAME
head(test)


####### Graph dairystats data ####
regfortd$DSHerds <- test$Total.herds[match(regfortd$TA_NAME,test$region)]
regfortd$DSCows  <- test$Total.cows[match(regfortd$TA_NAME,test$region)]
regfortd$DSEff   <- test$Total.effective.hectares[match(regfortd$TA_NAME,test$region)]
regfortd$DSMS    <- test$Total.milksolids[match(regfortd$TA_NAME,test$region)]

regfortd$DSMSperCow    <-  regfortd$DSMS / regfortd$DSCows
regfortd$DSMSperHa    <-  regfortd$DSMS / regfortd$DSEff
regfortd$DSMSperHerd    <-  regfortd$DSMS / regfortd$DSHerds
regfortd$DSCowsperHa    <-  regfortd$DSCows / regfortd$DSEff
regfortd$DSCowsperHerd    <-  regfortd$DSCows / regfortd$DSHerds
regfortd$DSHaperHerd    <-  regfortd$DSEff / regfortd$DSHerds

summary(regfortd$DSMSperEff)
head(regfortd)

# ggplot(c10d, aes(long, lat, group = group)) +
#   geom_polygon(aes(fill = factor(P0010001)), colour = alpha("white", 1/2), size = 0.2) +
#   scale_fill_brewer(pal = "PuRd")

## Proc + base 4
ggplot(regfortd, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = regfortd$herd.count))

ggplot(regfortd, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = regfortd$DSHerds))

ggplot(regfortd, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = regfortd$DSCows))

ggplot(regfortd, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = regfortd$DSEff))

ggplot(regfortd, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = regfortd$DSMS))

## 6 ratios
ggplot(regfortd, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = regfortd$DSMSperCow)) +
  ggtitle("District MS per Cow") +
  theme(legend.title=element_blank()) +
  theme_void()

ggplot(regfortd, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = regfortd$DSMSperHa))  +
  ggtitle("District MS per Eff Ha") +
  theme(legend.title=element_blank()) +
  theme_void()

ggplot(regfortd, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = regfortd$DSMSperHerd)) +
  ggtitle("District MS per Herd") +
  theme(legend.title=element_blank()) +
  theme_void()

ggplot(regfortd, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = regfortd$DSCowsperHa)) +
  ggtitle("District Cows per Eff Ha") +
  theme(legend.title=element_blank()) +
  theme_void()



ggplot(regfortd, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = regfortd$DSCowsperHerd)) +
  ggtitle("District Cows per Herd") +
  theme(legend.title=element_blank()) +
  theme_void()


ggplot(regfortd, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = regfortd$DSHaperHerd)) +
  ggtitle("District Eff Ha per Farm") +
  theme(legend.title=element_blank()) +
  theme_void()




###### Create grid data() ####
#option 1
# require(raster)
# require(sp)
# 
# # Create example data
# r <- raster(ncol=500, nrow=500)
# r[] <- runif(ncell(r))
# pts <- sampleRandom(r, 100, sp=TRUE)  
# 
# # Add a grouping value to points 
# pts@data <- data.frame(ID=rownames(pts@data), group=c( rep(1,25),rep(2,25),
#                                                        rep(3,25),rep(4,25)) )          
# 
# # Extract raster values and add to @data slot dataframe. Note, the "cells" 
# #   attribute indicates the cell index in the raster. 
# pts@data <- data.frame(pts@data, extract(r, pts, cellnumbers=TRUE))
# head(pts@data)
# 
# # Use tapply to cal group means  
# tapply(pts@data$layer, pts@data$group, FUN=mean)

# #option 2
# ##make some data
# longi <- runif(100,0,10)
# lati <- runif(100,0,10)
# value <- runif(500,20,30)
# ?runif
# ##put in data frame then change to spatial data frame
# df <- data.frame("lon"=longi,"lat"=lati,"val"=value)
# coordinates(df) <- c("lon","lat")
# proj4string(df) <- CRS("+proj=longlat")
# 
# ##create a grid that bounds the data
# grd <- GridTopology(cellcentre.offset=bbox(df)[,1],
#                     cellsize=c(1,1),cells.dim=c(11,21))
# sg <- SpatialGrid(grd)
# 
# plot(df)
# plot(sg)
# 
# ##convert the grid into a polygon##
# polys <- as.SpatialPolygons.GridTopology(grd) 
# proj4string(polys) <- CRS("+proj=longlat")
# 
# plot(polys)
# 
# ##can now use the function over to select the correct points and average them
# results <- rep(0,length(polys))
# i=1
# for(i in 1:length(polys))
# {
#    #results[i] = mean(df$val[which(!is.na(over(x=df,y=polys[i])))]) #mean of values in grid
#    results[i] = length(df$val[which(!is.na(over(x=df,y=polys[i])))]) #count of values in grid
# }
# results

#### my test to create a grid ####
grd <- GridTopology(cellcentre.offset=bbox(newdf)[,1],
                    cellsize=c(0.24,0.2),cells.dim=c(46,61))   #This is approximately 20km*20km
#nb: 1 degree of latitude is about 111 km (so for exactly 20km grid, should be 20/111=~0.18)
#nb: 1 degree of longitude is about 91km at the top (-35deg), and about 76 at the bottom (-46deg) 
# so should be between 20/91=~0.22 and 20/76=~0.26

sg <- SpatialGrid(grd)
grd
plot(newdf)
class(newdf)
plot(sg)
summary(newdf)

plot(OAD2015)
class(OAD2015)
head(OAD2015)

##convert the grid into a polygon##
polys <- as.SpatialPolygons.GridTopology(grd) 
proj4string(polys) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
summary(polys)
plot(polys)
head(polys)

############Centroids for grid ####
#create R centroids
#from http://gis.stackexchange.com/questions/43543/how-to-calculate-polygon-centroids-in-r-for-non-contiguous-shapes
class(polys)
trueCentroidsGrid = gCentroid(polys,byid=TRUE)
trueCentroidsGrid
head(trueCentroidsGrid)
plot(polys, axes=TRUE, border="gray", ylim=c(-47,-35),xlim=c(165,179))
#plot(regions.rg, axes=TRUE, border="gray", ylim=c(-44,-42),xlim=c(170,175))
#plot(regions.rg[c(7,8,64,66),], axes=TRUE, border="red", ylim=c(-40,-35),xlim=c(175,179))
points(trueCentroidsGrid,pch=1)
points(newdf,pch=4, cex=.1)
#points(trueCentroidsGrid[24],pch=2)
dfCentroidsGrid <- as.data.frame(trueCentroidsGrid)
head(dfCentroidsGrid)
class(dfCentroidsGrid)
dfCentroidsGrid$id <- row.names(dfCentroidsGrid)

### Put ALL farms in grid squares
##can now use the function over to select the correct points and average them

results <- rep(0,length(polys))
i=1
for(i in 1:length(polys))
{
  #results[i] = mean(newdf$SupplyID[which(!is.na(over(x=newdf,y=polys[i])))]) #mean of values in grid
  results[i] = length(newdf$SupplyID[which(!is.na(over(x=newdf,y=polys[i])))]) #count of values in grid
}
head(results)
summary(results)
class(results)

#now prepare data for ggplot
resultsdf <- data.frame(results)
class(resultsdf)

polysfort <- fortify(polys)
class(polysfort)
head(polysfort)
class(polysfort$group)

test2 <- levels(polysfort$group)
head(test2)
class(test2)
length(test2)

resultsdf$group <- test2
head(resultsdf)

polysfort$count <- resultsdf$results[match(polysfort$group,resultsdf$group)]
head(polysfort)

### REPEAT WITH OAD farms
##can now use the function over to select the correct points and average them

resultsOAD <- rep(0,length(polys))
i=1
for(i in 1:length(polys))
{
  #results[i] = mean(newdf$SupplyID[which(!is.na(over(x=newdf,y=polys[i])))]) #mean of values in grid
  resultsOAD[i] = length(OAD2015$Index[which(!is.na(over(x=OAD2015,y=polys[i])))]) #count of values in grid
}
head(resultsOAD)
summary(resultsOAD)
class(resultsOAD)

resultsdfOAD <- data.frame(resultsOAD)
class(resultsdfOAD)

resultsdfOAD$group <- test2
head(resultsdfOAD)

polysfort$countOAD <- resultsdfOAD$results[match(polysfort$group,resultsdfOAD$group)]
head(polysfort)
class(polysfort)

polysfort$propOAD <- polysfort$countOAD/polysfort$count/0.7
summary(polysfort)
polysfort$propOAD[polysfort$propOAD < 0.01] <- NA
polysfort$propOAD[polysfort$countOAD > polysfort$count ] <- 1
polysfort$propOAD[polysfort$count <= 1] <- NA
summary(polysfort)

polysfort$colourgrid[polysfort$count >=3] <- 2
polysfort$colourgrid[polysfort$count <3] <- 1

polysfort$countOADcensor <- polysfort$countOAD
polysfort$countOADcensor[polysfort$countOAD < 1 ] <- NA

polysfort$countcensor <- polysfort$count
polysfort$countcensor[polysfort$count < 1 ] <- NA

polysfortOAD <- merge(polysfort,dfCentroidsGrid,by="id",all=TRUE)
summary(polysfortOAD)


m <- ggplot(polysfortOAD, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = count),alpha = 0.9) +
  ggtitle("Farms per grid block") +
  theme(legend.title=element_blank()) +
  theme_void()
m

mOAD <- ggplot(polysfortOAD, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = count),alpha = 0.9) +
  geom_point(aes(x,y,size = propOAD/3),col=2) +
  ggtitle("OADFarms per grid block") +
  theme(legend.title=element_blank()) +
  theme_void()
mOAD


#geom_point(mapping = NULL, data = NULL, stat = "identity", position = "identity", ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)

##### Graph with underlying Google map ####
#note, ggmap and Rgooglemaps are different packages that do similar things
#GetMap belongs to Rgooglemaps
#get_map belongs to ggmap
library(ggmap)
library(RgoogleMaps)



#Nelson, works well for national
Nelson = c(173.3,-41.3)
WaitakiCentre = c(170.3,-44.6)
WaikatoCentre = c(175.6,-37.6)
#Matangi/Hamilton

#National
zoomfactor <- 5
#Local
#zoomfactor <- 9

# datapt = data.frame(x = 173.2839653, y = -41.2706319) 
# gmap + geom_point(data = datapt, aes(x = x, y = y,), colour = "red", size = 5)

# map <- GetMap(center=Hamilton, zoom=zoomfactor,GRAYSCALE=TRUE) #Rgooglemaps
# map <- get_map(location=Nelson, zoom=zoomfactor)              #ggmaps
# gmap <- ggmap(map)
# gmap
# eg to put a point on a gmap
# datapt = data.frame(x = 173.2839653, y = -41.2706319) 
# gmap + geom_point(data = datapt, aes(x = x, y = y), colour = "red", size = 5)
# gmap + geom_polygon(data = polysfort, aes(x = long, y = lat), alpha = 0.5)

################ alt eg that works for google map underlying ####
#from http://www.r-bloggers.com/throw-some-throw-some-stats-on-that-mappart-1/
# and http://www.r-bloggers.com/shapefile-polygons-plotted-on-google-maps-using-ggmap-in-r-throw-some-throw-some-stats-on-that-mappart-2/
#CenterOfMap <- geocode("Nelson, NZ")

mapoptions <- c("terrain", "terrain-background", "satellite","roadmap", "hybrid", "toner", "watercolor", "terrain-labels", "terrain-lines", 
                "toner-2010", "toner-2011", "toner-background", "toner-hybrid","toner-labels", "toner-lines", "toner-lite")
#NZ <- get_map(location=WaikatoCentre,zoom = zoomfactor, maptype = mapoptions[1], source = "google", color = "bw")
NZ <- get_map(location=Nelson,zoom = zoomfactor, maptype = mapoptions[1], source = "google", color = "bw")
#install.packages("ggmap", type = "source")
NZMap <- ggmap(NZ)
NZMap 
?get_map

Waitaki <- get_map(location=WaitakiCentre,zoom = 9, maptype = mapoptions[1], source = "google", color = "bw")
WaitakiMap <- ggmap(Waitaki)
WaitakiMap

library(RColorBrewer)
# par(mfrow=c(2,2))
# display.brewer.all()
#tcols <- colorRampPalette(brewer.pal(9,'Blues'))(20)
tcols <- colorRampPalette(brewer.pal(9,'YlGnBu'))(20)
# rand.data <- replicate(8,rnorm(100,100,sd=1.5))
# boxplot(rand.data,col=brewer.pal(9,'Blues'))
mycol <- rgb(255, 255, 255, max = 255, alpha = 0, names = "transparent")
mycol
head(tcols)
tcols[1]
tcols[1] <- mycol
tcols

x1 <- 1:20
y1 <- 5
z1 <- 1:20

myplotdata <- data.frame(x1,y1,z1,tcols)
myplotdata
class(myplotdata$x1)
myplotdata$x1 <- as.numeric(myplotdata$x1)
class(myplotdata$x1)
myplotdata$y1 <- as.numeric(myplotdata$y1)
class(myplotdata$y1)
class(round((polysfort$count+1)^0.5,0))

#Hack to make scale, made trickier by the square root to scale colours ####
hacklegend <- ggplot(myplotdata, aes(x=x1^2, y=y1)) + 
  geom_bar(fill=tcols[x1], stat="identity",  colour=mycol, width=(1.25*y1)^2 ) +
  coord_flip() +
  theme_classic() +
  theme(axis.line=element_blank(), axis.text.x=element_blank(),axis.title.x=element_blank(),axis.ticks.x=element_blank(),axis.title.y=element_blank()) +
  theme(axis.text.y=element_text(size = rel(1.5)))
#  ggtitle("Farms")
hacklegend
#ggsave("test.png",width=5,height=5)

max(polysfort$count)
NZMap <- ggmap(NZ, legend="topright")
MyMap <- NZMap + 
  geom_polygon(aes(x=long, y=lat, group=group), fill=tcols[round((polysfortOAD$count+1)^0.5,0)], size=.2,color=mycol, data=polysfortOAD) +
  geom_point(aes(x,y,size = propOAD,col=propOAD), data=polysfortOAD) +
  ggtitle("Farms per grid square") +
  theme(plot.title=element_text(size=20)) +
  ylab("Latitude") +
  xlab("Longitude")
MyMap

MyMap4 <- NZMap + 
  geom_polygon(aes(x=long, y=lat, group=group), fill=tcols[polysfortOAD$colourgrid], size=.2,color=mycol, data=polysfortOAD) +
  geom_point(aes(x,y,size = propOAD,color = propOAD), data=polysfortOAD) +
  scale_size(range = c(0, 3)) +
  labs(size = "Proportion FS-OAD", color = "Proportion FS-OAD") +
  ggtitle("Farms per grid square") +
  theme(plot.title=element_text(size=20)) +
  ylab("Latitude") +
  xlab("Longitude")
MyMap4
ggsave("FS-OAD Proportion.png", width=14,height=10)

MyMap5 <- NZMap + 
  geom_polygon(aes(x=long, y=lat, group=group), fill=tcols[polysfortOAD$colourgrid], size=.2,color=mycol, data=polysfortOAD) +
  geom_point(aes(x,y,size = countOADcensor,color = countOADcensor), data=polysfortOAD) +
  scale_size(range = c(0, 3)) +
  labs(size = "No. FS-OAD", color = "No. FS-OAD") +
  ggtitle("Farms per grid square") +
  theme(plot.title=element_text(size=20)) +
  ylab("Latitude") +
  xlab("Longitude")
MyMap5
ggsave("FS-OAD Number.png", width=14,height=10)

dt<-data.frame(country=letters[1:20],val=rnorm(20),siz=rnorm(20))
qplot(x=country,y=val,data=dt,geom="point",size=siz)
p <- qplot(x=country,y=val,data=dt,geom="point",size=siz)
p + scale_size_continuous(range = c(3,8))


tcols

?max
p <- ggplot(mtcars, aes(wt, mpg))
p + geom_point(aes(colour = cyl)) + scale_colour_gradient(high = "red")

newdfADF <- data.frame(newdf)
head(newdfADF)

# MyMap <- WaitakiMap + 
#   geom_polygon(aes(x=long, y=lat, group=group), fill="blue", alpha=0.35, size=.2,color=mycol, data=fortify(regions.rg)) +
#   geom_point(aes(x=Longitude, y=Latitude), fill="blue", size=.3,color="red", data=newdfADF) +  
#   ggtitle("Waitaki Farms") +
#   theme(plot.title=element_text(size=20)) +
#   ylab("Latitude") +
#   xlab("Longitude")
# MyMap
# ggsave("Waitaki.png", width=7,height=5)

############## Put map and hack legend into pdf ####
library(gridExtra)
grid.arrange(MyMap, hacklegend, ncol=2, widths=c(2, 0.5))
pdf("Grid map of farms.pdf")
grid.arrange(MyMap, hacklegend, ncol=2, widths=c(2, 0.5))
dev.off()

############## Alt version of map and hack legend ####

# MyMap2 <- MyMap +
#   geom_point(data=df1213, aes(x=long, y=lat, col="red"),size=0.5 )  + 
#   theme(legend.text = element_text(colour="black", size = 12, face = "plain")) + 
#   theme(legend.position=c(.75, .25)) + 
#   scale_color_manual(labels = c("DB12/13 Level 3"), values = c("red")) +  
#   theme(legend.title=element_blank())
# MyMap2  
# 
# 
# library(gridExtra)
# grid.arrange(MyMap2, hacklegend, ncol=2, widths=c(2, 0.5))
# pdf("Grid map of farms DB.pdf")
# grid.arrange(MyMap2, hacklegend, ncol=2, widths=c(2, 0.5))
# dev.off()


###########   Dairy Base numbers for pasture and crop eaten distributions ####

setwd("C:/Users/nealm/Desktop/dairynz/Phys")
db1213 <- read.csv('1213csv.csv',stringsAsFactors=FALSE)
head(db1213)

df1213 <- db1213[!is.na(db1213$lat),]
summary(df1213)
class(df1213)
df1213sp <- df1213
coordinates(df1213sp) <- cbind(df1213$long , df1213$lat)          #lat and long were wrong way around
proj4string(df1213sp) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
class(df1213sp)
summary(df1213sp)

plot(regions.rg, axes=TRUE, border="gray", ylim=c(-47,-35),xlim=c(165,179))
points(df1213sp,pch=4, cex=.1)
plot(df1213sp)

DBFarms <- NZMap + geom_point(data=df1213, aes(x=long, y=lat,colour=round(df1213$Pasture.and.Crop.eaten.t.DM.ha,0)),size=5 ) + 
  #scale_fill_grey(start=0,end=20) +
  scale_colour_gradient2(low="red",high="darkgreen",mid="yellow",midpoint=10) +
  labs(colour = "Pasture&Crop")
DBFarms

DBFarmsNadj <- NZMap + geom_point(data=df1213, aes(x=long, y=lat,colour=round(df1213$PastCropNAdj,0)),size=5 ) + 
  #scale_fill_grey(start=0,end=20) +
  scale_colour_gradient2(low="red",high="darkgreen",mid="yellow",midpoint=10) +
  labs(colour = "Pasture&Crop N fert adjusted")
DBFarmsNadj

mean(df1213$Pasture.and.Crop.eaten.t.DM.ha)
mean(df1213$Nitrogen.applied.for.year..L2.)
mean(df1213$PastCropNAdj)


head(df1213)
t <- ggplot(df1213, aes(x=Pasture.and.Crop.eaten.t.DM.ha, y=Nitrogen.applied.for.year..L2.,col=Column4)) + geom_point()
t + facet_grid(Column4 ~ .)

summary(df1213$Column4)

class(df1213)
head(df1213)
df1213$PastCropNAdj <- df1213$Pasture.and.Crop.eaten.t.DM.ha - ((12/1000) * df1213$Nitrogen.applied.for.year..L2.)


db <- data.table(df1213)
head(db)

dbSummary <- db[,list(count=length(Concat1)),by=Column4]
dbSummary

df1213censor <- df1213[which(df1213$Column4 != "West Coast - Tasman"),]
df1213censor2 <- df1213censor[which(df1213censor$Column4 != "Taranaki"),]
#| df1213$Column4 != "Taranaki"

dbc <- data.table(df1213censor2)
head(dbc)
dbcSummary <- dbc[,list(count=length(Concat1)),by=Column4]
dbcSummary

past <- ggplot(df1213censor2, aes(x=Pasture.and.Crop.eaten.t.DM.ha)) + geom_density(aes(group=Column4, colour=Column4, fill=Column4), alpha=0.3)
past + ggtitle('Pasture and Crop')

df1213censorSI <- df1213censor2[which(df1213censor2$Column4 == "Otago - Southland" | df1213censor2$Column4 == "Marlborough-Canterbury"),]
dbSI <- data.table(df1213censorSI)
head(dbSI)
dbSISummary <- dbSI[,list(count=length(Concat1)),by=Column4]
dbSISummary

past <- ggplot(df1213censorSI, aes(x=Pasture.and.Crop.eaten.t.DM.ha)) + geom_density(aes(group=Column4, colour=Column4, fill=Column4), alpha=0.3)
past + ggtitle('Pasture and Crop')

df1213censorNI <- df1213censor2[which(df1213censor2$Column4 == "Bay of Plenty" | df1213censor2$Column4 == "Lower North Island" | df1213censor2$Column4 == "Waikato" | df1213censor2$Column4 == "Northland"),]
dbNI <- data.table(df1213censorNI)
head(dbNI)
dbNISummary <- dbNI[,list(count=length(Concat1)),by=Column4]
dbNISummary

past <- ggplot(df1213censorNI, aes(x=Pasture.and.Crop.eaten.t.DM.ha)) + geom_density(aes(group=Column4, colour=Column4, fill=Column4), alpha=0.3)
past + ggtitle('Pasture and Crop')


dbc$Island[df1213censor2$Column4 == "Otago - Southland" | df1213censor2$Column4 == "Marlborough-Canterbury"] <- "South Island"
dbc$Island[df1213censor2$Column4 == "Bay of Plenty" | df1213censor2$Column4 == "Lower North Island" | df1213censor2$Column4 == "Waikato" | df1213censor2$Column4 == "Northland"] <- "North Island"

names(dbc)
class(dbc)
dbcdf <- as.data.frame(dbc)
class(dbcdf)
names(dbcdf)[18] <- "Region"


past <- ggplot(dbcdf, aes(x=Pasture.and.Crop.eaten.t.DM.ha)) + geom_density(aes(group=Region, colour=Region, fill=Region), alpha=0.3)
past

pastNAdj <- ggplot(dbcdf, aes(x=PastCropNAdj)) + geom_density(aes(group=Region, colour=Region, fill=Region), alpha=0.3)
pastNAdj


past + facet_grid(Island ~ .) + 
  theme(legend.title=element_blank()) +
  #ggtitle("Distributions of Pasture and Crop Eaten by Region, 12/13") +
  xlab("Pasture and Crop Eaten (tDM/ha)") +
  ylab("Density") +
  xlim(5,19) +
  ylim(0,0.275) +
  scale_fill_discrete(limits=c("Northland", "Waikato", "Bay of Plenty", "Lower North Island", "Marlborough-Canterbury", "Otago - Southland" )) +
  scale_color_discrete(limits=c("Northland", "Waikato", "Bay of Plenty", "Lower North Island", "Marlborough-Canterbury", "Otago - Southland" ))
ggsave("Pasture eaten 12-13.png", width=7,height=5)

pastNAdj + facet_grid(Island ~ .) + 
  theme(legend.title=element_blank()) +
  ggtitle("Distributions of Pasture and Crop Eaten by Region, N Adj, 12/13") +
  xlab("Pasture and Crop Eaten N Adj (tDM/ha)") +
  xlim(5,19) +
  ylim(0,0.275) +
  scale_fill_discrete(limits=c("Northland", "Waikato", "Bay of Plenty", "Lower North Island", "Marlborough-Canterbury", "Otago - Southland" )) +
  scale_color_discrete(limits=c("Northland", "Waikato", "Bay of Plenty", "Lower North Island", "Marlborough-Canterbury", "Otago - Southland" ))
ggsave("Pasture eaten 12-13 NAdj.png", width=7,height=5)


### Dairy stats plots by region ####
#newdata <- mydata[ which(mydata$gender=='F'      & mydata$age > 65), ]

dstNorthland <- dst[dst$District=="Far North ",]
head(dstNorthland)

head(dst)
dim(dst)
head(dstAgg)
length(colnames(dst))
dst$Year
dst <- as.data.frame(dst)
dstAgg <- aggregate(dst[,c(5:length(colnames(dst)))], by=list(as.numeric(dst$Year),factor(dst$LIC.region)),FUN = sum)
head(dstAgg)
colnames(dstAgg)[1:2] <- c("Year","LIC.region")
head(dstAgg)
class(dstAgg$Year)
dstAgg$Year <- as.numeric(dstAgg$Year)
class(dstAgg$Year)  
dstAgg$mspercow <- dstAgg$Total.milksolids/dstAgg$Total.cows
dstAgg$cowsperha <- dstAgg$Total.cows/dstAgg$Total.effective.hectares
dstAgg$msperha <- dstAgg$Total.milksolids/dstAgg$Total.effective.hectares
summary(dstAgg)

head(dstAgg)

#  All districts
#ggplot(data = dst, aes(x=Year,y=Total.herds)) +
#  geom_point(data = dst, aes(x=Year,y=Total.herds,col=factor(District),shape=factor(LIC.region))) 

######  LIC.regions
#herds
ggplot(data = dstAgg, aes(x=Year,y=Total.herds)) +
  geom_line(data = dstAgg, aes(x=Year,y=log(Total.herds,10),col=factor(LIC.region))) 
ggsave("herds.png", width=7,height=5)

#cows
ggplot(data = dstAgg, aes(x=Year,y=Total.cows)) +
  geom_line(data = dstAgg, aes(x=Year,y=log(Total.cows,10),col=factor(LIC.region))) 
ggsave("cows.png", width=7,height=5)

#ha
ggplot(data = dstAgg, aes(x=Year,y=Total.effective.hectares)) +
  geom_line(data = dstAgg, aes(x=Year,y=log(Total.effective.hectares,10),col=factor(LIC.region))) 
ggsave("ha.png", width=7,height=5)

#ms
ggplot(data = dstAgg, aes(x=Year,y=Total.milksolids)) +
  geom_line(data = dstAgg, aes(x=Year,y=log(Total.milksolids,10),col=factor(LIC.region))) 
ggsave("ms.png", width=7,height=5)

#mspercow
ggplot(data = dstAgg, aes(x=Year,y=mspercow)) +
  geom_line(data = dstAgg, aes(x=Year,y=log(mspercow,10),col=factor(LIC.region))) 
ggsave("mspercow.png", width=7,height=5)

#cowsperha
ggplot(data = dstAgg, aes(x=Year,y=cowsperha)) +
  geom_line(data = dstAgg, aes(x=Year,y=log(cowsperha,10),col=factor(LIC.region))) 
ggsave("cowsperha.png", width=7,height=5)

#msperha
ggplot(data = dstAgg, aes(x=Year,y=msperha)) +
  geom_line(data = dstAgg, aes(x=Year,y=log(msperha,10),col=factor(LIC.region))) 
ggsave("msperha.png", width=7,height=5)

## make labels line up with lines
library(grid)
library(gtable)
library(ggplot2)
library(plyr)

p <- ggplot(data = dstAgg, aes(x=Year,y=msperha)) +
  geom_line(data = dstAgg, aes(x=Year,y=msperha,col=factor(LIC.region))) +
#  scale_x_continuous(expand = c(0,0), limits = c(1987, 2013)) +
  scale_x_continuous(expand = c(0,0)) +
  theme(legend.position="left", plot.margin=unit(c(1,0,1,0),"line")) +
  xlab("Production Year Starting") +
  ylab("Milksolids per hectare") +
  scale_colour_discrete(name  ="Region (LIC)")
p

# set.seed(1)
# d <- data.frame(x=rep(1:10, 5),
#                 y=rnorm(50),
#                 g = gl(5,10))
# d
# 
# # example plot
# p <- ggplot(d, aes(x,y,colour=g)) +
#   geom_line() +
#   scale_x_continuous(expand=c(0,0))+
#   theme(legend.position="top",
#         plot.margin=unit(c(1,0,0,0),"line"))

# dummy data for the legend plot
# built with the same y axis (same limits, same expand factor)
d2 <- ddply(dstAgg, "LIC.region", summarise, x=0, y=msperha[length(msperha)])
d2
d2$lab <- paste0("  ",d2$LIC.region)
head(dstAgg)
dstAgg2 <- dstAgg
names(dstAgg2)[1] <- "x"
names(dstAgg2)[9] <- "y"

plegend <- ggplot(dstAgg2, aes(x, y, colour=LIC.region)) +
  geom_blank() +
  geom_segment(data=d2, aes(x=2, xend=0, y=y, yend=y), 
               arrow=arrow(length=unit(2,"mm"), type="closed")) +
  geom_text(data=d2, aes(x=2.5,label=lab), hjust=0) +
  scale_x_discrete(expand=c(0,0)) +
  guides(colour="none")+
  theme_minimal() + theme(line=element_blank(),
                          text=element_blank(),
                          panel.background=element_rect(fill="white", linetype=8, colour = "white"))
plegend

# extract the panel only, we don't need the rest
gl <- gtable_filter(ggplotGrob(plegend), "panel")

# add a cell next to the main plot panel, and insert gl there
g <- ggplotGrob(p)
index <- subset(g$layout, name == "panel")
g <- gtable_add_cols(g, unit(1, "strwidth", "line # 1") + unit(2.5, "cm"))
g <- gtable_add_grob(g, gl, t = index$t, l=ncol(g), 
                     b=index$b, r=ncol(g))
grid.newpage()
grid.draw(g)


#### Interactive map with leaflet ####
head(newdfADF)

library(leaflet)
library(png)
# Plot a default web map (brackets display the result)
(m <- leaflet() %>% addTiles())
#img <- readPNG("~/repos/Creating-maps-in-R/figure//shiny_world.png")
#grid.raster(img)

m %>% setView(lng = Nelson[1], lat = Nelson[2], zoom = 5) # set centre and extent of mapNelson[1],Nelson[2]
(m2 <- m %>%
  setView(Nelson[1], Nelson[2], 5) %>% # map location
  addMarkers(Nelson[1], Nelson[2]) %>% # add a marker
  addPopups(Nelson[1]+1, Nelson[2]+1, popup = "Hello From Near Nelson!") %>% # popup
  addPolygons(data = regions.rg, color = "red", weight = 4) %>% 
  # add som circles:
  addCircles(color = "black", newdfADF$Longitude, newdfADF$Latitude, 10))


library(leaflet)
library(shiny)
shinyApp(
  ui = fluidPage(leafletOutput('myMap')),
  server = function(input, output) {

    # download and load data
    map = leaflet() %>% addTiles() %>% setView(Nelson[1], Nelson[2], 5) %>% addPolygons(data = regions.rg, color = "red", weight = 4) %>% addCircles(color = "black", newdfADF$Longitude, newdfADF$Latitude, 10)
    output$myMap = renderLeaflet(map)
  }
)

# library(rsconnect)
# deployApp()
# 
# shinyApp(ui, server)


# ---------- maptools ---------- #
# 
# library(maptools)
# 
# # read in shapefiles; here we use the specialized readShape* functions,
# # but readShapeSpatial would produce identical output in all three cases
# centroids.mp <- readShapePoints("nw-centroids")
# rivers.mp <- readShapeLines("nw-rivers")
# counties.mp <- readShapePoly("nw-counties")
# 
# # note that readShape* does _not_ read the shapefile's .prj file
# print(proj4string(counties.mp))
# ## [1] NA
# 
# # specifying projection information is not strictly necessary for
# # plotting, but does yield nicer default axis labels and aspect ratio in
# # the case of geographic data
# proj4string(counties.mp) <- "+proj=longlat +datum=WGS84"
# 
# # generate a simple map showing all three layers
# plot(counties.mp, axes=TRUE, border="gray")
# points(centroids.mp, pch=20, cex=0.8)
# lines(rivers.mp, col="blue", lwd=2.0)
# 
# # write out a new shapefile (but without .prj); the more general
# # writeSpatialShape would produce equivalent output
# writePolyShape(counties.mp, "counties-maptools")
# 
# 
# # ---------- PBSmapping ---------- #
# 
# library(PBSmapping)
#    
# # read in shapefiles
# centroids.pb <- importShapefile("nw-centroids")
# rivers.pb <- importShapefile("nw-rivers")
# counties.pb <- importShapefile("nw-counties")
#    
# # note that importShapefile reads the .prj file if it exists, but it
# # does not adopt the proj4 format used by the above approaches
# proj.abbr <- attr(counties.pb, "projection") # abbreviated projection info
# proj.full <- attr(counties.pb, "prj") # full projection info
# print(proj.abbr)
# # [1] "LL"
# 
# # generate map using PBSmapping plotting functions
# plotPolys(counties.pb, projection=proj.abbr, border="gray",
#     xlab="Longitude", ylab="Latitude")
# addPoints(centroids.pb, pch=20, cex=0.8)
# addLines(rivers.pb, col="blue", lwd=2.0)  
# 
# 
# # ---------- generate PNG map ---------- #
# 
# png("map-points-lines-polys.png", height=600, width=400)
# par(mfrow=c(2,1))
# 
# # generate the same map as above
# plot(counties.rg, axes=TRUE, border="gray")
# points(centroids.rg, pch=20, cex=0.8)
# lines(rivers.rg, col="blue", lwd=2.0)
# title("plot() using data read via rgdal/maptools")
# 
# plotPolys(counties.pb, projection=proj.abbr, border="gray",
#     xlab="Longitude", ylab="Latitude")
# title("plotPolys() using data read via PBSmapping")
# addPoints(centroids.pb, pch=20, cex=0.8)
# addLines(rivers.pb, col="blue", lwd=2.0)  
# 
# dev.off()
# 

.libPaths()

