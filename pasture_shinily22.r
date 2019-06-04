# https://shiny.rstudio.com/tutorial/written-tutorial/lesson1/

library(shiny)

#### global ####

# library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly) 
# library(rlang)
library(leaflet) # interactive map (need devtools version of leaflet)
# library(ggmap) # register google api
# library(geosphere) # distm
# library(proj4) # warning masked by rgdal
# library(KernSmooth) # for contouring
# library(sp) # spatial polygons
# library(quantreg) # quantile regression
# # library(ggthemes) # chart theme
# # library(dismo) # geocode
# # library(raster) # required by geocode. Warning - masks dplyr::select
# # library(XML) # needed by geocode
# library(googleway) # for geocoding
# library(cowplot)

# https://stackoverflow.com/questions/36175529/getting-over-query-limit-after-one-request-with-geocode
# https://developers.google.com/maps/documentation/geocoding/get-api-key
# https://lucidmanager.org/geocoding-with-ggmap/
googlekey <- readLines("google.api") # text file with the API key
# ggmap::register_google(key = googlekey)
# ggmap::ggmap_credentials()

# this is good place to put global data or utility functions
# you can change them (for every session) but you have to use the <<- operator

# read multiyear data and tidy up
data_all <- readRDS("joined.rds") %>%
	dplyr::mutate(pasture_eaten=as.numeric(pasture_and_crop_eaten_t_dm_ha),
				 region=as.factor(region),
				 season=as.factor(season),
				 slope=slope,
				 aspect=aspect,
				 nitrogen_applied=as.numeric(nitrogen_applied_for_year_l2),
				 topo=topo_position_index,
				 soil=as.character(nzsc_order))
data_all <- data_all %>%
	dplyr::select(farm_number, pasture_eaten, region, season, long, lat, soil, elev,
				  slope, aspect, topo, nitrogen_applied)
temp <- nrow(data_all)
data_all <- data_all %>% 
  tidyr::drop_na() # drops a lot of rows!
cat(file=stderr(), paste("Rows dropped with missing data =", temp-nrow(data_all), "\n"))
cat(file=stderr(), paste("Rows remaining =", nrow(data_all), "\n"))
# write_tsv(data_all, "data_all.tsv") # dump text version

# add elevation factor
data_all <- data_all %>%
	mutate(elev_fact=factor(x=case_when(elev<=50 ~ "Low (0-50m)",
										elev<=200 ~ "Middle (50-200m)",
										TRUE ~ "High (200m+)"),
							levels=c("Low (0-50m)", "Middle (50-200m)", "High (200m+)")))

# summary(data_all)

# create season and soil list
season_all <- as.list(sort(unique(as.character(data_all$season))))
names(season_all) <- season_all
soil_all <- as.list(sort(unique(data_all$soil)))
names(soil_all) <- soil_all
names(soil_all)[soil_all=="L"] <- "Allophanic"
names(soil_all)[soil_all=="A"] <- "Anthropic"
names(soil_all)[soil_all=="B"] <- "Brown"
names(soil_all)[soil_all=="Z"] <- "Podzol"
names(soil_all)[soil_all=="M"] <- "Pumice"
names(soil_all)[soil_all=="W"] <- "Raw"
names(soil_all)[soil_all=="G"] <- "Gley"
names(soil_all)[soil_all=="N"] <- "Granular"
names(soil_all)[soil_all=="E"] <- "Melanic"
names(soil_all)[soil_all=="R"] <- "Recent"
names(soil_all)[soil_all=="S"] <- "Semiarid"
names(soil_all)[soil_all=="U"] <- "Ultic"
names(soil_all)[soil_all=="O"] <- "Organic"
names(soil_all)[soil_all=="X"] <- "Oxidic"
names(soil_all)[soil_all=="P"] <- "Pallic"
elev_all <- levels(data_all$elev_fact)
names(elev_all) <- elev_all

# define nitrogen steps
nitrogen_range <- seq(0L, 250L, 50L)
nitrogen_here <- as.list(c("Don't Adjust", paste(nitrogen_range, "kgN/ha/y")))
nitrogen_default <- nitrogen_here[[4]]

# calculate nitrogen adjustment
#  could fail if there is insufficient data to do regression
# temp <- data_all %>%
#   group_by(season, elev_fact, soil) %>%
#   do(nitrogen_slope=lm(pasture_eaten ~ nitrogen_applied + 1, data=.)$coefficients["nitrogen_applied"],
#      nitrogen_count=length(.$nitrogen_applied)
#      )
# temp$nitrogen_slope <- unlist(temp$nitrogen_slope)
# ggplot(data=temp) +
#   geom_histogram(mapping=aes(nitrogen_slope*1000)) +
#   scale_x_continuous(limits=c(-30,50))
# temp$nitrogen_count <- unlist(temp$nitrogen_count)
# temp$nitrogen_slope <- median(temp$nitrogen_slope, na.rm=TRUE) # median about 0.010 anyway?
# temp$nitrogen_slope <- 0.010 # after all that, use standard value
data_all <- data_all %>%
	# left_join(temp, by=c("season", "elev_fact", "soil")) %>%
	mutate(
		nitrogen_slope = 0.010,
		pasture_eaten_raw = pasture_eaten,
		pasture_eaten_min = pasture_eaten + (min(nitrogen_range) - nitrogen_applied) * nitrogen_slope,
		pasture_eaten_max = pasture_eaten + (max(nitrogen_range) - nitrogen_applied) * nitrogen_slope
	)

# create data point map/contours for plotting on leaflet
# https://gis.stackexchange.com/questions/168886/r-how-to-build-heatmap-with-the-leaflet-package
data_pts <- unique(data_all[c("long", "lat")])
kde <- KernSmooth::bkde2D(data.matrix(data_pts), bandwidth=c(0.1, 0.1), gridsize=c(100,100))
CL <- contourLines(kde$x1 , kde$x2 , kde$fhat) # contour lines (list)
LEVS <- as.factor(sapply(CL, `[[`, "level")) # contour levels (vector)
NLEV <- length(levels(LEVS)) # number of levels
pgons <- lapply(1:length(CL), function(i) # convert to polygons (ID=i is actually the level)
	sp::Polygons(list(sp::Polygon(cbind(CL[[i]]$x, CL[[i]]$y))), ID=i))
spgons = sp::SpatialPolygons(pgons)
spgon_cols <- topo.colors(NLEV, NULL)[LEVS]

# convert spatialPolygons to data frame for use in ggplot
#spgonsdf <- broom::tidy(spgons, region=ID)

# calculate NZTM2000 coordinates for farm locations
proj4string <- "+proj=tmerc +lat_0=0.0 +lon_0=173.0 +k=0.9996 +x_0=1600000.0 +y_0=10000000.0 +datum=WGS84 +units=m"
nzgd <- data.matrix(data_all[,c("long", "lat")])
nztm <- proj4::project(xy=nzgd, proj=proj4string)
temp <- proj4::project(xy=nzgd, proj=proj4string, inverse=TRUE)
data_all$east <- nztm[,1]
data_all$north <- nztm[,2]

# define some constants
trim <- 0.0 # rqss fails near tails if insufficient data
probs <- seq(trim, 1-trim, 0.02) # probabilities for sampcdf
nprobs <- length(probs)
windows <- c(60,40,20)
nmin <- 4L # minimum number of farms in a window for analysis

# gets a list of default ggplot colours
gg_colour_hue <- function(n) {
	hues = seq(15, 375, length = n + 1)
	hcl(h = hues, l = 65, c = 100)[1:n]
}
window_cols <- gg_colour_hue(length(windows))

# default_loc must be somewhere with data!!!
# default_loc <- list(long=172.833333, lat=-41.5) # Nelson
default_loc <- list(long=175.619105, lat=-40.386396) # Massey
# default_loc <- list(long=174.865530, lat=-41.259256) # Wellington
# default_loc <- list(long=175.352116, lat=-37.781841) # DairyNZ
# default_loc <- list(long=167.893066, lat=-43.036775) # In the ocean!

min_long <- 160
max_long <- 180
min_lat <- -50
max_lat <- -30
# default_location <- "Click map or enter location here"
default_location <- ""

# normal list with same elements, useful for testing
my <- list(name="You Are Here",
		   long=default_loc$long,
		   lat=default_loc$lat,
		   distortion=1,
		   east=NA, north=NA,
		   adjust_here=nitrogen_here,
		   adjust_default=nitrogen_default,
		   adjust=nitrogen_default,
		   season_here=list(NA),
		   season=list(NA),
		   soil_here=list(NA),
		   soil=list(NA),
		   elev_here=list(NA),
		   elev=list(NA),
		   data=list(NA),
		   breaks=list(NA),
		   recalc=0L
)

css <- "
/* fix cursor */
.leaflet-container {
  cursor: auto !important;
}
"

#### ui ####
ui <- fluidPage(

	tags$style(type='text/css', css),

	sidebarLayout(

		sidebarPanel(
			tags$style(".well {background-color:#FFFFFF;border-color:#FFFFFF;}"), # worked!
			cat(file=stderr(), paste("render sidebar"), "\n"),
			width=5, # 12ths of the panel
			uiOutput("season_selector"),
			uiOutput("elev_selector"),
			uiOutput("soil_selector"),
			uiOutput("nitrogen_checkbox")
		), # end sidebarPanel

		mainPanel(
			cat(file=stderr(), paste("render main panel"), "\n"),
			width=7, # 12ths of the panel
			leaflet::leafletOutput("map", width="100%", height=480), # can manipulate size here
			absolutePanel(top=10, left=70, textInput("search_bar", "" , default_location, "75%")) # search bar
		), # end mainPanel

		position="right"

	), # end sidebarLayout

	fluidRow(

		plotlyOutput("stacked_histogram")

	) # end fluidRow

) # end ui

#### server ####
server <- function(input, output) {

	# collect info about the current location and selections
	# these variables are available in the reactive context
	# "my" is a list of reactive objects
	my <- reactiveValues(name="You Are Here",
						 long=default_loc$long,
						 lat=default_loc$lat,
						 distortion=1,
						 east=NA, north=NA,
						 adjust_here=nitrogen_here,
						 adjust_default=nitrogen_default,
						 adjust=nitrogen_default,
						 season_here=list(NA),
						 season_default=list(NA),
						 season=list(NA),
						 soil_here=list(NA),
						 soil_default=list(NA),
						 soil=list(NA),
						 elev_here=list(NA),
						 elev_default=list(NA),
						 elev=list(NA),
						 data=list(NA),
						 breaks=list(NA),
						 recalc=0L
	)

	# set leaflet info
	v <- reactiveValues(zoom=5, minzoom=5, maxzoom=15, long=NA, lat=NA)

	# initialise map centre
	isolate({
		cat(file=stderr(), paste("initialise location"), "\n")
		v$long <- my$long
		v$lat <- my$lat
	})

	#### define ui elements ####

	# titlePanel("Where are You?")

	output$season_selector <- renderUI({
		cat(file=stderr(), paste("render season selector"), "\n")
		selectInput("season", p("Production season?"), my$season_here, selected=my$season_default)
	})

	output$elev_selector <- renderUI({
		cat(file=stderr(), paste("render elev selector"), "\n")
		selectInput("elev", p("Altitude over sea level?"), my$elev_here, selected=my$elev_default,
					selectize=FALSE, multiple=TRUE)
	})

	output$soil_selector <- renderUI({
		cat(file=stderr(), paste("render soil selector"), "\n")
		selectInput("soil", p("Soil orders?"), my$soil_here, selected=my$soil_default,
					selectize=FALSE, multiple=TRUE)
	})

	output$nitrogen_checkbox <- renderUI({
		cat(file=stderr(), paste("render adjust checkbox"), "\n")
		selectInput("adjust", p("Nitrogen fertiliser applied?"), my$adjust_here, selected=my$adjust_default)
	})

	#### make initial map ####

	# https://stackoverflow.com/questions/34348737/r-leaflet-how-to-click-on-map-and-add-a-circle
	output$map <- leaflet::renderLeaflet({
		cat(file=stderr(), paste("render leaflet"), "\n")

		isolate({ # prevent redraw if arguments change

			leaflet(spgons, options=leafletOptions(minZoom=v$minzoom, maxZoom=v$maxzoom)) %>%
				setView(v$long, v$lat, zoom=v$zoom)  %>%
				addTiles() %>% # default map
				addPolygons(data=spgons, color=spgon_cols, weight=0, options=pathOptions(clickable=FALSE)) %>%
				addMarkers(my$long, my$lat, "layer1", options=pathOptions(clickable=FALSE)) %>%
				addCircles(my$long, my$lat, layerId=as.character(windows), radius=windows*1000,
						   color=window_cols, weight=4, fill=NA, options=pathOptions(clickable=FALSE))

		})

	}) # end renderLeaflet

	#### react to mouse clicks ####

	# see also https://rstudio.github.io/leaflet/shiny.html
	# "_click" is an event on the object "map"
	observeEvent(input$map_click, {

		cat(file=stderr(), "\n")
		cat(file=stderr(), paste("observed map_click"), "\n")

		click <- input$map_click
		my$long <- click$lng
		my$lat <- click$lat
		my$season <- list(NA)
		my$elev <- list(NA)
		my$soil <- list(NA)

		# update map
		leafletProxy("map", deferUntilFlush=FALSE) %>%
			addMarkers(my$long, my$lat, "layer1", options=pathOptions(clickable=FALSE)) %>%
			addCircles(my$long, my$lat, layerId=as.character(windows), radius=windows*1000,
					   color=window_cols, weight=4, fill=NA, options=pathOptions(clickable=FALSE))

	}) # end observe mouse click

	#### react to search bar ####

	# https://www.r-bloggers.com/4-tricks-for-working-with-r-leaflet-and-shiny/
	observeEvent(input$search_bar, {

		cat(file=stderr(), "\n")
		cat(file=stderr(), paste("observed search bar =", input$search_bar), "\n")
		req(input$search_bar, nchar(input$search_bar)>=4)
		req(googlekey>"")

		# target_pos <- dismo::geocode(paste(input$search_bar, "New Zealand"),
		#                              extent=raster::extent(min_long, max_long, min_lat, max_lat),
		#                              oneRecord=TRUE)
		target_pos <- googleway::google_geocode(paste(input$search_bar, "New Zealand"),
												bounds=list(c(min_lat,min_long),c(max_lat,max_long)),
												key=googlekey)$results$geometry$location %>% rename(lon=lng)
		cat(file=stderr(), paste(input$search_bar, "=", target_pos$lon, target_pos$lat), "\n")
		req(target_pos$lon, target_pos$lat)
		req(target_pos$lon!="NA", target_pos$lat!="NA")
		req(target_pos$lon>=min_long, target_pos$lon<=max_long)
		req(target_pos$lat>=min_lat, target_pos$lat<=max_lat)

		my$long <- target_pos$lon
		my$lat <- target_pos$lat
		my$season <- list(NA)
		my$elev <- list(NA)
		my$soil <- list(NA)

		# update map
		leafletProxy("map", deferUntilFlush=FALSE) %>%
			setView(my$long, my$lat, zoom=input$map_zoom)  %>%
			addMarkers(my$long, my$lat, "layer1", options=pathOptions(clickable=FALSE)) %>%
			addCircles(my$long, my$lat, layerId=as.character(windows), radius=windows*1000,
					   color=window_cols, weight=4, fill=NA, options=pathOptions(clickable=FALSE))

	}) # end observe search bar

	#### react to change of location ####

	observeEvent(c(my$long, my$lat), {

		cat(file=stderr(), "\n")
		cat(file=stderr(), paste("observed location =", my$long, my$lat), "\n")

		# calculate aspect ratio near my farm (not used)
		# nzgd <- data.matrix(tibble(long=c(my$long, my$long, my$long-0.5, my$long+0.5),
		#                            lat=c(my$lat-0.5, my$lat+0.5, my$lat, my$lat)))
		# nztm <- proj4::project(xy=nzgd, proj=proj4string)
		# my$distortion <- (max(nztm[,2])-min(nztm[,2]))/(max(nztm[,1])-min(nztm[,1]))

		# location for map centre
		nzgd <- data.matrix(c(my$long, my$lat))
		nztm <- proj4::project(xy=nzgd, proj=proj4string)
		my$east <- nztm[,1]
		my$north <- nztm[,2]

		# calculate distance 
		# this is slow
		# we need to use rowwise()  because distm is not vectorised, I think, although rowwise() is deprecated
		# ungroup() removes the effect of rowwise()
		# http://www.expressivecode.org/2014/12/17/mutating-using-functions-in-dplyr/
		# data <- data_all %>%
		#   rowwise() %>% # slow
		#   mutate(dist=geosphere::distHaversine(c(my$long, my$lat), c(long, lat))/1000) %>%
		#   ungroup()
		data <- data_all %>%
		  group_by(long, lat) %>%
		  mutate(dist=geosphere::distHaversine(c(my$long, my$lat), c(long[1], lat[1]))/1000) %>%
		  ungroup()

		# filter data
		temp <- data %>%
			filter(dist < max(windows)) # km

		# insufficient data?
		if (nrow(temp)>=nmin){
			data <- temp
		} else {
			data <- data[1:max(2,nmin-1),] # grab a few rows to avoid errors (they won't be shown)
		}

		# calculate width of histogram for region
		my$breaks <- seq(  floor(min(data$pasture_eaten_raw, data$pasture_eaten_min, data$pasture_eaten_max)),
						   ceiling(max(data$pasture_eaten_raw, data$pasture_eaten_min, data$pasture_eaten_max)),
						   1)

		# store data for region (only about 60 kB)
		my$data <- data

		# what seasons are available here
		i <- sort(unique(as.character(data$season)))
		my$season_here <- season_all[match(i, season_all)]
		n <- unlist(purrr::map(my$season_here, function(u) sum(u==data$season))) # could have used table() maybe
		names(my$season_here) <- paste(names(my$season_here), " (", n, " Farms)", sep="")
		cat(file=stderr(), paste("my$season_here =", length(my$season_here)), "\n")
		cat(file=stderr(), paste(names(my$season_here)), "\n")

		# reset selections
		my$season_default <- tail(my$season_here, 1)
		my$season <- my$season_default
		my$elev <- list(NA)
		my$soil <- list(NA)
		# don't change my$adjust

	}) # end reaction to location changing

	#### react to change of season ####

	observeEvent(input$season, {
		cat(file=stderr(), "\n")
		cat(file=stderr(), paste("observed input$season =", input$season), "\n")
		req(input$season, input$season!="NA")
		if (my$season != input$season) {
			my$season <- input$season
			my$elev <- list(NA)
			my$soil <- list(NA)
			# don't change my$adjust
		}
	})

	observeEvent(my$season, {

		cat(file=stderr(), "\n")
		cat(file=stderr(), paste("observed my$season =", my$season), "\n")
		req(my$season, my$season!="NA")

		# make elev list
		data <- my$data %>%
			filter(season == my$season)

		# insufficient data?
		if (nrow(data)<nmin){
			data <- my$data[1:max(2,nmin-1),]
		}

		# data <- data_all[1:10,] # subset for testing
		i <- unique(data$elev_fact)
		my$elev_here <- elev_all[which(elev_all %in% i)] # retains sorting
		n <- unlist(purrr::map(my$elev_here, function(u) sum(u==data$elev_fact)))
		names(my$elev_here) <- paste(my$elev_here, " (", n, " Farms)", sep="")
		cat(file=stderr(), paste("my$elev_here =", length(my$elev_here)), "\n")
		cat(file=stderr(), paste(names(my$elev_here)), "\n")

		# reset selections
		my$elev_default <- my$elev_here
		my$elev <- my$elev_default
		my$soil <- list(NA)
		# don't change my$adjust

	}) # end reaction to season changing

	#### react to change of elevation ####

	observeEvent(input$elev,  {
		cat(file=stderr(), "\n")
		cat(file=stderr(), paste("observed input$elev ="), paste(input$elev), "\n")
		req(input$elev, input$elev!="NA")
		if (any(my$elev != input$elev, na.rm=TRUE)) {
			my$elev <-  input$elev
			my$soil <- list(NA)
			# don't change my$adjust
		}
	})

	observeEvent(my$elev,  {

		cat(file=stderr(), "\n")
		cat(file=stderr(), paste("observed my$elev ="), paste(my$elev), "\n")
		req(my$elev, my$elev!="NA")

		# make soil list
		data <- my$data %>%
			filter(season == my$season) %>%
			filter(elev_fact %in% my$elev)

		# insufficient data?
		if (nrow(data)<nmin){
			data <- my$data[1:max(2,nmin-1),]
		}

		i <- sort(unique(data$soil))
		my$soil_here <- soil_all[match(i, soil_all)]
		n <- unlist(purrr::map(my$soil_here, function(u) sum(u==data$soil)))
		names(my$soil_here) <- paste(names(my$soil_here), " (", n, " Farms)", sep="")
		cat(file=stderr(), paste("my$soil_here =", length(my$soil_here)), "\n")
		cat(file=stderr(), paste(names(my$soil_here)), "\n")

		# reset selections
		my$soil_default <- my$soil_here
		my$soil <- my$soil_default
		# don't change my$adjust

	})

	#### react to change of soil ####

	observeEvent(input$soil, {
		cat(file=stderr(), "\n")
		cat(file=stderr(), paste("observed input$soil ="), paste(input$soil), "\n")
		req(input$soil, input$soil!="NA")
		if (any(my$soil != input$soil, na.rm=TRUE)) {
			my$soil <- input$soil
			# don't change my$adjust
		}
	})

	observeEvent(my$soil, {

		cat(file=stderr(), "\n")
		cat(file=stderr(), paste("observed my$soil = "), paste(my$soil), "\n")
		req(my$soil, my$soil!="NA")

		# trigger recalc
		my$recalc <- my$recalc + 1L

	}) # end reaction to soil changing

	#### react to change of adjust ####

	observeEvent(input$adjust, {
		cat(file=stderr(), "\n")
		cat(file=stderr(), paste("observed input$adjust =", input$adjust), "\n")
		req(my$adjust, input$adjust)
		if (my$adjust != input$adjust) {
			my$adjust <- input$adjust
		}
	})

	observeEvent(my$adjust, {

		cat(file=stderr(), "\n")
		cat(file=stderr(), paste("observed my$adjust =", my$adjust), "\n")
		req(my$adjust)

		# trigger recalc
		my$recalc <- my$recalc + 1L

	})

	#### react to change of inputs ####

	calc <- eventReactive(my$recalc,  {

		cat(file=stderr(), "\n")
		cat(file=stderr(), paste("analyse for location =", my$long, my$lat), "\n")
		cat(file=stderr(), paste("analyse for season =", my$season), "\n")
		cat(file=stderr(), paste("analyse for soil ="), paste(my$soil), "\n")
		cat(file=stderr(), paste("analyse for elev ="), paste(my$elev), "\n")
		cat(file=stderr(), paste("analyse for adjust ="), my$adjust, "\n")
		req(my$season!="NA", my$soil!="NA", my$elev!="NA", my$adjust!="NA")
		req(my$long, my$lat, my$season, my$soil, my$elev, my$adjust)

		dont_adjust <- nitrogen_here[[1]]
		nitrogen_level <- as.numeric(stringr::word(my$adjust,1))
		data <-  my$data %>%
			dplyr::filter(season == my$season) %>%
		  dplyr::filter(soil %in% my$soil) %>%
		  dplyr::filter(elev_fact %in% my$elev) %>%
		  dplyr::mutate(pasture_eaten =
    				   	case_when(
    				   		my$adjust==dont_adjust ~ pasture_eaten_raw,
    				   		TRUE ~ pasture_eaten_raw + (nitrogen_level - nitrogen_applied) * nitrogen_slope
    				   	)
			)

		if (my$adjust==dont_adjust){
			my$name <- paste("Your Location (", season_all[my$season[[1]]], ")", sep="")
		} else {
			my$name <- paste("Your Location (", season_all[my$season[[1]]], ")",
							 # " (Nitrogen response = ", sprintf("%.1f", data$nitrogen_slope[1]*1000), ")",
							 sep="")
		}

		cat(file=stderr(), paste("nrow(data) = ", nrow(data)), "\n")

		# circle function
		# circle_fun <- function(centre=c(0,0), r=1, npoints=100){
		# 	tt <- seq(0, 2*pi, length.out=npoints)
		# 	xx <- centre[1] + r * cos(tt)
		# 	yy <- centre[2] + r * sin(tt)
		# 	return(tibble(x=xx, y=yy))
		# }

		# prepare empty data frames for loop
		# farms <- tibble(x=numeric(), y=numeric(), east=numeric(), north=numeric(), long=numeric(), lat=numeric(),
		# 				pasture=numeric(), dist=numeric(), window=numeric(), radius=factor())
		# sampcdf <- tibble(probs=numeric(), quants=numeric(), radius=factor())
		# samppdf <- tibble(pasture=numeric(), window=numeric(), radius=factor(),
		# 				  q=numeric(), qr=numeric(), qrlower=numeric(), qrupper=numeric())
		# circles <- tibble(east=numeric(), north=numeric(), radius=factor())
		# farms_list <- vector("list", length(windows))
		# sampcdf_list <- vector("list", length(windows))
		samppdf_list <- vector("list", length(windows))
		all_radius <- vector("character", length(windows))
		# circles_list <- vector("list", length(windows))
		
		# loop through decreasing window sizes
		for (i in seq_along(windows)) {

			# select data within window
		  window <- windows[i]
		  data_window <- data %>% filter(dist < window)
			n <- nrow(data_window)
			code <- paste(window," km\n(", format(n, width=1), " Farms)", sep="")
			all_radius[[i]] <- code
			
			cat(file=stderr(), paste("window = ", window, " km"), "\n")

			# calculate circle
			# circle <- circle_fun(centre=c(my$east, my$north), r=window*1000, npoints=100)
			# nztm <- data.matrix(circle[,c("x", "y")])
			# nzgd <- proj4::project(xy=nztm, proj=proj4string, inverse=TRUE)
			# circle$long <- nzgd[,1]
			# circle$lat <- nzgd[,2]

			# save selected farms for plot
			# if (n >= 1) {
			# 	# farms <- rbind(farms, tibble(east=data_window$east, north=data_window$north,
			# 	# 							 long=data_window$long, lat=data_window$lat,
			# 	# 							 pasture=data_window$pasture_eaten,
			# 	# 							 dist=data_window$dist, window=window, radius=as.factor(code)))
			# 	farms_list[[i]] <- tibble(east=data_window$east, north=data_window$north,
			# 	                          long=data_window$long, lat=data_window$lat,
			# 	                          pasture=data_window$pasture_eaten,
			# 	                          dist=data_window$dist, window=window, radius=as.factor(code))
			# }

			# circles <- rbind(circles, tibble(long=circle$long, lat=circle$lat, radius=as.factor(code)))
			# circles_list[[i]] <- tibble(long=circle$long, lat=circle$lat, radius=as.factor(code))

			# save sample quantiles if enough data to be sensible
			if (n >= nmin) {

				# calculate quantile
				qr1 <- quantreg::rq(formula=pasture_eaten ~ 1, tau=0.9, data=data_window) # linear quantile regression
				se_method <- "boot" # how condience intervals are calculated, some methods more robust
				yqr1<- predict(qr1, tibble(east=my$east, north=my$north), interval="confidence", level=0.95, se=se_method)
				q90 <- quantile(data_window$pasture_eaten, 0.9, type=1) # also calc simple q90

				cat(file=stderr(), paste("yqr1 =", yqr1), "\n")
				# cat(file=stderr(), paste("q90 =", q90), "\n") # should be the same

				# quants <- quantile(data_window$pasture_eaten, probs=probs, type=8) # see documentation for type=?
				# sampcdf <- rbind(sampcdf, tibble(probs=probs, quants=quants, radius=as.factor(code)))
				# sampcdf_list[[i]] <- tibble(probs=probs, quants=quants, radius=as.factor(code))
				
				# samppdf <- rbind(samppdf, tibble(pasture=data_window$pasture_eaten, window=window,
				# 								 radius=as.factor(code),
				# 								 q=q90, qr=yqr1[1], qrlower=yqr1[2], qrupper=yqr1[3]))
				samppdf_list[[i]] <- tibble(pasture=data_window$pasture_eaten, window=window,
				                            radius=code,
				                            q=q90, qr=yqr1[1], qrlower=yqr1[2], qrupper=yqr1[3])

			} else {

			  samppdf_list[[i]] <- tibble(pasture=NA, window=window,
			                              radius=code,
			                              q=NA, qr=NA, qrlower=NA, qrupper=NA)
			}

			# add a blank line (causes warnings but prevents errors) is this in the wrong place?
			# samppdf <- rbind(samppdf, tibble(pasture=NA, window=window,
			# 								 radius=as.factor(code),
			# 								 q=NA, qr=NA, qrlower=NA, qrupper=NA))

		} # next window size

		samppdf <- dplyr::bind_rows(samppdf_list) %>% 
		  mutate(radius=factor(radius, levels=all_radius))
		  
		# biggest circle
		# circle <- circle_fun(centre=c(my$east, my$north), r=max(windows)*1000, npoints=100)
		# nztm <- data.matrix(circle[,c("x", "y")])
		# nzgd <- proj4::project(xy=nztm, proj=proj4string, inverse=TRUE)
		# circle$long <- nzgd[,1]
		# circle$lat <- nzgd[,2]

		# return results as function for testing
		#calc <- function() list(data=data, circles=circles, circle=circle, farms=farms, sampcdf=sampcdf, samppdf=samppdf)

		# return results in a list
		# return(list(data=data, circles=circles, circle=circle, farms=farms,
		#             sampcdf=sampcdf, samppdf=samppdf))
		return(list(samppdf=samppdf)) # only actually use samppdf
		
	}) # end reaction to elev chaning, calculation of calc <- list(results)

	#### create histograms ####

	output$stacked_histogram <- renderPlotly({

		samppdf <- calc()$samppdf # get data for histogram when calc() changes

		isolate({

			cat(file=stderr(), paste("render stacked histograms"), "\n")

		# 	title_string <- paste("Pasture and Crop Eaten Near", my$name)
		# 
		# 	# create empty plot
		# 	stacked_histogram <- ggplot() +
		# 		labs(title=title_string, y="Number of Farms\n",
		# 			 x="Pasture and Crop Eaten, tonnes DM per ha", colour="Radius (km)") +
		# 		# theme_cowplot() +
		# 		#theme_stata(base_size=16, scheme="s2color") +
		# 		# ggthemes::theme_economist_white(base_size=12, horizontal=FALSE) +
		# 		theme(axis.text.x=element_text(size=16, colour="grey35"),
		# 			  axis.text.y=element_text(size=16, colour="grey35"),
		# 			  strip.text=element_text(size=16, colour="grey35"),
		# 			  plot.title=element_text(size=18, hjust=0.5, colour="grey35"),
		# 			  axis.title=element_text(size=16, colour="grey35"),
		# 			  axis.title.x=element_text(size=18, colour="grey35"),
		# 			  panel.background=element_rect(fill="white")
		# 		) +
		# 		#scale_y_continuous(breaks=c()) + # remove y-scale when too many facets
		# 		cowplot::panel_border(colour="grey35") +
		# 		theme(legend.position="none")
		# 
		# 	# add histograms to empty plot
		# 	if (nrow(tidyr::drop_na(samppdf))>0) {
		# 
		# 		breaks <- my$breaks
		# 
		# 		# cat(file=stderr(), paste("xlim =", min(breaks), max(breaks)), "\n")
		# 
		# 		stacked_histogram <- stacked_histogram +
		# 			geom_rect(data=samppdf, mapping=aes(xmin=qrlower, xmax=qrupper, ymin=0, ymax=Inf), fill="lightcyan") +
		# 			geom_histogram(data=samppdf, mapping=aes(x=pasture, colour=radius), fill=NA, size=1.1, binwidth=1) +
		# 			geom_vline(data=samppdf, mapping=aes(xintercept=qr), size=1.5, colour="lightcyan4", alpha=0.2) +
		# 			geom_vline(data=samppdf, mapping=aes(xintercept=q), size=1.5, colour="blue4") +
		# 			geom_text(data=samppdf, mapping=aes(x=q, y=4, label=sprintf("%.1f t", q)),
		# 					  colour="blue4", size=6, hjust=0, nudge_x=0.2) +
		# 			# facet_grid(radius ~ ., as.table=TRUE) + # as.table=FALSE reverses the order
		# 			theme(strip.background=element_blank(), strip.text.y=element_text(angle=0)) +
		# 			scale_x_continuous(breaks=breaks) +
		# 			# scale_y_continuous(breaks=pretty_breaks(n=4)) +
		# 			scale_y_continuous(breaks=NULL) +
		# 			coord_cartesian(xlim=c(min(breaks),max(breaks)))
		# 
		# 	} # end add histograms to empty plot

		}) # end isolate

		# works
		# plot_ly() %>% 
		#   add_histogram(data=samppdf, x=~pasture, color=~radius) %>% 
		#   layout(
		#     title=list(text=paste("<b>Pasture and Crop Eaten Near", my$name, "</b>"),
		#                font=list(size=18)),
		#     xaxis=list(title="<b>Pasture and Crop Eaten, tonnes DM per ha</b>", gridcolor="grey"),
		#     yaxis=list(title="<b>Number of Farms</b>", showgrid=FALSE)
		#   ) 
		 
		# https://plot.ly/r/subplots/ 
		all_radius=levels(samppdf$radius)
		p <- vector("list", length(all_radius))
		# reverse order since we need to carry yaxis 
		p[[3]] <- samppdf %>% 
		  filter(radius==all_radius[1]) %>% 
		  plot_ly(colors=gg_colour_hue(3)) %>% 
		  add_histogram(x=~pasture, color=~radius, opacity=0.5) %>% 
		  layout(
		    xaxis=list(title="<b>Pasture and Crop Eaten, tonnes DM per ha</b>")
		  )
		p[[2]] <- samppdf %>% 
		  filter(radius==all_radius[2]) %>% 
		  plot_ly(colors=gg_colour_hue(3)) %>% 
		  add_histogram(x=~pasture, color=~radius, opacity=0.5) %>% 
		  layout(
		    yaxis=list(title="<b>Number of Farms</b>")
		  )
		p[[1]] <- samppdf %>% 
		  filter(radius==all_radius[3]) %>% 
		  plot_ly(colors=gg_colour_hue(3)) %>% 
		  add_histogram(x=~pasture, color=~radius, opacity=0.5) %>% 
		  layout(
		    title=list(text=paste("<b>Pasture and Crop Eaten Near", my$name, "</b>"))
		  )
		p %>% 
		  subplot(nrows=3, shareX=TRUE)

		# stacked_histogram
		# plotly::ggplotly(stacked_histogram) 
		# plotly::partial_bundle(plotly::toWebGL(plotly::ggplotly(stacked_histogram)))

	}) # end renderPlot

}

# profvis::profvis(shiny::runApp("pasture_shiny19.r"))

# this is how you run it
shinyApp(ui = ui, server = server)
