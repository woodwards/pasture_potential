# pasture_embed##.r
# Simon Woodward, DairyNZ Ltd, 2018-2019
# uses leaflet, plotly, rdrop2
# for embedding in DairyNZ website page
# requires:
#   google.api
#   token.rds

#### global ####

library(shiny)
# library(tidyverse) # don't load whole thing
library(dplyr)
library(ggplot2)
library(purrr)
library(stringr)
library(plotly) 
# library(rlang)
library(leaflet) # interactive map (need devtools version of leaflet)
library(geosphere) # distm
library(proj4) # warning masked by rgdal
library(KernSmooth) # for contouring
library(sp) # spatial polygons
library(quantreg) # quantile regression
library(googleway) # for geocoding
library(rdrop2) # dropbox
library(shinythemes)

# start
cat("\n")
cat("=========================================================\n")
cat("Starting app...\n")

# https://stackoverflow.com/questions/36175529/getting-over-query-limit-after-one-request-with-geocode
# https://developers.google.com/maps/documentation/geocoding/get-api-key
googlekey <- readLines("google.api") # text file with the API key

# dropbox auth
if (file.exists("token.rds")){
  token <- readRDS("token.rds")
} else {
  token <- drop_auth() # launches a login page
  saveRDS(token, "token.rds") # need to do this locally then deploy this file to shiny
}

# read data 
drop_folder <- "pasture_potential_data_files"
join_file_rds <- "joined_small.rds"
drop_download(paste0(drop_folder, "/", join_file_rds), progress=FALSE, verbose=FALSE, overwrite=TRUE, dtoken=token)
data_all <- readRDS(join_file_rds) 

# add elevation factor
data_all <- data_all %>%
	mutate(elev_fact=factor(x=case_when(elev<=50 ~ "Low (0-50m)",
										elev<=200 ~ "Middle (50-200m)",
										TRUE ~ "High (200m+)"),
							levels=c("Low (0-50m)", "Middle (50-200m)", "High (200m+)")))

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
data_all <- data_all %>%
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

#e0efd4 - light green
#e6e6e6 - light grey
#ddfcff - light blue

# colours
zzblack <- "black"
zzwhite <- "white"
zzslate <- "#353735"
zzlightslate <- "#b5c2bc" # https://www.color-hex.com/color-palette/18977
zzpaleslate <- "#e6e6e6"
zzmidslate <- "#c0c2c0"
zzgreen <- "#69BE28"
zzlightgreen <- "#74ff8b" # https://www.color-hex.com/color-palette/77235
zzpalegreen <- "#e0efd4"
zzmidgreen <- "#a5d77e"
zzblue <- "#009AA6"
zzlightblue <- "#94d1e4" # https://www.color-hex.com/color-palette/76824
zzpaleblue <- "#ddfcff"
zzmidblue <- "#6dc9d0"
zzred <- "#ff0000" # https://www.color-hex.com/color-palette/76991
zzdarkred <- "#b30033" # https://www.color-hex.com/color-palette/76991
if (FALSE)
{
  scales::show_col(c(zzslate, zzmidslate, zzlightslate, zzpaleslate, 
                     zzgreen, zzmidgreen, zzlightgreen, zzpalegreen, 
                     zzblue,  zzmidblue,  zzlightblue,  zzpaleblue))
}

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

# fix leaflet cursor
css <- "
/* fix leaflet cursor */
.leaflet-container {
  cursor: auto !important; 
  border: 2px solid bordercolour
}
.well {
  background-color: white; 
  border-color: white;
}"

css <- css %>% 
  str_replace_all("bordercolour", zzblue)

# https://excelquick.com/r-leaflet/leaflet-map-in-r-shiny/
# style=str_replace("color: #fff; background-color: #990000; border-style: solid; border-color: #999999; margin: 5px", "#999999", zzblue),

#### ui ####
ui <- fluidPage(

	tags$style(type='text/css', css),
	theme = shinythemes::shinytheme("spacelab"),
	
	fluidRow(
	  column(6,
	         leaflet::leafletOutput("map", 
	                                width="100%", 
	                                height=455
	         ), 
	         absolutePanel(top=-7, 
	                       left=70, 
	                       textInput("search_bar", "" , default_location, "75%")
	                       ) # search bar
	  ),
	  column(6,
	         uiOutput("season_selector"),
      		 uiOutput("elev_selector"),
      		 uiOutput("soil_selector"),
      		 uiOutput("nitrogen_checkbox")
		)
	), # fluidRow
		
	fluidRow(
	  column(12,
	         plotlyOutput("stacked_histogram",
	                      width="90%")
	  )
	) # fluidRow

) # end ui

#### server ####
server <- function(input, output) {

	# list of reactive variables
	my <- reactiveValues(
	# my <- list( # for testing offline
	           name="You Are Here",
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
		cat("initialise location\n")
		v$long <- my$long
		v$lat <- my$lat
	})

	#### define ui elements ####

	# titlePanel("Where are You?")

	output$season_selector <- renderUI({
		cat("render season selector\n")
		selectInput("season", p("Production season?", style="line-height: 0.0;"), my$season_here, my$season_default )
	})

	output$elev_selector <- renderUI({
		cat("render elev selector\n")
	  selectInput("elev", p("Elevation(s) above sea level?", style="line-height: 0.0;"), my$elev_here, my$elev_default, multiple=TRUE, selectize=FALSE, size=3)
	})

	output$soil_selector <- renderUI({
		cat("render soil selector\n")
		selectInput("soil", p("Soil order(s)?", style="line-height: 0.0;"), my$soil_here, my$soil_default, multiple=TRUE, selectize=FALSE, size=max(3, length(my$soil_here)))
	})

	output$nitrogen_checkbox <- renderUI({
		cat("render adjust checkbox\n")
		selectInput("adjust", p("Nitrogen fertiliser applied?", style="line-height: 0.0;"), my$adjust_here, selected=my$adjust_default)
	})

	#### make initial map ####

	# https://stackoverflow.com/questions/34348737/r-leaflet-how-to-click-on-map-and-add-a-circle
	output$map <- leaflet::renderLeaflet({
		cat("render leaflet\n")

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

		cat("\n")
		cat("observed map_click\n")

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

		cat("\n")
		cat("observed search bar =", input$search_bar, "\n")
		req(input$search_bar, nchar(input$search_bar)>=4)
		req(googlekey>"")

		target_pos <- googleway::google_geocode(paste(input$search_bar, "New Zealand"),
												bounds=list(c(min_lat,min_long),c(max_lat,max_long)),
												key=googlekey)$results$geometry$location %>% rename(lon=lng)
		cat(input$search_bar, "=", target_pos$lon, target_pos$lat, "\n")
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

		cat("\n")
		cat("observed location =", my$long, my$lat, "\n")

		# location for map centre
		nzgd <- data.matrix(c(my$long, my$lat))
		nztm <- proj4::project(xy=nzgd, proj=proj4string)
		my$east <- nztm[,1]
		my$north <- nztm[,2]

		# calculate distance 
		data <- data_all %>%
		  group_by(long, lat) %>%
		  mutate(dist=geosphere::distHaversine(c(my$long, my$lat), c(long[1], lat[1]))/1000) %>%
		  ungroup()

		# filter data
		data <- data %>%
			filter(dist < max(windows)) # km

		# calculate width of histogram for highlighted region
		if (nrow(data)>=nmin){
		  x <- c(data$pasture_eaten_raw, data$pasture_eaten_min, data$pasture_eaten_max)
		} else {
		  x <- 10:17
		}
		minx <- floor(min(x))
		maxx <- ceiling(max(x))
		my$breaks <- seq(minx, maxx, 1)

		# store data for region (only about 60 kB)
		my$data <- data

		# what seasons are available here
		if (nrow(data)>0){
  		i <- sort(unique(as.character(data$season)))
  		my$season_here <- season_all[match(i, season_all)]
  		n <- unlist(purrr::map(my$season_here, function(u) sum(u==data$season))) # could have used table() maybe
  		names(my$season_here) <- paste(names(my$season_here), " (", n, " Farms)", sep="")
		} else {
		  my$season_here <- tail(season_all, 1)
		  n <- 0
		  names(my$season_here) <- paste(names(my$season_here), " (", n, " Farms)", sep="")
		}
		cat("my$season_here =", length(my$season_here), "\n")
		cat(paste(names(my$season_here)), "\n")

		# reset selections
		my$season_default <- tail(my$season_here, 1)
		my$season <- my$season_default
		my$elev <- list(NA)
		my$soil <- list(NA)
		# don't change my$adjust

	}) # end reaction to location changing

	#### react to change of season ####

	observeEvent(input$season, {
		cat("\n")
		cat("observed input$season =", input$season, "\n")
		req(input$season, input$season!="NA")
		if (my$season != input$season) {
			my$season <- input$season
			my$elev <- list(NA)
			my$soil <- list(NA)
			# don't change my$adjust
		}
	})

	observeEvent(my$season, {

		cat("\n")
		cat("observed my$season =", paste(my$season), "\n")
		req(my$season, my$season!="NA")

		# make elev list
		data <- my$data %>%
			filter(season == my$season)

		# data <- data_all[1:10,] # subset for testing
		if (nrow(data)>0){
  		i <- unique(data$elev_fact)
  		my$elev_here <- elev_all[which(elev_all %in% i)] # retains sorting
  		n <- unlist(purrr::map(my$elev_here, function(u) sum(u==data$elev_fact)))
  		names(my$elev_here) <- paste(my$elev_here, " (", n, " Farms)", sep="")
		} else {
		  my$elev_here <- head(elev_all,1)
		  n <- 0
		  names(my$elev_here) <- paste(my$elev_here, " (", n, " Farms)", sep="")
		}
		cat("my$elev_here =", length(my$elev_here), "\n")
		cat(paste(names(my$elev_here)), "\n")

		# reset selections
		my$elev_default <- my$elev_here
		my$elev <- my$elev_default
		my$soil <- list(NA)
		# don't change my$adjust

	}) # end reaction to season changing

	#### react to change of elevation ####

	observeEvent(input$elev,  {
		cat("\n")
		cat("observed input$elev =", paste(input$elev), "\n")
		req(input$elev, input$elev!="NA")
		if (any(my$elev != input$elev, na.rm=TRUE)) {
			my$elev <-  input$elev
			my$soil <- list(NA)
			# don't change my$adjust
		}
	})

	observeEvent(my$elev,  {

		cat("\n")
		cat("observed my$elev =", paste(my$elev), "\n")
		req(my$elev, my$elev!="NA")

		# make soil list
		data <- my$data %>%
			filter(season == my$season) %>%
			filter(elev_fact %in% my$elev)

		if (nrow(data)>0){
  		i <- sort(unique(data$soil))
  		my$soil_here <- soil_all[match(i, soil_all)]
  		n <- unlist(purrr::map(my$soil_here, function(u) sum(u==data$soil)))
  		names(my$soil_here) <- paste(names(my$soil_here), " (", n, " Farms)", sep="")
		} else {
		  my$soil_here <- head(soil_all,1)
		  n <- 0
		  names(my$soil_here) <- paste(names(my$soil_here), " (", n, " Farms)", sep="")
		}
		cat(paste("my$soil_here =", length(my$soil_here)), "\n")
		cat(paste(names(my$soil_here)), "\n")

		# reset selections
		my$soil_default <- my$soil_here
		my$soil <- my$soil_default
		# don't change my$adjust

	})

	#### react to change of soil ####

	observeEvent(input$soil, {
		cat("\n")
		cat("observed input$soil =", paste(input$soil), "\n")
		req(input$soil, input$soil!="NA")
		if (any(my$soil != input$soil, na.rm=TRUE)) {
			my$soil <- input$soil
			# don't change my$adjust
		}
	})

	observeEvent(my$soil, {

		cat("\n")
		cat("observed my$soil = ", paste(my$soil), "\n")
		req(my$soil, my$soil!="NA")

		# trigger recalc
		my$recalc <- my$recalc + 1L

	}) # end reaction to soil changing

	#### react to change of adjust ####

	observeEvent(input$adjust, {
		cat("\n")
		cat("observed input$adjust =", input$adjust, "\n")
		req(my$adjust, input$adjust)
		if (my$adjust != input$adjust) {
			my$adjust <- input$adjust
		}
	})

	observeEvent(my$adjust, {

		cat("\n")
		cat("observed my$adjust =", my$adjust, "\n")
		req(my$adjust)

		# trigger recalc
		my$recalc <- my$recalc + 1L

	})

	#### react to change of inputs ####

	calc <- eventReactive(my$recalc,  {

		cat("\n")
		cat(paste("analyse for location =", my$long, my$lat), "\n")
		cat(paste("analyse for season =", my$season), "\n")
		cat(paste("analyse for soil ="), paste(my$soil), "\n")
		cat(paste("analyse for elev ="), paste(my$elev), "\n")
		cat(paste("analyse for adjust ="), my$adjust, "\n")
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

		cat("nrow(data) = ", nrow(data), "\n")

		samppdf_list <- vector("list", length(windows))
		all_radius <- vector("character", length(windows))

		# loop through decreasing window sizes
		for (i in seq_along(windows)) {

			# select data within window
		  window <- windows[i]
		  data_window <- data %>% filter(dist < window)
			n <- nrow(data_window)
			code <- paste(window," km (", format(n, width=1), " Farms)", sep="")
			all_radius[[i]] <- code
			
			cat("window = ", window, " km", "\n")

			# save sample quantiles if enough data to be sensible
			if (n >= nmin) {

				# calculate quantile
				qr1 <- quantreg::rq(formula=pasture_eaten ~ 1, tau=0.9, data=data_window) # linear quantile regression
				se_method <- "boot" # how condience intervals are calculated, some methods more robust
				yqr1<- predict(qr1, tibble(east=my$east, north=my$north), interval="confidence", level=0.666, se=se_method)
				q90 <- quantile(data_window$pasture_eaten, 0.9, type=1) # also calc simple q90

				cat(paste("yqr1 =", yqr1), "\n")
				# cat(paste("q90 =", q90), "\n") # should be the same

				samppdf_list[[i]] <- tibble(pasture=data_window$pasture_eaten, 
				                            window=window,
				                            radius=code,
				                            q=round(q90,1), 
				                            qr=round(yqr1[1],1), 
				                            qrlower=round(yqr1[2],1), 
				                            qrupper=round(yqr1[3],1))

			} else {

			  samppdf_list[[i]] <- tibble(pasture=NA, window=window,
			                              radius=code,
			                              q=NA, qr=NA, qrlower=NA, qrupper=NA)
			}

		} # next window size

		samppdf <- dplyr::bind_rows(samppdf_list) %>% 
		  mutate(radius=factor(radius, levels=all_radius))
		  
		return(list(samppdf=samppdf)) 
		
	}) # end reaction to elev changing, calculation of calc <- list(results)

	#### create histograms ####

	output$stacked_histogram <- renderPlotly({

		samppdf <- calc()$samppdf # get data for histogram when calc() changes

		# https://plot.ly/r/subplots/ 
		all_radius=levels(samppdf$radius)
		p <- vector("list", length(all_radius)) # list of subplots
		xbreaks <- c(my$breaks[1]-0.5, my$breaks+0.5)
		pdata <- filter(samppdf, radius==all_radius[1])$pasture
		if (length(pdata)>=nmin){
		  yrange <- graphics::hist(x=pdata, breaks=xbreaks, include.lowest=TRUE, plot=FALSE)$counts
		} else {
		  yrange <- c(0,1)
		}
		xbins <- list(start=min(xbreaks), end=max(xbreaks), size=1)
		xrange <- range(xbreaks)
		yrange <- range(yrange)
		# https://www.w3.org/TR/css-color-3/#svg-color
		gridcolor <- toRGB(zzmidslate)
		targetcolor <- toRGB(zzblue)
		radiuscolor <- toRGB(zzslate)
		textsize <- 15
		nfarms <- table(samppdf$radius)
		uncop <- 0.4 # uncertainty opacity
		cat("nfarms =", paste(nfarms), "\n")
		# bottom
		p[[3]] <- samppdf %>% 
		  filter(radius==all_radius[1]) %>% 
		  plot_ly(colors=window_cols, height=500) %>%
		  add_text(x=xrange[1]+(xrange[2]-xrange[1])*0.95, y=yrange[2]*0.9, text=~paste0("<b>", radius[1], "</b>"), textposition="middle left", color=I(radiuscolor), textfont=list(size=textsize)) 
		if (nfarms[1]>1){
  		p[[3]] <- p[[3]] %>% 
  		  add_histogram(x=~pasture, color=~radius, opacity=0.5, xbins=xbins, hoverinfo="x+y") %>% 
  		  add_segments(x=~qrlower[1], xend=~qrlower[1], y=yrange[1], yend=yrange[2], color=I(targetcolor), opacity=uncop, showlegend=FALSE, line=list(dash="dash"), name="uncertainty", hoverinfo="x+name") %>% 
  		  add_segments(x=~qrupper[1], xend=~qrupper[1], y=yrange[1], yend=yrange[2], color=I(targetcolor), opacity=uncop, showlegend=FALSE, line=list(dash="dash"), name="uncertainty", hoverinfo="x+name") %>% 
  		  add_segments(x=~qr[1], xend=~qr[1], y=yrange[1], yend=yrange[2], color=I(targetcolor), showlegend=FALSE, name="target", hoverinfo="x+name") %>% 
  		  add_text(x=~qr[1], y=yrange[2]*0.7, text=~sprintf("<b>  %.1f t</b>", q[1]), color=I(targetcolor), textposition="middle right", textfont=list(size=textsize), name="target", hoverinfo="name+text")
		}
		p[[3]] <- p[[3]] %>%   
		  layout(
		    showlegend=FALSE,
		    yaxis=list(range=yrange, showticklabels=FALSE, fixedrange=TRUE),
		    xaxis=list(title="<b>Pasture and Crop Eaten, tonnes DM per ha</b>", gridcolor=I(gridcolor), range=xrange, dtick=1, fixedrange=TRUE),
		    margin=list(t=60)
		  )
		# middle
		p[[2]] <- samppdf %>% 
		  filter(radius==all_radius[2]) %>% 
		  plot_ly(colors=window_cols, height=500) %>%
		  add_text(x=xrange[1]+(xrange[2]-xrange[1])*0.95, y=yrange[2]*0.9, text=~paste0("<b>", radius[1], "</b>"), textposition="middle left", color=I(radiuscolor), textfont=list(size=textsize)) 
		if (nfarms[2]>1){
  		p[[2]] <- p[[2]] %>% 
  		  add_histogram(x=~pasture, color=~radius, opacity=0.5, xbins=xbins, hoverinfo="x+y") %>% 
  		  add_segments(x=~qrlower[1], xend=~qrlower[1], y=yrange[1], yend=yrange[2], color=I(targetcolor), opacity=uncop, showlegend=FALSE, line=list(dash="dash"), name="uncertainty", hoverinfo="x+name") %>% 
  		  add_segments(x=~qrupper[1], xend=~qrupper[1], y=yrange[1], yend=yrange[2], color=I(targetcolor), opacity=uncop, showlegend=FALSE, line=list(dash="dash"), name="uncertainty", hoverinfo="x+name") %>% 
  		  add_segments(x=~qr[1], xend=~qr[1], y=yrange[1], yend=yrange[2], color=I(targetcolor), showlegend=FALSE, name="target", hoverinfo="x+name") %>% 
  		  add_text(x=~qr[1], y=yrange[2]*0.7, text=~sprintf("<b>  %.1f t</b>", q[1]), color=I(targetcolor), textposition="middle right", textfont=list(size=textsize), name="target", hoverinfo="name+text")
  		}
		p[[2]] <- p[[2]] %>% 
		  layout(
		    showlegend=FALSE,
		    yaxis=list(title="<b>Number of Farms</b>", range=yrange, showticklabels=FALSE, fixedrange=TRUE),
		    xaxis=list(gridcolor=I(gridcolor), range=xrange, dtick=1, fixedrange=TRUE)
		  ) 
		# top
		p[[1]] <- samppdf %>% 
		  filter(radius==all_radius[3]) %>% 
		  plot_ly(colors=window_cols, height=500) %>%
		  add_text(x=xrange[1]+(xrange[2]-xrange[1])*0.95, y=yrange[2]*0.9, text=~paste0("<b>", radius[1], "</b>"), textposition="middle left", color=I(radiuscolor), textfont=list(size=textsize)) 
		if (nfarms[3]>1){
  		p[[1]] <- p[[1]] %>% 
  		  add_histogram(x=~pasture, color=~radius, opacity=0.5, xbins=xbins, hoverinfo="x+y") %>% 
  		  add_segments(x=~qrlower[1], xend=~qrlower[1], y=yrange[1], yend=yrange[2], color=I(targetcolor), opacity=uncop, showlegend=FALSE, line=list(dash="dash"), name="uncertainty", hoverinfo="x+name") %>% 
  		  add_segments(x=~qrupper[1], xend=~qrupper[1], y=yrange[1], yend=yrange[2], color=I(targetcolor), opacity=uncop, showlegend=FALSE, line=list(dash="dash"), name="uncertainty", hoverinfo="x+name") %>% 
  		  add_segments(x=~qr[1], xend=~qr[1], y=yrange[1], yend=yrange[2], color=I(targetcolor), showlegend=FALSE, name="target", hoverinfo="x+name") %>% 
  		  add_text(x=~qr[1], y=yrange[2]*0.7, text=~sprintf("<b>  %.1f t</b>", q[1]), color=I(targetcolor), textposition="middle right", textfont=list(size=textsize), name="target", hoverinfo="name+text")
		}
		p[[1]] <- p[[1]] %>% 
		  layout(
		    showlegend=FALSE,
		    plot_bgcolor=zzpalegreen,
		    yaxis=list(range=yrange, showticklabels=FALSE, fixedrange=TRUE),
		    xaxis=list(gridcolor=I(gridcolor), range=xrange, dtick=1, fixedrange=TRUE, tickfont=list(size=14)),
		    title=list(text=paste("<b>Pasture and Crop Eaten Near", my$name, "</b>"))
		  )
		p %>% 
		  subplot(nrows=3, shareX=TRUE, shareY=TRUE) %>% 
		  # style(hoverinfo="none") %>% 
		  config(displayModeBar=FALSE) 
		  
	}) # end renderPlot

}

# profvis::profvis(shiny::runApp("pasture_embed26.r"))

# this is how you run it
shinyApp(ui = ui, server = server)
