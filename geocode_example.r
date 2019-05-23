# https://lucidmanager.org/geocoding-with-ggmap
library(tidyverse)
library(ggmap)
api <- readLines("google.api") # Text file with the API key
register_google(key = api)
ggmap_credentials()
locations <- c("Hoensbroek", "Johannesburg", "Barrow-in-Furness",
               "Hong Kong", "Singapore", "Tangail", "Maastricht", "Bendigo") %>%
  geocode()
world <- map_data("world")
ggplot() +
  geom_polygon(data = world,  aes(long, lat, group = group), fill = "grey") +
  geom_point(data = locations, aes(lon, lat), colour = "red", size = 5) + 
  coord_map("ortho", orientation = c(30, 80, 0)) +
  theme_void()

# using googleway
library(googleway)
gg <- google_geocode("Hoensbroek", key = api)$results$geometry$location
