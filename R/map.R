library(tidyverse)     ## data wrangling
library(sf)            ## for spatial features in R
library(rnaturalearth) ## for interacting with natural earth
library(ggspatial)     ## map scalebars and arrows
library(patchwork)     ## combining multiple plots

sf::sf_use_s2(FALSE)

coast_php <- rnaturalearth::ne_countries(
  scale = 10, type = "countries",
  country = "Philippines",
  returnclass = "sf"
)
#coast_php
ggplot() +
  geom_sf(data = coast_php)

## bbox <- sf::st_bbox(coast_php)

## Lets make a bounding box for Palawan
bbox <- sf::st_bbox(c(
  xmin = 116,
  xmax = 121,
  ymin = 6,
  ymax = 13
), crs = sf::st_crs(coast_php))

## Read in the Tropical Coral Reefs of the World shapefile
reef <- sf::read_sf("../data/GIS/reef_500_poly.shp")

reef_pal <- reef |>
  sf::st_transform(crs = sf::st_crs(coast_php)) |>
  sf::st_make_valid() |>
  sf::st_crop(bbox)

coast_pal <- coast_php |>
  sf::st_crop(bbox)

## Make a map of Palawan
ggplot() +
  geom_sf(data = coast_pal, aes(fill = "land"), colour = "brown") +
  geom_sf(data = reef_pal, colour = "blue") +
  ggspatial::annotation_scale(location = "bl",
    width_hint =  0.4,
    bar_cols = c("pink", "white"))+
  annotation_north_arrow(location = "bl", which_north = "true", 
    pad_x = unit(0.15, "npc"), pad_y = unit(0.05, "npc"),
    style = north_arrow_fancy_orienteering) +
  theme_bw() 


## Zoom in on a specific area
ggplot() +
  geom_sf(data = coast_pal) +
  geom_sf(data = reef_pal, colour = "blue") +
  ggspatial::annotation_scale(location = "bl",
    width_hint =  0.4,
    bar_cols = c("grey20", "white"))+
  annotation_north_arrow(location = "bl",
    which_north = "true", 
    pad_x = unit(0.25, "in"),
    pad_y = unit(0.2, "in"),
    style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(119, 120),
    ylim = c(10,11), expand = FALSE) +
  theme_bw() 

## Map of the Philippines to use as an inset
php_map <-
  ggplot() +
  geom_sf(data = coast_php, fill = "grey20") +
  geom_sf(dat = sf::st_as_sfc(bbox), fill = "#c6c6c640") +
  theme_bw() +
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank()
  )
php_map 

## Store the base map
base_map <-
  ggplot() +
  geom_sf(data = coast_pal) +
  geom_sf(data = reef_pal, colour = "blue") +
  ggspatial::annotation_scale(location = "bl",
    width_hint =  0.4,
    bar_cols = c("grey20", "white"))+
  annotation_north_arrow(location = "bl",
    which_north = "true", 
    pad_x = unit(0.2, "npc"), pad_y = unit(0.2, "in"),
    style = north_arrow_fancy_orienteering) +
  theme_bw() +
  theme(panel.background = element_rect(fill = "#0000ff10"))

base_map +
  inset_element(php_map, left = 0.01, bottom = 0.6, right = 0.4, top = 0.99)




## Retrieve Philippines cities
cities <- rnaturalearth::ne_states(
  country = "Philippines",
  returnclass = "sf"
)
## Narrow down to the cities we want to place on the map
php_cities <- cities |>
  ## filter(name %in% c("Puerto Princesa", ""))
  filter(name == "Puerto Princesa")

jessies_house <- data.frame(
  latitude = c(9.7363, 9.5),
  longitude = c(117.7477, 116)
)
base_map <-
  ggplot() +
  geom_sf(data = coast_pal, aes(colour = "land", fill = "land")) +
  geom_sf(data = reef_pal, aes(colour = "reef")) +
  ggspatial::annotation_scale(location = "bl",
    width_hint =  0.4,
    bar_cols = c("grey20", "white"))+
  annotation_north_arrow(location = "bl",
    which_north = "true", 
    pad_x = unit(0.2, "npc"),
    pad_y = unit(0.2, "in"),
    style = north_arrow_fancy_orienteering) +
  geom_point(data = jessies_house, aes(y = latitude, x = longitude,
    colour = "Jessies house"), shape = 16, size =  1) +
  geom_point(data = php_cities,
    aes(y = latitude, x = longitude)) +
  geom_text(data = php_cities,
    aes(y = latitude + 0.1,
      x = longitude, label = name), vjust =  0) +
  scale_colour_manual("", values = c("red", "green", "blue")) +
  scale_fill_manual("", breaks = "land", values = c("green")) +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "#0000ff10")
  )

base_map +
  inset_element(php_map, left = 0.01, bottom = 0.6, right = 0.4, top = 0.99)

base_map +
  inset_element(php_map, left = 0.6, bottom = 0, right = 0.99, top = 0.5)

