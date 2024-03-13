library(tidyverse)

source("functions.R")

##Read in the data from primary data folder
data <- read_csv("../data/primary/Siete Picados_T1.csv")

##looking at the data 
glimpse(data)
data |> glimpse()
data

data <- data |>
  cpce_classif_to_points() |>
  separate(`Frame image name`,
           into = c("Drive", "Folder", "Project", "Something", "Site", "Transect", "Photo"),
           sep = "\\\\"
  ) |>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = `Major Category`)


data |> as.data.frame() |> head()


## map creation ####

library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggspatial)
library(patchwork)

sf::sf_use_s2(FALSE)

coast_php <- rnaturalearth::ne_countries(
  scale = 10, type = "countries",
  country = "Philippines", returnclass = "sf"
)
coast_php
ggplot() +
  geom_sf(data = coast_php)
## bbox <- sf::st_bbox(coast_php)


## Lets make a bounding box for Palawan
bbox <- sf:: st_bbox(c(
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
coast_pal <-coast_php |> 
  sf::st_crop(bbox)

## Make a map of Palawan
ggplot() +
  geom_sf(data = coast_pal, fill = "grey", colour = "black") +
  geom_sf(data = reef_pal, colour = "skyblue4") +
  ggspatial::annotation_scale(location = "bl",
                              width_hint = 0.4,
                              bar_cols = c("grey20", "white"))+
  annotation_north_arrow(location = "tl", which_north = "true",
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)+
  theme_bw()

## Zoom in on a specific area
ggplot()+
  geom_sf(data = coast_pal, fill = "grey", colour = "black") +
  geom_sf(data = reef_pal, colour = "skyblue4") +
  ggspatial::annotation_scale(location = "bl",
                              width_hint = 0.4,
                              bar_cols = c("grey20", "white"))+
  annotation_north_arrow(location = "tl", which_north = "true",
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)+
  coord_sf(xlim = c(119, 120),
           ylim = c(10,11), expand = FALSE) +
  theme_bw()


## Map of the Philippines to use as an inset
php_map <-
  ggplot() +
  geom_sf(data = coast_php, fill = "grey20") +
  geom_sf(dat = sf::st_as_sfc(bbox), fill = "#c6c6c618") +
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
  geom_sf(data = coast_pal, fill = "grey", colour = "black") +
  geom_sf(data = reef_pal, colour = "skyblue4") +
  ggspatial::annotation_scale(location = "bl",
                              width_hint = 0.4,
                              bar_cols = c("grey20", "white"))+
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)+
  theme_bw() +
  theme(panel.background = element_rect(fill = "#0000ff05"))

base_map +
  inset_element(php_map, left = 0.01, bottom = 0.6, right = 0.4, top = 0.99)

  
## Retrieve Philippine Cities
cities <- rnaturalearth::ne_states(
  country = "Philippines",
  returnclass = "sf"
)

cities$name


## Narrow down to the cities we want to place on the map
php_cities <- cities |> 
  #filter(name %in% c("Puerto Princesa", ""))
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
                              width_hint = 0.4,
                              bar_cols = c("grey20", "white"))+
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)+
  geom_point(data = jessiies_house, aes(y = latitude, x = longitude, colour = "Jessies house"),
             shape = 5, size = 1) +
             aes(y = latitute, x = longitude)) +
  geom_text(data = php_cities,
            aes(y = latitude + 0.1,
                x = longitude, label = name), vjust = 0) +
  scale_colour_manual("", values = c("red", "green", "blue"))+
  scale_fill_manual("", breaks = "land", values = c("green"))+
  theme_bw() +
  theme(panel.background = element_rect(fill = "#0000ff05"))

base_map +
  inset_element(php_map, left = 0.01, bottom = 0.6, right = 0.4, top = 0.99)







