library(tidyverse)
library(rstan)
library(brms)
library(tidybayes)
library(DHARMa)
library(emmeans)
library(patchwork)
library(sf)
library(rnaturalearth)
library(ggspatial)

coast_php <- rnaturalearth::ne_countries(scale = 10, country = "Philippines", returnclass = "sf")

ggplot()+
  geom_sf(data=coast_php)

bbox <- sf::st_bbox(coast_php)

bbox <- sf::st_bbox(c(
  xmin = 116,
  xmax = 121,
  ymin = 8,
  ymax = 13),
  crs = sf::st_crs(coast_php))

reef <- sf::read_sf("../PCSD/PCSD-Repository/data/GIS/reef_500_poly.shp")

reef_pal <- reef |> 
  sf::st_transform(crs = sf::st_crs(coast_php)) |> 
  sf::st_make_valid() |> 
  sf::st_crop(bbox)

coast_pal <- coast_php |> 
  sf::st_crop(bbox)

ggplot() +
  geom_sf(data = coast_pal, fill = "green", colour = "black") +
  geom_sf(data = reef_pal, colour = "blue") +
  ggspatial::annotation_scale(location = "bl",
                              width_hint = 0.4,
                              bar_cols = c("grey20", "white")) +
  ggspatial::annotation_north_arrow(location = "tr",
                                    which_north = "true",
                                    pad_x = unit(0.25,"in"), pad_y = unit (0.2, "in"),
                                    style = north_arrow_fancy_orienteering) +
  theme_bw()

ggplot() +
  geom_sf(data = coast_pal, fill = "green", colour = "black") +
  geom_sf(data = reef_pal, fill = "blue", colour = "black") +
  ggspatial::annotation_scale(location = "bl",
                              width_hint = 0.5,
                              bar_cols = c("grey20", "white")) +
  ggspatial::annotation_north_arrow(location = "tl",
                                    which_north = "true",
                                    pad_x = unit(0.05, "npc"), pad_y = unit (0.05, "npc"),
                                    style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(119, 120), ylim = c(10, 11), expand = FALSE) +
  theme_bw()

php_map <-
  ggplot() +
  geom_sf(data = coast_php, fill = "grey20") +
  geom_sf(dat = sf::st_as_sfc(bbox), fill = "#c6c6c640") +
  theme_bw() +
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank())
  php_map

cities <- rnaturalearth::ne_states(
  country = "Philippines",
  returnclass = "sf"
)

cities$name

php_cities <- cities |> 
  ##filter(name%in% c("Puerto Princesa", ""))
filter(name == "Puerto Princesa")

jessHouse <- data.frame(
  latitude = c(9.73603034138314), 
  longitude = c(118.74771525325221)
)

base_map <-
  ggplot() +
  geom_sf(data = coast_pal, aes(colour = "land", fill = "land")) +
  geom_sf(data = reef_pal, aes(colour = "reef", fill = "reef")) +
  ggspatial::annotation_scale(location = "bl",
                              width_hint = 0.4,
                              bar_cols = c("grey20", "white")) +
  annotation_north_arrow(location = "tl",
                                    which_north = "true",
                                    pad_x = unit(0.05, "npc"), pad_y = unit (0.05, "in"),
                                    style = north_arrow_fancy_orienteering) +
  geom_point(data = php_cities,
             aes(y = latitude, x = longitude)) +
  geom_text(data = php_cities,
            aes(y = latitude + 0.1,
                x = longitude, label = name),
            vjust = 0) +
  theme_bw() +
  theme(panel.background = element_rect(fill = "#0000ff10"))
base_map

base_map +
  inset_element(php_map, left = 0.6, bottom = 0, right = 0.99, top = 0.5)

bbox <- sf::st_bbox(c(
  xmin = 118,
  xmax = 120,
  ymin = 10,
  ymax = 12),
  crs = sf::st_crs(coast_pal))

pal_map<-
  ggplot() +
  geom_sf(data = coast_pal, fill = "grey20") +
  geom_sf(dat = sf::st_as_sfc(bbox), fill = "#c6c6c640") +
  theme_bw() +
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank())
pal_map 

Helicopter <- data.frame(
  latitude = c(11.2004194), 
  longitude = c(119.3417194))
Pagawanen <- data.frame(
  latitude = c(11.1059611),
  longitude = c(119.3311861)
)

base_map <-
  ggplot() +
  geom_sf(data = coast_pal, aes(colour = "Land", fill = "Land")) +
  geom_sf(data = reef_pal, aes(colour = "Reef")) +
  ggspatial::annotation_scale(location = "bl",
                              width_hint = 0.4,
                              bar_cols = c("grey20", "white")) +
  annotation_north_arrow(location = "bl",
                         which_north = "true",
                         pad_x = unit(0.05, "npc"), pad_y = unit (0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  
  geom_point(data = Helicopter, aes(y = latitude, x = longitude, colour = "Helicopter"), shape = 16, size = 2) +
  geom_point(data = Pagawanen, aes(y = latitude, x = longitude, colour = "Pagawanen"), shape = 16, size = 2) +
  coord_sf(xlim = c(118, 120), ylim = c(10, 12), expand = FALSE) +
  scale_color_manual("", values = c("red", "grey", "green", "blue")) +
  scale_fill_manual("", breaks = "Land", values = c("grey")) +
  theme_bw() +
  theme(panel.background = element_rect(fill = "#0000ff10"))
base_map

base_map +
  inset_element(pal_map, left = 0.01, bottom = 0.6, right = 0.4, top = 0.99)

