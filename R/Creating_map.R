library(tidyverse) ##data wrangling
library(rstan) 
library(brms) 
library(tidybayes)
library(DHARMa)
library(emmeans)
library(patchwork) ##combining multiple plots
library(rnaturalearth) ##for interacting with natural earth
library(sf) ##for spatial features in R
library(ggspatial) ##map scalebars and arrows

remotes::install_github("ropensci/rnaturalearthhires")

install.packages("remotes")

sf::sf_use_s2(FALSE)    ##TURN S2 off (S2 is a feature that is very acurrate but slows down process)

##download stuff from natural earth website
coast_php <- rnaturalearth::ne_countries(
  scale = 10, type = "countries",
  country = "Philippines",
  returnclass = "sf") 

library (devtools)
library (remotes)

remotes::install_github("ropensci/rnaturalearthhires")


ggplot() +
  geom_sf(data= coast_php)

##lets make a bounding box for Palawan

bbox <- sf::st_bbox(c(
  xmin = 116,
  xmax = 121,
  ymin = 6,
  ymax = 13
), crs = sf::st_crs(coast_php))

##read in the tropical Coral Reefs of the World shapefile
reef <- sf::read_sf ("../data/GIS/reef_500_poly.shp")


reef_pal <- reef |>
  sf::st_transform(crs = sf::st_crs(coast_php)) |>
  sf::st_make_valid() |>
  sf::st_crop(bbox)

coast_pal <- coast_php |>
  sf::st_crop(bbox)

##Make a Map of Palawan
ggplot() +
  geom_sf(data = coast_pal, fill = "green") + 
  geom_sf(data = reef_pal, colour = "blue") +
  ggspatial::annotation_scale(location = "bl",
                              width_hint = 0.4,
                              bar_cols = c("grey20","white")) + 
  annotation_north_arrow(location = "bl", 
                         which_north = "true", 
                         pad_x = unit (0.25, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme_bw()

## Zoom in a specific area
ggplot() +
  geom_sf(data = coast_pal, fill = "green") + 
  geom_sf(data = reef_pal, colour = "blue") +
  ggspatial::annotation_scale(location = "bl",
                              width_hint = 0.4,
                              bar_cols = c("grey20","white")) + 
  annotation_north_arrow(location = "bl", 
                         which_north = "true", 
                         pad_x = unit (0.25, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
              coord_sf(xlim = c(119, 120),
                       ylim = c(10,11), expand = FALSE) +
              theme_bw()

  
## Map of the Philippines to use as an inset

base_map <-
ggplot() +
  geom_sf(data = coast_pal, fill = "green") + 
  geom_sf(data = reef_pal, colour = "cyan")+
  ggspatial::annotation_scale(location = "bl",
                              width_hint = 0.4,
                              bar_cols = c("grey20","white")) + 
  annotation_north_arrow(location = "bl", 
                         which_north = "true", 
                         pad_x = unit (0.25, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme_bw()
base_map

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

bbox <- sf::st_bbox(c(
  xmin = 118.5,
  xmax = 119.7,
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


cities <- rnaturalearth::ne_states(
  country = "Philippines",
  returnclass = "sf"
)

cities$name


php_cities <- cities |> 
  ##filter(name%in% c("Puerto Princesa", ""))
  filter(name == "Puerto Princesa")

base_map <-
  ggplot() +
  geom_sf(data = coast_pal) +
  geom_sf(data = reef_pal, colour = "cyan") +
  ggspatial::annotation_scale(location = "bl",
                              width_hint = 0.4,
                              bar_cols = c("grey20", "white")) +
  annotation_north_arrow(location = "tl",
                         which_north = "true",
                         pad_x = unit(0.2, "npc"), pad_y = unit (0.2, "in"),
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

##plotting the real location of study site

Pagawanen <- data.frame(
  latitude = c(11.1059611),
  longitude = c(119.3311861))

Helicopter <- data.frame(
  latitude = c(11.2004194),
  longitude = c(119.3417194))

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
  coord_sf(xlim = c(119.8, 118.6),
           ylim = c(10.5,11.5), expand = FALSE) +
  geom_point(data = Pagawanen, aes(y = latitude, x = longitude,
                                   colour = "Pagawanen"), shape = 16, size =  1) +
  geom_point(data = Helicopter, aes(y = latitude, x = longitude,
                                    colour = "Helicopter"), shape = 16, size = 1) +
  geom_point(data = php_cities,
             aes(y = latitude, x = longitude)) +
  geom_text(data = php_cities,
            aes(y = latitude + 0.1,
                x = longitude, label = name), vjust =  0) +
  scale_colour_manual("", values = c("red", "grey", "blue" , "pink"))+
  scale_fill_manual("", breaks = "land", values = c("grey")) +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "#0000ff10")
  )

base_map +
  inset_element(pal_map, left = 0.01, bottom = 0.4, right = 0.4, top = 1)

base_map +
  inset_element(pal_map, left = 0.6, bottom = 0, right = 0.99, top = 0.5)


                