library(tidyverse)

setwd("R")

sf::sf_use_s2(FALSE)

coast_php <- rnaturalearth::ne_countries(
  scale = 10, type = "countries",
  country = "Philippines", returnclass = "sf"
)
ggplot() +
  geom_sf(data = coast_php)

bbox <- sf::st_bbox(c(
  xmin = 116,
  xmax = 121,
  ymin = 6,
  ymax = 13
), crs = sf::st_crs(coast_php))

reef <- sf::read_sf("../data/GIS/reef_500_poly.shp")

reef_pal <- reef |>
  sf::st_transform(crs = sf::st_crs(coast_php)) |>
  sf::st_make_valid() |>
  sf::st_crop(bbox)
coast_pal <- coast_php |>
  sf::st_crop(bbox)

coast_pal <- coast_php |>
  sf::st_crop(bbox)

ggplot() + 
  geom_sf(data = coast_pal) +
  geom_sf(data = reef_pal, colour = "blue") +
  ggspatial::annotation_scale(location = "bl", 
                              width_hint = 0.4,
                              bar_cols = c("pink", 'white'))+
  annotation_north_arrow(location = "bl", which_north = 
                                "true",
pad_x = unit(0.25, "in"), pad_y = unit(0.2, "in"),
      style = north_arrow_fancy_orienteering) +
      theme_bw()

ggplot() + 
  geom_sf(data = coast_pal) +
  geom_sf(data = reef_pal, colour = "blue") +
  ggspatial::annotation_scale(location = "bl", 
                              width_hint = 0.4,
                              bar_cols = c("pink", 'white'))+
  annotation_north_arrow(location = "bl", which_north = 
                           "true",
                         pad_x = unit(0.25, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
coord_sf(xlim = c(119, 120),
         ylim = c(10,11), expand = FALSE) +
  theme_bw()

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

php_cities <- cities |>
  filter(name %in% c("Puerto Princesa",""))


base_map <-
ggplot() + 
  geom_sf(data = coast_pal) +
  geom_sf(data = reef_pal, colour = "blue") +
  ggspatial::annotation_scale(location = "bl", 
                              width_hint = 0.4,
                              bar_cols = c("pink", 'white'))+
  annotation_north_arrow(location = "bl", which_north = 
                           "true",
                         pad_x = unit(0.25, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  geom_point(data = php_cities,
             aes(y = latitude, x = longitude)) +
  geom_text(data = php_cities, 
            aes(y = latitude + 0.1,
                x = longitude, label = name), vjust = 0) +
  theme_bw() + 
  theme(
    panel.background = element_rect(fill = "#0000ff10")
  )


base_map +
  inset_element(php_map, left = 0.01, bottom = 0.6, right =
                  0.4, top = 0.99)

cities <- rnaturalearth::ne_states(
  country = "Philippines",
  returnclass = "sf"
)

php_cities <- cities |>
  filter(name %in% c("Puerto Princesa",""))

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

base_map <-
  ggplot() +
  geom_sf(data = coast_pal) +
  geom_sf(data = reef_pal, colour = "blue") +
  ggspatial::annotation_scale(location = "bl",
                              width_hint = 0.4,
                              bar_cols = c("grey20", "white")) +
  annotation_north_arrow(location = "bl",
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
Write to ReefScan-ReefCloud Palawan






