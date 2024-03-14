library(tidyverse)
library(brms)
library(rstan)
library(ggrepel)
library(ggplot2)
library(tidybayes)
library(DHARMa)
library(emmeans)
library(patchwork)
library(sf)
library(rnaturalearth)
library(ggspatial)

setwd("~/Repository/Pal_Test_Repo")

data_rc <- read.csv("../Pal_Test_Repo/data/primary/data-coral-cover.csv")
labelset_rc <- read.csv("../Pal_Test_Repo/data/primary/data-coral-cover-labelset.csv")

data_rc |> glimpse()

data_rc <- data_rc |> 
  dplyr::select(project_id,
                project_name,
                site_id,
                site_name,
                site_latitude,
                site_longitude,
                site_reef_name,
                site_exposure,
                site_reef_type,
                site_reef_zone,
                site_management,
                survey_id,
                survey_title,
                survey_start_date..UTC.,
                survey_depth,
                survey_transect_number, 
                image_id,
                image_disabled,
                point_machine_classification,
                point_human_classification
  ) |> 
  rename(survey_start_date = survey_start_date..UTC.) |> 
  dplyr::filter(image_disabled == "False") |> 
  select(-image_disabled)

data_rc <- data_rc |> 
  pivot_longer(cols = matches("point_.*_classification"),
               names_to = "type",
               values_to = "classification"
  ) 

data_rc <-
  data_rc |>
  left_join(labelset_rc |>
              dplyr::select(CODE, GROUP = `FUNCTIONAL.GROUP`),
            by = c("classification" = "CODE")
  ) |> 
  mutate(transect_name = paste(site_name, year(survey_start_date), survey_transect_number, sep ="_"),
         transect_id = paste0(site_id, year(survey_start_date), survey_transect_number)) |>
  mutate(year = lubridate::year(survey_start_date))

data_rc_cover <- 
  data_rc |> 
  group_by(across(c(starts_with("site"),
                    starts_with("survey"),
                    starts_with("transect"),
                    year,
                    type,
                    image_id,
                    classification,
                    GROUP))
  ) |>
  summarise(COUNT = n(), .groups = "keep") |> 
  ungroup(GROUP) |>
  mutate(TOTAL = sum(COUNT)) |>
  ungroup() 

GROUPS <- data_rc_cover |> pull(GROUP) |> unique()
filler <- data_rc_cover %>%
  dplyr::select(
    starts_with("site"),
    survey_start_date,
    survey_depth,
    transect_name,
    transect_id,
    image_id,
    type,
    TOTAL) |> 
  distinct() |> 
  tidyr::crossing(GROUP = GROUPS) 

data_rc_cover <-
  data_rc_cover |> 
  full_join(filler) |>
  group_by(
    across(c(starts_with("site"),
             survey_start_date,
             survey_depth,
             transect_name,
             transect_id,
             image_id,
             type,
             GROUP
    ))) |> 
  mutate(COUNT = ifelse(is.na(COUNT), 0, COUNT),
         TOTAL = max(TOTAL, na.rm = TRUE)
  )

data_rc_cover |> glimpse()
data_rc_cover$site_name |> unique() 

data_rc_cover <- data_rc_cover |>
  filter(type == "point_machine_classification") |>
  mutate(site_management = case_when(
    site_name %in% c("DENR_Snake_S3", "PAMO_Helicopter","WWF_Ar_Cambari", "WWF_Ar_Catad") ~ "Protected",
    site_name %in% c("WPU_PB_Aquarium","WPU_PB_Fantastic", "WWF_Ar_Langoy", "WPU_PB_Manta") ~ "Tourist_Site",
  ))

data_rc_cover$site_management <- factor(data_rc_cover$site_management, levels = c("Protected","Tourist_Site"))
levels(data_rc_cover$site_management)

data_rc_cover_site <- data_rc_cover |>
  group_by(
    across(c(starts_with("site"),
             survey_start_date,
             survey_depth,
             transect_name,
             transect_id,
             type,
             GROUP
    ))) |> 
  summarise(COUNT = sum(COUNT),
            TOTAL = sum(TOTAL)
  ) |> 
  ungroup() 


##boxplot
plotRC1 <- data_rc_cover_site |> 
  filter(GROUP == "HC") |> ggplot() +
  geom_boxplot(aes(x = site_name, y = COUNT/TOTAL, fill = site_management)) +
  ggtitle("Hard Coral Cover") +
  theme_bw(base_size = 16) +
  theme(axis.text.x = element_text(angle=30, hjust = 1))

plotRC1


data_rc_cover_site |> ggplot(aes(y = site_latitude, x = site_longitude)) +
  geom_point()

map1 <- rnaturalearth::ne_countries(scale = 10, country = "Philippines", returnclass = "sf")
ggplot() + geom_sf(data = map1)

ggplot()+
  geom_sf(data=map1)

bbox <- sf::st_bbox(map1)

bbox <- sf::st_bbox(c(
  xmin = 116,
  xmax = 121,
  ymin = 8,
  ymax = 13),
  crs = sf::st_crs(map1))

reef <- sf::read_sf("../PCSD/PCSD-Repository/data/GIS/reef_500_poly.shp")

reef_pal <- reef |> 
  sf::st_transform(crs = sf::st_crs(map1)) |> 
  sf::st_make_valid() |> 
  sf::st_crop(bbox)

coast_pal <- map1 |> 
  sf::st_crop(bbox)

data_rc_cover_site <- data_rc_cover_site |> 
  st_as_sf(coords = c("site_longitude", "site_latitude"), 
           remove = FALSE, 
           crs = 4326)
data_rc_cover_site

bbox <- data_rc_cover_site |> 
  st_bbox()

bbox <- data_rc_cover_site |> st_buffer(dist = units::as_units(40, "km")) |> st_bbox()
bbox
bbox1 <- bbox

bbox <- sf::st_bbox(c(
  xmin = 118.3,
  xmax = 120.5,
  ymin = 9,
  ymax = 11.5),
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

Protected <- data.frame(
  latitude = c(9.91,10.55,10.54,11.2), 
  longitude = c(118.85,120.09,120.02,119.34))
Tourist_Site <- data.frame(
  latitude = c(10.43,10.46,10.5,10.47),
  longitude = c(119.12,119.15,120,119.17)
)

plot1<-
  ggplot() +
  geom_sf(data = map1, fill = "white") +
  geom_sf(data = reef_pal, fill = "pink") +
  geom_point(data = Protected, aes(y = latitude, x = longitude, colour = "Protected"), shape = 16, size = 2) +
  geom_point(data = Tourist_Site, aes(y = latitude, x = longitude, colour = "Tourist_Site"), shape = 16, size = 2) +
  ggspatial::annotation_north_arrow(location = "tr",
                                    which_north = "true",
                                    pad_x = unit(0.05, "in"), pad_y = unit (0.05, "npc"),
                                    style = north_arrow_fancy_orienteering) + 
  ggspatial::annotation_scale(location = "bl",
                              width_hint = 0.5,
                              bar_cols = c("grey20", "white")) +
  coord_sf(xlim = c(118.3, 120.5), ylim = c(9, 12), expand = FALSE) +
  geom_point(data = php_cities,
             aes(y = latitude, x = longitude)) +
  geom_text(data = php_cities,
            aes(y = latitude + 0.1,
                x = longitude, label = name),
            vjust = 0) +
  theme_bw() +
  theme(panel.background = element_rect(fill = "#0000ff10"))
plot1

base_map <-
  ggplot() +
  geom_sf(data = map1, aes(colour = "Land", fill = "Land")) +
  geom_sf(data = reef_pal, aes(colour = "Reef")) +
  geom_point(data = Protected, aes(y = latitude, x = longitude, colour = "Protected"), shape = 16, size = 2) +
  geom_point(data = Tourist_Site, aes(y = latitude, x = longitude, colour = "Tourist_Site"), shape = 16, size = 2) +
  ggspatial::annotation_north_arrow(location = "tr",
                                    which_north = "true",
                                    pad_x = unit(0.05, "in"), pad_y = unit (0.05, "npc"),
                                    style = north_arrow_fancy_orienteering) + 
  ggspatial::annotation_scale(location = "bl",
                              width_hint = 0.5,
                              bar_cols = c("grey20", "white")) + 
  coord_sf(xlim = c(118.3, 120.5), ylim = c(9.5, 11.5), expand = FALSE) +
  scale_color_manual("", values = c("grey", "red", "pink", "green")) +
  scale_fill_manual("", breaks = "Land", values = c("grey")) +
  geom_point(data = php_cities,
             aes(y = latitude, x = longitude)) +
  geom_text(data = php_cities,
            aes(y = latitude + 0.1,
                x = longitude, label = name),
            vjust = 0) +
  theme_classic() +
  theme(panel.background = element_rect(fill = "#0000ff10"))
base_map

base_map +
  inset_element(pal_map, left = 0.01, bottom = 0.7, right = 0.4, top = 0.99)

lab <- read.csv("../Pal_Test_Repo/data/GIS/reef.csv", sep=",")

labplot <- ggplot(lab, aes(x= site_longitude, y= site_latitude, colour="green", label= site_name)) +
  geom_point()
labplot

ggplot(lab, aes(x= site_longitude, y= site_latitude, colour="green", label=site_name))+
  geom_point() +geom_text(hjust=0, vjust=0)

ggplot(lab, aes(x= site_longitude, y= site_latitude, colour="green", label=site_name))+
  geom_point() +
  geom_text(aes(label=ifelse(site_latitude>24,as.character(site_name),'')),hjust=0,vjust=0)


labplot <- ggplot(lab, aes(x= site_longitude, y= site_latitude)) + 
  geom_point(color = "blue", size = 3)
labplot

### geom_label_repel
labplot + 
  geom_label_repel(aes(label = site_name),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  theme_classic()

labplot2 <-
  ggplot(lab, aes(x= site_longitude, y= site_latitude, label = site_name)) + 
  geom_point(color = dplyr::case_when(lab$site_latitude > 25 ~ "#1b9e77", 
                                      lab$site_latitude < 18 ~ "#d95f02",
                                      TRUE ~ "#7570b3"), 
             size = 3, alpha = 0.8) +
  geom_text_repel(data          = subset(lab, site_latitude > 25),
                  nudge_y       = 32 - subset(lab, site_latitude > 25)$site_latitude,
                  size          = 4,
                  box.padding   = 1.5,
                  point.padding = 0.5,
                  force         = 100,
                  segment.size  = 0.2,
                  segment.color = "grey50",
                  direction     = "x") +
  geom_label_repel(data         = subset(lab, site_latitude < 18),
                   nudge_y       = 16 - subset(lab, site_latitude < 18)$site_latitude,
                   size          = 4,
                   box.padding   = 0.5,
                   point.padding = 0.5,
                   force         = 100,
                   segment.size  = 0.2,
                   segment.color = "grey50",
                   direction     = "x") +
  scale_x_continuous(expand = expand_scale(mult = c(0.2, .2))) +
  scale_y_continuous(expand = expand_scale(mult = c(0.1, .1))) +
  theme_classic(base_size = 16)
labplot2

labplot2 +
  geom_sf(data = map1, fill = "white") +
  geom_sf(data = reef_pal, fill = "pink") +
  coord_sf(xlim = c(118, 120.5), ylim = c(9, 11.5), expand = FALSE) +
  theme_classic() +
  theme(panel.background = element_rect(fill = "#0000ff10"))
