
##Load necessary libraries (7 libraries)
library(tidyverse)
library(brms)
library(rstan)
library(ggrepel)
library(ggplot2)
library(emmeans)
library(patchwork)
library(sf)
library(rnaturalearth)
library(ggspatial)
library(cmdstanr)
library(DHARMa)
library(tidybayes)

source ("functions.R")
setwd("../R")


## ----readData
data_rc <- read.csv("../data/primary/data-coral-cover.csv")
labelset_rc <- read.csv("../data/primary/data-coral-cover-labelset.csv")

data_rc |> glimpse()
## ----end

## ---- filterDisabledImages
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
## ----end

## ---- pivotLonger
data_rc <- data_rc |> 
  pivot_longer(cols = matches("point_.*_classification"),
               names_to = "type",
               values_to = "classification"
  ) 
## ----end

## ---- addTransectInfo
data_rc <-
  data_rc |>
  left_join(labelset_rc |>
              dplyr::select(CODE, GROUP = `FUNCTIONAL.GROUP`),
            by = c("classification" = "CODE")
  ) |> 
  mutate(transect_name = paste(site_name, year(survey_start_date), survey_transect_number, sep ="_"),
         transect_id = paste0(site_id, year(survey_start_date), survey_transect_number)) |>
  mutate(year = lubridate::year(survey_start_date))
## ----end

## ---- calculateCount
data_rc_cover_site <- 
  data_rc |> 
  group_by(across(c(starts_with("site"),
                    starts_with("survey"),
                    starts_with("transect"),
                    year,
                    type,
                    classification,
                    GROUP))
  ) |>
  summarise(COUNT = n(), .groups = "keep") |> 
  ungroup(GROUP) |>
  mutate(TOTAL = sum(COUNT)) |>
  ungroup() 
## ----end

## ---- fillDataCategories
GROUPS <- data_rc_cover_site |> pull(GROUP) |> unique()
filler <- data_rc_cover_site %>%
  dplyr::select(
    starts_with("site"),
    survey_start_date,
    survey_depth,
    transect_name,
    transect_id,
    type,
    TOTAL) |> 
  distinct() |> 
  tidyr::crossing(GROUP = GROUPS) 
## ----end

## ---- calculateCover
data_rc_cover_site <-
  data_rc_cover_site |> 
  full_join(filler) |>
  group_by(
    across(c(starts_with("site"),
             survey_start_date,
             survey_depth,
             transect_name,
             transect_id,
             type,
             GROUP
    ))) |> 
  mutate(COUNT = ifelse(is.na(COUNT), 0, COUNT),
         TOTAL = max(TOTAL, na.rm = TRUE)
  )
## ----end


data_rc_cover_site |> glimpse()

## ---- addManagement
data_rc_cover_site$site_name |> unique() 

data_rc_cover_site <- data_rc_cover_site |>
  filter(type == "point_machine_classification") |>
  mutate(site_management = case_when(
    site_name %in% c("DENR_Snake_S3", "PAMO_Helicopter","WWF_Ar_Cambari", "WWF_Ar_Catad") ~ "Protected",
    site_name %in% c("WPU_PB_Aquarium","WPU_PB_Fantastic", "WWF_Ar_Langoy", "WPU_PB_Manta") ~ "Tourist_Site",
  ))

data_rc_cover_site$site_management <- factor(data_rc_cover_site$site_management, levels = c("Protected","Tourist_Site"))
## ----end

levels(data_rc_cover_site$site_management)

## ----calculateSiteCover
data_rc_cover_site <- data_rc_cover_site |>
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
## ----end

##boxplot

## ---- plot1a
plotRC1a <- data_rc_cover_site |> 
  filter(GROUP == "HC") |> ggplot() +
  geom_boxplot(aes(x = site_name, y = COUNT/TOTAL, fill = site_management)) +
  ggtitle("Hard Coral Cover") +
  theme_bw(base_size = 16) +
  theme(axis.text.x = element_text(angle=30, hjust = 1))

plotRC1a
## ----end

## ---- modelFormula
formRC1a <- bf(COUNT | trials(TOTAL) ~ site_management + (1 | site_name),
              family = beta_binomial(link = "logit"))
## ----end

get_prior(formRC1a, data=data_rc_cover_site)

## ---- createPrior
data_rc_cover_site %>% 
  mutate(cover = COUNT/TOTAL) %>% 
  group_by(site_management) %>% 
  summarise(across(cover, list(mean, sd, median, sd)))

qlogis(0.128)

priors <- prior(normal(-1.91, 1), class = "Intercept") +
  prior(normal(0,1), class = "b") +
  prior(student_t(3,0,1), class = "sd")
## ----end

## ---- model
model_RC1a <- brm(formRC1a, 
                data = data_rc_cover_site,
                prior = priors,
                chains = 3, cores = 3,
                iter = 3000,
                warmup = 1000,
                thin = 10,
                sample_prior = "yes",
                control=list(adapt_delta=0.99),
                backend = "rstan")
save(model_RC1a, file="../data/modelled/q6_RC_mod1a.Rdata")
## ----end


model_RC1a |> conditional_effects() |> plot() 

model_RC1a <- model_RC |> 
  update(sample_prior = "yes")

model_RC1a |> conditional_effects() |> plot()

## ---- modelSummary
load("../data/modelled/q6_RC_mod1a.Rdata")
model_RC1a |> summary()

model_RC1a$fit |> stan_trace()

model_RC1a$fit |> stan_ac()

model_RC1a$fit |> stan_rhat()

model_RC1a$fit |> stan_ess()

model_RC1a |> pp_check(type = "dens_overlay", ndraws = 100)

resids <- model_RC1a |> make_brms_dharma_res(integerResponse = FALSE)

testUniformity(resids)

plotResiduals(resids, form = factor(rep(1, nrow(data_rc_cover_site))))

plotResiduals(resids)

testDispersion(resids)

save(model_RC1a, file = "../data/modelled/q6_mod_RC1a.Rdata")
## ----end

## ----pairWiseContrast

load("../data/modelled/q6_mod_RC1a.Rdata")

model_RC1a |> summary()

model_RC1a |> 
  as_draws_df() |> 
  dplyr::select(starts_with("b_")) |> 
  mutate(b_Intercept = plogis(b_Intercept)) |> 
  mutate(across(starts_with("b_site"), exp)) |> 
  summarise_draws(median, 
                  HDInterval::hdi,
                  Pl = ~mean(.x < 1),
                  Pg = ~ mean(.x > 1),
                  Pg10 = ~ mean(.x > 1.1))
## ----end

model_RC1a |> emmeans(~ site_management, type = "response")

model_RC1a  |> emmeans(~ site_management, type = "response") |>
  pairs()


model_RC1a  |> 
  emmeans(~site_management) |> 
  regrid() |> 
  pairs() |> 
  gather_emmeans_draws() |> 
  ggplot(aes(x = .value, y = contrast)) +
  stat_halfeye(aes(fill = after_stat(level)), .width = c(0.66, 0.95, 1)) +
  scale_fill_brewer() +
  geom_vline(xintercept = 0, linetype = "dashed")

## ----LoadPhilippinesMap
map1 <- rnaturalearth::ne_countries(scale = 10, country = "Philippines", returnclass = "sf")
ggplot() + geom_sf(data = map1)
## ----end

## ----ZoomPalawanMap
bbox <- sf::st_bbox(map1)

bbox <- sf::st_bbox(c(
  xmin = 116,
  xmax = 121,
  ymin = 8,
  ymax = 13),
  crs = sf::st_crs(map1))
## ----end

## ---- PlottingDiveLocations
data_rc_cover_site |> ggplot(aes(y = site_latitude, x = site_longitude)) +
  geom_point()
reef <- sf::read_sf("../Pal_Test_Repo/data/GIS/reef_500_poly.shp")
## ----end

## ----createInsetMap
ggplot() + geom_sf(data = map1)

bbox <- sf::st_bbox(c(
  xmin = 118.3,
  xmax = 120.5,
  ymin = 9,
  ymax = 11.5),
  crs = sf::st_crs(coast_pal))

coast_pal <- map1 |> 
  sf::st_crop(bbox)

data_rc_cover_site <- data_rc_cover_site |> 
  st_as_sf(coords = c("site_longitude", "site_latitude"), 
           remove = FALSE, 
           crs = 4326)
data_rc_cover_site


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

## ----end

## ----LoadCitiesNames
cities <- rnaturalearth::ne_states(
  country = "Philippines",
  returnclass = "sf"
)

cities$name

php_cities <- cities |> 
  ##filter(name%in% c("Puerto Princesa", ""))
  filter(name == "Puerto Princesa")
## ----end

## ----CoordinatesPerCategory
Protected <- data.frame(
  latitude = c(9.91,10.55,10.54,11.2), 
  longitude = c(118.85,120.09,120.02,119.34))
Tourist_Site <- data.frame(
  latitude = c(10.43,10.46,10.5,10.47),
  longitude = c(119.12,119.15,120,119.17)
)
## ----end

## ----BaseMap
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
## ----end

## ----FinalMap
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
## ----end



base_map <-
  ggplot() +
  geom_sf(data = coast_pal, aes(colour = "land", fill = "land")) +
  geom_sf(data = reef_pal, aes(colour = "reef")) +
  ggspatial::annotation_scale(location = "br",
                              width_hint =  0.4,
                              bar_cols = c("grey", "white"))+
  annotation_north_arrow(location = "br",
                         which_north = "true", 
                         pad_x = unit(0.1, "npc"),
                         pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(118.5, 120.2), 
           ylim = c(9.6,11.5), expand = FALSE) +
  geom_point(data = Protected, aes(y = latitude, x = longitude,
                                   colour = "Protected"), 
             shape = 16, size =  2) +
  geom_point(data = Tourist_Site, aes(y = latitude, x = longitude,
                                      colour = "Tourist_Site"), 
             shape = 16, size =  2) +
  geom_point(data = php_cities,
             aes(y = latitude, x = longitude)) +
  geom_text(data = php_cities,
            aes(y = latitude + 0.1,
                x = longitude, label = name), vjust =  0) +
  scale_colour_manual("", values = c("grey", "red","pink", "blue")) +
  scale_fill_manual("", breaks = "land", values = c("grey")) +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "#0000ff10")
  )

base_map

base_map +
  inset_element(map1, left = 0.01, bottom = 0.6, right = 0.4, top = 0.99)




