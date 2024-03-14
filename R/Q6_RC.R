<<<<<<< HEAD

##Load necessary libraries (7 libraries)
=======
<<<<<<< HEAD
library(tidyverse) ##for data wrangling
library(brms) ##for modeling
library(rstan) ##for model validation
library(DHARMa) ##for model validation
library(emmeans) ##to gather results
library(tidybayes) ##to get the result

source ("functions.R") ##before using function, set working director, the to source file location

data_rc <- read.csv("../data/primary/data-coral-cover.csv") ##to read the data
labelset_rc <- read.csv("../data/primary/data-coral-cover-labelset.csv") ##to read the data
=======
>>>>>>> f32cb717e682533a41ddace46e3dcd4f651e2882
library(tidyverse)
library(brms)
<<<<<<< HEAD
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
=======
library(cmdstanr)
library(rstan)
library(DHARMa)
library(emmeans)
library(tidybayes)

source ("functions.R")
>>>>>>> 8eb7cb9e4c2b7ba1a272bc7837331886c02de62c

<<<<<<< HEAD
## ----mapPlotting


## ----readData
data_rc <- read.csv("../data/primary/data-coral-cover.csv")
labelset_rc <- read.csv("../data/primary/data-coral-cover-labelset.csv")
=======
setwd("~/Repository/Pal_Test_Repo")

data_rc <- read.csv("../Pal_Test_Repo/data/primary/data-coral-cover.csv")
labelset_rc <- read.csv("../Pal_Test_Repo/data/primary/data-coral-cover-labelset.csv")
>>>>>>> 5b7437daa1faff99545cc9025a51248043a1b22f

<<<<<<< HEAD
data_rc |> glimpse()
## ----end

## ---- filterDisabledImages
data_rc <- data_rc |> 
=======
data_rc |> glimpse() ##to show the content of the data in console

data_rc <- data_rc |>  ## to process the data
>>>>>>> f32cb717e682533a41ddace46e3dcd4f651e2882
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
  dplyr::filter(image_disabled == "False") |> ## False means you don't want the "image_disabled" to be include in your data
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
<<<<<<< HEAD
  mutate(year = lubridate::year(survey_start_date))
## ----end
=======
  mutate(year = lubridate::year(survey_start_date)) ##mutate a function to change some information in the data ##note, right side is the new one, left is the old one
>>>>>>> f32cb717e682533a41ddace46e3dcd4f651e2882

## ---- calculateCount
data_rc_cover <- 
  data_rc |> 
  group_by(across(c(starts_with("site"),      ##to organize which group you want to go first 
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
## ----end

## ---- fillDataCategories
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
## ----end

## ---- calculateCover
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
## ----end


data_rc_cover |> glimpse()

## ---- addManagement
data_rc_cover$site_name |> unique() 

data_rc_cover <- data_rc_cover |>
  filter(type == "point_machine_classification") |>
  mutate(site_management = case_when(
    site_name %in% c("DENR_Snake_S3", "PAMO_Helicopter","WWF_Ar_Cambari", "WWF_Ar_Catad") ~ "Protected",
    site_name %in% c("WPU_PB_Aquarium","WPU_PB_Fantastic", "WWF_Ar_Langoy", "WPU_PB_Manta") ~ "Tourist_Site",
  ))

data_rc_cover$site_management <- factor(data_rc_cover$site_management, levels = c("Protected","Tourist_Site"))
## ----end

levels(data_rc_cover$site_management)

## ----calculateSiteCover
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
## ----end

##boxplot of Hard COral Cover
plotRC1 <- data_rc_cover |> 
  filter(GROUP == "HC") |> ggplot() +
  geom_boxplot(aes(x = site_name, y = COUNT/TOTAL, fill = site_management)) +
  ggtitle("Hard Coral Cover") +
  theme_bw(base_size = 16) +
  theme(axis.text.x = element_text(angle=30, hjust = 1))


plotRC1 ##run this to see the box plot of Hard Coral Cover



##formula of the model relating coral cover to site management
formRC1 <- bf(COUNT | trials(TOTAL) ~ site_management + (1 | site_name),
<<<<<<< HEAD
              family = beta_binomial(link = "logit")) 
=======
              family = binomial(link = "logit"))
>>>>>>> 5b7437daa1faff99545cc9025a51248043a1b22f

##to determine what the default priors could be
get_prior(formRC1, data=data_rc_cover) 

##calculating simple summary to help you create sensible priors
data_rc_cover %>% 
  mutate(cover = COUNT/TOTAL) %>% 
  group_by(site_management) %>% 
  summarise(across(cover, list(mean, sd, median, sd)))

##formula 
qlogis(0.148)

priors <- prior(normal(-1.75, 1), class = "Intercept") +
  prior(normal(0,1), class = "b") +
  prior(student_t(3,0,1), class = "sd")

model_RC <- brm(formRC1, 
                   data = data_rc_cover,
                   prior = priors,
                   chains = 3,
                cores = 3,
                   iter = 3000,
                   warmup = 1000,
                   thin = 10,
                   sample_prior = "yes",
                   backend = "rstan")

model_RC |> conditional_effects() |> plot() 
model_RC |> SUYR_prior_and_posterior()
model_RC <- model_RC |> 
  update(sample_prior = "yes")

model_RC |> conditional_effects() |> plot()

model_RC |> summary()


model_RC$fit |> stan_trace()

model_RC$fit |> stan_ac()

##this model shows how effective your samples are if the value is more than 1
model_RC$fit |> stan_rhat()

##this model 
model_RC$fit |> stan_ess()

model_RC |> pp_check(type = "dens_overlay", ndraws = 100)

resids <- model_RC |> make_brms_dharma_res(integerResponse = FALSE)

testUniformity(resids)

plotResiduals(resids, form = factor(rep(1, nrow(data_rc_cover))))

plotResiduals(resids)

testDispersion(resids)

model_RC |> summary()

##to need to know what is the probability pf being different of two site_management 
model_RC |>
  as_draws_df() %>%
  dplyr::select(starts_with("b_")) |> 
  mutate(b_Intercept = plogis (b_Intercept)) |> 
  mutate(across(starts_with("b_site"), exp)) |> 
  summarise_draws(median,
                  HDInterval::hdi,
                  Pl = ~mean(.x < 1),
                  Pg = ~mean(.x>1),
                  Pg10 = ~mean(.x > 1.1))

model_RC |> emmeans(~ site_management, type = "response")

model_RC  |> emmeans(~ site_management, type = "response") |>
  pairs()

model_RC  |> 
  emmeans(~site_management) |> 
  regrid() |> 
  pairs() |> 
  gather_emmeans_draws() |> 
  ggplot(aes(x = .value, y = contrast)) +
  stat_halfeye(aes(fill = after_stat(level)), .width = c(0.66, 0.95, 1)) +
  scale_fill_brewer() +
  geom_vline(xintercept = 0, linetype = "dashed")

<<<<<<< HEAD
formRC2 <- bf(COUNT | trials(TOTAL) ~ site_management + (1 | site_name),
              family = binomial(link = "logit"))

=======
<<<<<<< HEAD

##model by transect
=======
<<<<<<< HEAD
formRC2 <- bf(COUNT | trials(TOTAL) ~ site_management + (1 | site_name),
              family = binomial(link = "logit"))
=======
>>>>>>> 5b7437daa1faff99545cc9025a51248043a1b22f
>>>>>>> f32cb717e682533a41ddace46e3dcd4f651e2882
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

##formula to get the exact prior that you need
qlogis(0.128)

priors <- prior(normal(-1.91, 1), class = "Intercept") +
  prior(normal(0,1), class = "b") +
  prior(student_t(3,0,1), class = "sd")
## ----end

<<<<<<< HEAD
## ---- model
=======

>>>>>>> f32cb717e682533a41ddace46e3dcd4f651e2882
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



##to run the plot from the model
model_RC1a |> conditional_effects() |> plot() 

##
model_RC2a |> SUYR_prior_and_posterior()

model_RC1a <- model_RC1a |> 
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

## ---- PlottingDiveLocations
data_rc_cover_site |> ggplot(aes(y = site_latitude, x = site_longitude)) +
  geom_point()
## ----end

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

## ----LoadReefData
reef <- sf::read_sf("../Pal_Test_Repo/data/GIS/reef_500_poly.shp")
## ----end

## ----createInsetMap
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
