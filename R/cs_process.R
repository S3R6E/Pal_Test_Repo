rm(list=ls())
library(tidyverse)

## Case Study Processing ####
cs_data <- read.csv("../data/primary/cs_data.csv")
cs_label <- read.csv("../data/primary/cs_label.csv")

cs_data |> glimpse()

cs_data <- cs_data |> 
  dplyr::select(
    project_name,
    site_id,
    site_name,
    site_latitude,
    site_longitude,
    site_reef_name,
    site_management,
    survey_id,
    survey_title,
    survey_start_date..UTC.,
    survey_depth,
    survey_transect_number, 
    image_id,
    image_disabled,
    point_machine_classification,
    point_human_classification) |>
  rename(survey_start_date = survey_start_date..UTC.,
         label=point_machine_classification) |> 
  dplyr::filter(image_disabled == "FALSE") |> 
  select(-image_disabled)

cs_data <-
  cs_data |>
  left_join(cs_label |>
              dplyr::select(CODE, GROUP = FUNCTIONAL.GROUP),
            by = c("label" = "CODE")
  ) |> 
  mutate(transect_name = paste(site_name, year(survey_start_date), survey_transect_number, sep ="_"),
         transect_id = paste0(site_id, year(survey_start_date), survey_transect_number)) |>
  mutate(year = lubridate::year(survey_start_date))

# pivot class
cs_data <- cs_data |> 
  pivot_longer(cols = matches("point_.*_classification"),
               names_to = "type",
               values_to = "classification"
  ) 

# join labelset
cs_data <-
  cs_data |>
  left_join(cs_label |>
              dplyr::select(CODE, GROUP = `FUNCTIONAL.GROUP`),
            by = c("classification" = "CODE")
  )

# recode
cs_data <- 
  cs_data |> 
  mutate(transect_name = paste(site_name, year(survey_start_date), survey_transect_number, sep ="_"),
         transect_id = paste0(site_id, year(survey_start_date), survey_transect_number))

# mutate year
cs_data <-
  cs_data |>
  mutate(year = lubridate::year(survey_start_date))

# count cover
cs_cover <- 
  cs_data |> 
  group_by(across(c(starts_with("site"),
                    starts_with("survey"),
                    starts_with("transect"),
                    year,
                    image_id,
                    type,
                    classification,
                    GROUP.y))
  ) |>
  summarise(COUNT = n(), .groups = "keep") |> 
  ungroup(GROUP.y) |>
  mutate(TOTAL = sum(COUNT)) |>
  ungroup() 

GROUP.y <- cs_cover |> pull(GROUP.y) |> unique()
filler <- cs_cover %>%
  dplyr::select(
    starts_with("site"),
    survey_start_date,
    survey_depth,
    transect_name,
    image_id,
    transect_id,
    type,
    TOTAL) |> 
  distinct() |> 
  tidyr::crossing(GROUP.y = GROUP.y) 

cs_cover <-
  cs_cover |> 
  full_join(filler) |>
  group_by(
    across(c(starts_with("site"),
             survey_start_date,
             survey_depth,
             transect_name,
             image_id,
             transect_id,
             type,
             GROUP.y
    ))) |> 
  mutate(COUNT = ifelse(is.na(COUNT), 0, COUNT),
         TOTAL = max(TOTAL, na.rm = TRUE)
  )

cs_cover <- 
  cs_cover |>
  summarise(COUNT = sum(COUNT),
            TOTAL = sum(TOTAL)
  ) |> 
  ungroup() 

cs_cover |> as.data.frame() |> head()

save(cs_cover, file = "../data/processed/cs_cover.RData")

# TAUs
cs_tau <- 
  cs_data |> 
  group_by(across(c(starts_with("site"),
                    starts_with("survey"),
                    starts_with("transect"),
                    year,
                    type,
                    image_id,
                    classification))
  ) |>
  summarise(COUNT = n(), .groups = "keep") |> 
  ungroup(classification) |>
  mutate(TOTAL = sum(COUNT)) |>
  ungroup() 

TAUS <- cs_tau |> pull(classification) |> unique()
filler.tau <- cs_tau %>%
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
  tidyr::crossing(classification = TAUS) 

cs_tau <-
  cs_tau |> 
  full_join(filler.tau) |>
  group_by(
    across(c(starts_with("site"),
             survey_start_date,
             survey_depth,
             transect_name,
             transect_id,
             image_id,
             type,
             classification
    ))) |> 
  mutate(COUNT = ifelse(is.na(COUNT), 0, COUNT),
         TOTAL = max(TOTAL, na.rm = TRUE)
  )

cs_tau <- 
  cs_tau |>
  summarise(COUNT = sum(COUNT),
            TOTAL = sum(TOTAL)
  ) |> 
  ungroup() 

cs_tau |> as.data.frame() |> head()

save(cs_tau, file="../data/processed/cs_tau.RData")
