library(tidyverse)

data_rc <- read.csv("../data/primary/data-coral-cover.csv")
labelset_rc <- read.csv("../data/primary/data-coral-cover-labelset.csv")

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

data_rc_cover <- 
  data_rc_cover |>
  ungroup(image_id) |>
  summarise(COUNT = sum(COUNT),
            TOTAL = sum(TOTAL)
  ) |> 
  ungroup() 

data_rc_cover <- 
  data_rc_cover |> 
  rename(data_tally_group = GROUP,
         count_groupcode  = COUNT,
         total = TOTAL)

data_rc_cover |> as.data.frame() |> glimpse()

data_rc_cover <- 
  data_rc_cover |> 
  filter(data_tally_group == "HC") |>
  mutate(cover = count_groupcode / total) |>
  mutate(date = as.Date(survey_start_date,"%Y-%m-%d"))

location_lookup <- tribble(
  ~site_reef_name, ~Side,
  "Aquarium Reef", "West",
  "Fantastic Reef", "West",
  "Manta Ray Reef", "West",
  "Cambari Reef", "East",
  "Catad Reef", "East",
  "Langoy Reef", "East",
  "Snake Island - S3", "East",
  "Helicopter Island Reef", "West"
)
data_rc_cover <- 
  data_rc_cover |> 
  left_join(location_lookup,
            by="site_reef_name")

 # filter(type == "point_machine_classification") |>
 # mutate(cover = count_groupcode / total * 100) %>% 
 # mutate(date = as.Date(survey_start_date, "%Y-%m-%d"))


data_rc_cover |> as.data.frame() |> glimpse()


data_rc_cover |>
  filter(type=="point_machine_classification") |>
  ggplot(aes(y = cover, x = date, colour=site_reef_name)) +
  geom_line()+
  geom_point()

data_rc_cover |>
  filter(type=="point_machine_classification") |>
  ggplot(aes(y = cover, x = date, colour=site_reef_type, group=site_reef_zone)) +
  geom_line()+
  geom_point()

data_rc_cover |>
  filter(type=="point_machine_classification") |>
  ggplot(aes(y = cover, x = date, colour=Side, group=site_reef_zone)) +
  geom_line()+
  geom_point()

data_rc_cover |>
  filter(type=="point_machine_classification") |>
  ggplot(aes(y = cover, x = Side)) +
  geom_point()

write_csv(data_rc_cover, file="../data/processed/coral_cover_rc.csv")

data_rc_cover %>%
  ggplot(aes (y = cover, x = survey_start_date)) +
  geom_point()

data_rc_cover %>% 
  filter(type=="point_machine_classification") %>% 
  ggplot(aes(y = cover, x = date, colours=site_reef_type, group=site_reef_zone)) +
  geom_line()+
  geom_point()

