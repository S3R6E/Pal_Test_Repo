library(tidyverse)

source("functions.R")

## Transect 1
## Read in the data from the primary data folder
SPT1 <- read_csv("../data/primary/Siete Picados_T1.csv")

##traditional way 
glimpse(SPT1)

##modern way
data |> glimpse (SPT1) 
data

SPT1 <- SPT1 |>
  cpce_classif_to_points() |>
  separate(`Frame image name`,
           into = c("Drive", "Folder", "Project", "Something", "Site", "Transect", "Photo"),
           sep = "\\\\"
  ) |>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = `Major Category`)

data1 <- data1 |>
  cpce_raw_classif_to_points() |>
  separate(`Frame image name`,
           into = c("Drive", "Folder", "Project", "Something", "Site", "Transect", "Photo"),
           sep = "\\\\"
  ) |>
  #filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = `Raw Data`)


data1a <- data1 |> 
  filter(`Major Category`=="HC") |> 
  group_by(Photo, Site, Transect) |> 
  summarise(Richness = length(unique(data_tally_group)))


##transect 2
SPT2 <- read_csv("../data/primary/Siete Picados_T2.csv")

glimpse(SPT2)

SPT2 <- SPT2 |>
  cpce_classif_to_points() |>
  separate(`Frame image name`,
           into = c("Drive", "Folder", "Project", "Something", "Site", "Transect", "Photo"),
           sep = "\\\\"
  ) |>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = `Major Category`)

## Transect 3
SPT3 <- read_csv("../data/primary/Siete Picados_T3.csv")

glimpse(SPT3)

SPT3 <- SPT3 |>
  cpce_classif_to_points() |>
  separate(`Frame image name`,
           into = c("Drive", "Folder", "Project", "Something", "Site", "Transect", "Photo"),
           sep = "\\\\"
  ) |>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = `Major Category`)

## Transect 4
SPT4 <- read_csv("../data/primary/Siete Picados_T4.csv")

glimpse(SPT4)

SPT4 <- SPT4 |>
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