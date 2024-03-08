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


## Transect 4

data |> as.data.frame() |> head()