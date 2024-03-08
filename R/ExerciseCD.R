library(tidyverse) ##package tidyverse used for manipulating data

source("functions.R")

## Read in the data from the primary data folder (Transect1)
data1 <- read_csv("../data/primary/Siete Picados_T1.csv")

##traditional way of using a function
glimpse(data1)
##modern way of using a function
data1 |> glimpse()
data1

data1 <- data1 |>
  cpce_classif_to_points() |>
  separate(`Frame image name`,
           into = c("Drive", "Folder", "Project", "Something", "Site", "Transect", "Photo"),
           sep = "\\\\"
  ) |>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = `Major Category`)


##Data2 for Transect2
data2 <- read_csv("../data/primary/Siete Picados_T2.csv")

glimpse(data2)
data2

data2 <- data2 |>
  cpce_classif_to_points() |>
  separate(`Frame image name`,
           into = c("Drive", "Folder", "Project", "Something", "Site", "Transect", "Photo"),
           sep = "\\\\"
  ) |>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = `Major Category`)

##Data3 for Transect3
data3 <- read_csv("../data/primary/Siete Picados_T3.csv")

glimpse(data3)
data3

data3 <- data3 |>
  cpce_classif_to_points() |>
  separate(`Frame image name`,
           into = c("Drive", "Folder", "Project", "Something", "Site", "Transect", "Photo"),
           sep = "\\\\"
  ) |>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = `Major Category`)

####Data4 for Transect4
data4 <- read_csv("../data/primary/Siete Picados_T4.csv")

glimpse(data4)
data4

data4 <- data4 |>
  cpce_classif_to_points() |>
  separate(`Frame image name`,
           into = c("Drive", "Folder", "Project", "Something", "Site", "Transect", "Photo"),
           sep = "\\\\"
  ) |>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = `Major Category`)

