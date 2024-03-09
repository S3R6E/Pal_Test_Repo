library(tidyverse) ##package tidyverse used for manipulating data

setwd("R")


source("functions.R")

## Read in the data from the primary data folder (Transect1)
data1 <- read_csv("../data/primary/Siete Picados_T1.csv")

##traditional way of using a function
glimpse(data1)
##modern way of using a function
data1 |> glimpse()
data1

##hard coral cover1 
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
  
##New function showing richness  
data1a <- data1 |> 
  filter(`Major Category`=="HC") |> 
  group_by(Photo, Site, Transect) |> 
  summarise(Richness = length(unique(data_tally_group)))


##Data2 for Transect2
data2 <- read_csv("../data/primary/Siete Picados_T2.csv")

glimpse(data2)
data2

data2 <- data2 |>
  cpce_raw_classif_to_points() |>
  separate(`Frame image name`,
           into = c("Drive", "Folder", "Project", "Something", "Site", "Transect", "Photo"),
           sep = "\\\\"
  ) |>
  #filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = `Raw Data`)

##New function showing richness  
data2a <- data2 |> 
  filter(`Major Category`=="HC") |> 
  group_by(Photo, Site, Transect) |> 
  summarise(Richness = length(unique(data_tally_group)))

##Data3 for Transect3
data3 <- read_csv("../data/primary/Siete Picados_T3.csv")

glimpse(data3)
data3

data3 <- data3 |>
  cpce_raw_classif_to_points() |>
  separate(`Frame image name`,
           into = c("Drive", "Folder", "Project", "Something", "Site", "Transect", "Photo"),
           sep = "\\\\"
  ) |>
  #filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = `Raw Data`)

##New function showing richness  
data3a <- data3 |> 
  filter(`Major Category`=="HC") |> 
  group_by(Photo, Site, Transect) |> 
  summarise(Richness = length(unique(data_tally_group)))

####Data4 for Transect4
data4 <- read_csv("../data/primary/Siete Picados_T4.csv")

glimpse(data4)
data4

data4 <- data4 |>
  cpce_raw_classif_to_points() |>
  separate(`Frame image name`,
           into = c("Drive", "Folder", "Project", "Something", "Site", "Transect", "Photo"),
           sep = "\\\\"
  ) |>
  #filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = `Raw Data`)

##New function showing richness  
data4a <- data4 |> 
  filter(`Major Category`=="HC") |> 
  group_by(Photo, Site, Transect) |> 
  summarise(Richness = length(unique(data_tally_group)))

data <-bind_rows(data1,
                 data2,
                 data3,
                 data4)

datarich <-bind_rows(data1a,
                 data2a,
                 data3a,
                 data4a)

