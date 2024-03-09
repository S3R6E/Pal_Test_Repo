setwd("R")


##Transect1
load(file = "../data/primary/q2_data1.RData")

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

save(data1, file = "../data/processed/q2_data1.RData")

##New function showing richness transect1 
data1a <- data1 |> 
  filter(`Major Category`=="HC") |> 
  group_by(Photo, Site, Transect) |> 
  summarise(Richness = length(unique(data_tally_group)))


save(data1a, file = "../data/processed/q2_data1a.RData")


##Transect2
load(file = "../data/primary/q2_data2.RData")

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

save(data2, file = "../data/processed/q2_data2.RData")

##New function showing richness transect2
data2a <- data2 |> 
  filter(`Major Category`=="HC") |> 
  group_by(Photo, Site, Transect) |> 
  summarise(Richness = length(unique(data_tally_group)))

save(data2a, file = "../data/processed/q2_data2a.RData")

##Transect3
load(file = "../data/primary/q2_data3.RData")

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

save(data3, file = "../data/processed/q2_data3.RData")

##New function showing richness transect3
data3a <- data3 |> 
  filter(`Major Category`=="HC") |> 
  group_by(Photo, Site, Transect) |> 
  summarise(Richness = length(unique(data_tally_group)))

save(data3a, file = "../data/processed/q2_data3a.RData")

##Transect4
load(file = "../data/primary/q2_data4.RData")

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

save(data4, file = "../data/processed/q2_data4.RData")

##New function showing richness  
data4a <- data4 |> 
  filter(`Major Category`=="HC") |> 
  group_by(Photo, Site, Transect) |> 
  summarise(Richness = length(unique(data_tally_group)))

save(data4a, file = "../data/processed/q2_data4a.RData")

##All 4 transects merged

data <-bind_rows(data1,
                 data2,
                 data3,
                 data4)
save(data, file = "../data/processed/q2_data.RData")

datarich <-bind_rows(data1a,
                     data2a,
                     data3a,
                     data4a)

save(datarich, file = "../data/processed/q2_datarich.RData")

