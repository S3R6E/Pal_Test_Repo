##To process the data
load(file = "../data/primary/q6_data.Rdata")

##Transect 1
data <- data |>
  cpce_classif_to_points() |>
  separate(`Frame image name`,
           into = c("Drive", "Folder",  "Site", "Transect", "Photo"),
           sep = "\\\\"
  ) |>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = `Major Category`)|>
  dplyr::select(-Drive, -Folder) |>
  mutate('Tourist Access'= "yes")
save(data, file = "../data/processed/q6_data.Rdata")

##Transect 2
load(file = "../data/primary/q6_data_T2.Rdata")
data_T2 <- data_T2 |>
  cpce_classif_to_points() |>
  separate(`Frame image name`,
           into = c("Drive", "Folder",  "Site", "Transect", "Photo"),
           sep = "\\\\"
  ) |>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = `Major Category`) |>
  dplyr::select(-Drive, -Folder)|>
  mutate('Tourist Access'= "yes")
save(data, file = "../data/processed/q6_data_T2.Rdata")

##Transect 3
load(file = "../data/primary/q6_data_T3.Rdata")
data_T3 <- data_T3 |>
  cpce_classif_to_points() |>
  separate(`Frame image name`,
           into = c("Drive", "Folder",  "Site", "Transect", "Photo"),
           sep = "\\\\"
  ) |>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = `Major Category`)|>
  dplyr::select(-Drive, -Folder)|>
  mutate('Tourist Access'= "yes")
save(data, file = "../data/processed/q6_data_T3.Rdata")


##Protected Transect 1
load(file = "../data/primary/q6_data_PT1.Rdata")
data_PT1 <- data_PT1 |>
  cpce_classif_to_points() |>
  separate(`Frame image name`,
           into = c("Drive", "Folder", "A", "B", "Site", "Transect", "Photo"),
           sep = "\\\\"
  ) |>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = `Major Category`)|>
  dplyr::select(-Drive, -Folder,-A,-B)|>
  mutate('Tourist Access'= "no")
save(data, file = "../data/processed/q6_data_PT1.Rdata")

##Protected Transect 2
load(file = "../data/primary/q6_data_PT2.Rdata")
data_PT2 <- data_PT2 |>
  cpce_classif_to_points() |>
  separate(`Frame image name`,
           into = c("Drive", "A", "B", "Folder",  "Site", "Transect", "Photo"),
           sep = "\\\\"
  ) |>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = `Major Category`)|>
  dplyr::select(-Drive, -Folder,-A,-B)|>
  mutate('Tourist Access'= "no")
save(data, file = "../data/processed/q6_data_PT2.Rdata")


##Protected Transect 3
load(file = "../data/primary/q6_data_PT3.Rdata")
data_PT3 <- data_PT3 |>
  cpce_classif_to_points() |>
  separate(`Frame image name`,
           into = c("Drive", "A", "B", "Folder",  "Site", "Transect", "Photo"),
           sep = "\\\\"
  ) |>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = `Major Category`) |>
  dplyr::select(-Drive, -Folder,-A,-B)|>
  mutate('Tourist Access'= "no")
save(data, file = "../data/processed/q6_data_PT3.Rdata")


## to bind all the data
all_data <- bind_rows(data,
                      data_T2,
                      data_T3,
                      data_PT1,
                      data_PT2,
                      data_PT3)
save(all_data, file = "../data/processed/q6_all_data.Rdata")


