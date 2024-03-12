#to process the data
load(file = "../data/primary/q6_data.Rdata")

#Data_T1
data_T1 <- read_csv("../data/primary/DiveSite_AbdeensRock_T1.csv")
glimpse(data_T1)
data_T1 <- data_T1 |>
  cpce_classif_to_points() |>
  separate(`Frame image name`,
           into = c("Drive", "A", "Site", "Transect", "Photo"),
           sep = "\\\\"
  ) |>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = `Major Category`)|>
  dplyr::select(-Drive, -A)|>
  mutate('Tourist Access'= "Yes")
save(data, file = "../data/processed/Q6_data_T1.Rdata")

#Data_T2
data_T2 <- read_csv("../data/primary/DiveSite_AbdeensRock_T2.csv")
glimpse(data_T2)
data_T2 <- data_T2 |>
  cpce_classif_to_points() |>
  separate(`Frame image name`,
           into = c("Drive", "A", "Site", "Transect", "Photo"),
           sep = "\\\\"
  ) |>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = `Major Category`)|>
  dplyr::select(-Drive, -A)|>
  mutate('Tourist Access'= "Yes")
save(data, file = "../data/processed/Q6_data_T2.Rdata")

#Data_T3
data_T3 <- read_csv("../data/primary/DiveSite_AbdeensRock_T3.csv")
glimpse(data_T3)
data_T3 <- data_T3 |>
  cpce_classif_to_points() |>
  separate(`Frame image name`,
           into = c("Drive", "A", "Site", "Transect", "Photo"),
           sep = "\\\\"
  ) |>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = `Major Category`)|>
  dplyr::select(-Drive, -A)|>
  mutate('Tourist Access'= "Yes")
save(data, file = "../data/processed/Q6_data_T3.Rdata")

#Data_PT1
data_PT1 <- read_csv("../data/primary/Protected_MitriRock_T1.csv")
glimpse(data_PT1)
data_PT1 <- data_PT1 |>
  cpce_classif_to_points() |>
  separate(`Frame image name`,
           into = c("Drive", "A", "B", "C", "Site", "Transect", "Photo"),
           sep = "\\\\"
  ) |>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = `Major Category`)|>
  dplyr::select(-Drive, -A, -B, -C)|>
  mutate('Tourist Access'= "No")
save(data, file = "../data/processed/Q6_data_PT1.Rdata")

#Data_PT2
data_PT2 <- read_csv("../data/primary/Protected_MitriRock_T2.csv")
glimpse(data_PT2)

data_PT2 <- data_PT2 |>
  cpce_classif_to_points() |>
  separate(`Frame image name`,
           into = c("Drive", "A", "B", "C", "Site", "Transect", "Photo"),
           sep = "\\\\"
  ) |>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = `Major Category`)|>
  dplyr::select(-Drive, -A, -B, -C)|>
  mutate('Tourist Access'= "No")
save(data, file = "../data/processed/Q6_data_PT2.Rdata")

#Data_PT3
data_PT3 <- read_csv("../data/primary/Protected_MitriRock_T3.csv")
glimpse(data_PT3)

data_PT3 <- data_PT3 |>
  cpce_classif_to_points() |>
  separate(`Frame image name`,
           into = c("Drive", "Folder", "Project", "Something", "Site", "Transect", "Photo"),
           sep = "\\\\"
  ) |>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = `Major Category`)|>
  dplyr::select(-Drive, -Folder, -Project, -Something)|>
  mutate('Tourist Access'= "No")
save(data, file = "../data/processed/Q6_data_PT3.Rdata")

#data2_T1
data2_T1 <- read_csv("../data/primary/Tourist_Helicopter_T1.csv")
glimpse(data2_T1)

data2_T1 <- data2_T1 |>
  cpce_classif_to_points() |>
  separate(`Frame image name`,
           into = c("Drive", "A", "Site", "Transect", "Photo"),
           sep = "\\\\"
  ) |>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = `Major Category`)|>
  dplyr::select(-Drive, -A)|>
  mutate('Tourist Access'= "Yes")
save(data, file = "../data/processed/Q6_data2_T1.Rdata")

#data2_T2
data2_T2 <- read_csv("../data/primary/Tourist_Helicopter_T2.csv")
glimpse(data2_T2)

data2_T2 <- data2_T2 |>
  cpce_classif_to_points() |>
  separate(`Frame image name`,
           into = c("Drive", "A", "Site", "Transect", "Photo"),
           sep = "\\\\"
  ) |>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = `Major Category`)|>
  dplyr::select(-Drive, -A)|>
  mutate('Tourist Access'= "Yes")
save(data, file = "../data/processed/Q6_data2_T2.Rdata")

#data2_T3
data2_T3 <- read_csv("../data/primary/Tourist_Helicopter_T3.csv")
glimpse(data2_T3)

data2_T3 <- data2_T3 |>
  cpce_classif_to_points() |>
  separate(`Frame image name`,
           into = c("Drive", "A", "Site", "Transect", "Photo"),
           sep = "\\\\"
  ) |>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = `Major Category`)|>
  dplyr::select(-Drive, -A)|>
  mutate('Tourist Access'= "Yes")
save(data, file = "../data/processed/Q6_data2_T3.Rdata")

#data2_PT1
data2_PT1 <- read_csv("../data/primary/Pagawanen_T1.csv")
glimpse(data2_PT1)

data2_PT1 <- data2_PT1 |>
  cpce_classif_to_points() |>
  separate(`Frame image name`,
           into = c("Drive", "A", "B", "C", "D", "E", "F", "Site", "Transect", "Photo"),
           sep = "\\\\"
  ) |>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = `Major Category`)|>
  dplyr::select(-Drive, -A, -B, -C, -D, -E, -F)|>
  mutate('Tourist Access'= "No")
save(data, file = "../data/processed/Q6_data2_PT1.Rdata")

#data2_PT2
data2_PT2 <- read_csv("../data/primary/Pagawanen_T2.csv")
glimpse(data2_PT2)

data2_PT2 <- data2_PT2 |>
  cpce_classif_to_points() |>
  separate(`Frame image name`,
           into = c("Drive", "A", "B", "C", "D", "E", "F", "Site", "Transect", "Photo"),
           sep = "\\\\"
  ) |>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = `Major Category`)|>
  dplyr::select(-Drive, -A, -B, -C, -D, -E, -F)|>
  mutate('Tourist Access'= "No")
save(data, file = "../data/processed/Q6_data2_PT2.Rdata")

#data2_PT3
data2_PT3 <- read_csv("../data/primary/Pagawanen_T3.csv")
glimpse(data2_PT3)

data2_PT3 <- data2_PT3 |>
  cpce_classif_to_points() |>
  separate(`Frame image name`,
           into = c("Drive", "A", "B", "C", "D", "E", "F", "Site", "Transect", "Photo"),
           sep = "\\\\"
  ) |>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = `Major Category`)|>
  dplyr::select(-Drive, -A, -B, -C, -D, -E, -F)|>
  mutate('Tourist Access'= "No")
save(data, file = "../data/processed/Q6_data2_PT3.Rdata")

#to bind all the data
all_data <- bind_rows(data_T1,
                      data_T2,
                      data_T3,
                      data_PT1,
                      data_PT2,
                      data_PT3,
                      data2_T1,
                      data2_T2,
                      data2_T3,
                      data2_PT1,
                      data2_PT2,
                      data2_PT3)|>
  dplyr::rename(tourist_access = `Tourist Access`)
save(all_data, file = "../data/processed/Q6_all_data_Arvil.Rdata")
