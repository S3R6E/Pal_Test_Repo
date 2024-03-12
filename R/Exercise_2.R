library(tidyverse) 

source ("functions.R")

## to read files

data_HeliC23_T1 <- read_csv("../data/primary/Helicopter_corals1_23.csv")
data_HeliC23_T2 <- read_csv("../data/primary/Helicopter_corals2_23.csv")
data_HeliC23_T3 <- read_csv("../data/primary/Helicopter_corals3_23.csv")
data_PagaC23_T1 <- read_csv("../data/primary/Pagawanen_corals1_23.csv")
data_PagaC23_T2 <- read_csv("../data/primary/Pagawanen_corals2_23.csv")
data_PagaC23_T3 <- read_csv("../data/primary/Pagawanen_corals3_23.csv")


data_HeliC23_T1 <- read_csv("../data/primary/Helicopter_corals1_23.csv")
glimpse(data_HeliC23_T1)

data_HeliC23_T1 <- data_HeliC23_T1 |>
  cpce_classif_to_points() |>
  separate('Frame image name',
           into = c("Drive", "A","Site","Transect","Photo"),
           sep = "\\\\"
  )|>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = 'Major Category')|>
  dplyr::select(-Drive, -A)|>
  mutate('tourist_access'= "Yes") |> 
  mutate('Year'= "2023")


data_HeliC23_T2 <- read_csv("../data/primary/Helicopter_corals2_23.csv")

glimpse(data_HeliC23_T2)

data_HeliC23_T2 <- data_HeliC23_T2 |>
  cpce_classif_to_points() |>
  separate('Frame image name',
           into = c("Drive", "A","Site","Transect","Photo"),
           sep = "\\\\"
  )|>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = 'Major Category')|>
  dplyr::select(-Drive, -A)|>
  mutate('tourist_access'= "Yes") |> 
  mutate('Year'= "2023")

data_HeliC23_T3 <- read_csv("../data/primary/Helicopter_corals3_23.csv")
glimpse(data_HeliC23_T3)

data_HeliC23_T3 <- data_HeliC23_T3 |>
  cpce_classif_to_points() |>
  separate('Frame image name',
           into = c("Drive", "A","Site","Transect","Photo"),
           sep = "\\\\"
  )|>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = 'Major Category')|>
  dplyr::select(-Drive, -A)|>
  mutate('tourist_access'= "Yes") |> 
  mutate('Year'= "2023")

data_PagaC23_T1 <- read_csv("../data/primary/Pagawanen_corals1_23.csv")
glimpse(data_PagaC23_T1)

data_PagaC23_T1 <- data_PagaC23_T1 |>
  cpce_classif_to_points() |>
  separate('Frame image name',
           into = c("Drive", "A","Site","Transect","Photo"),
           sep = "\\\\"
  )|>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = 'Major Category')|>
  dplyr::select(-Drive, -A)|>
  mutate('tourist_access'= "Yes") |> 
  mutate('Year'= "2023")

data_PagaC23_T2 <- read_csv("../data/primary/Pagawanen_corals2_23.csv")
glimpse(data_PagaC23_T2)

data_PagaC23_T2 <- data_PagaC23_T2 |>
  cpce_classif_to_points() |>
  separate('Frame image name',
           into = c("Drive", "A","Site","Transect","Photo"),
           sep = "\\\\"
  )|>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = 'Major Category')|>
  dplyr::select(-Drive, -A)|>
  mutate('tourist_access'= "Yes") |> 
  mutate('Year'= "2023")

data_PagaC23_T3 <- read_csv("../data/primary/Pagawanen_corals3_23.csv")
glimpse(data_PagaC23_T3)

data_PagaC23_T3 <- data_PagaC23_T3 |>
  cpce_classif_to_points() |>
  separate('Frame image name',
           into = c("Drive", "A","Site","Transect","Photo"),
           sep = "\\\\"
  )|>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = 'Major Category')|>
  dplyr::select(-Drive, -A)|>
  mutate('tourist_access'= "Yes") |> 
  mutate('Year'= "2023")



data_HeliC22_T1 <- read_csv("../data/primary/Tourist_Helicopter_T1.csv")
data_HeliC22_T2 <- read_csv("../data/primary/Tourist_Helicopter_T2.csv")
data_HeliC22_T3 <- read_csv("../data/primary/Tourist_Helicopter_T3.csv")
data_PagaC22_T1 <- read_csv("../data/primary/Pagawanen_T1.csv")
data_PagaC22_T2 <- read_csv("../data/primary/Pagawanen_T2.csv")
data_PagaC22_T3 <- read_csv("../data/primary/Pagawanen_T3.csv")


data_HeliC22_T1 <- read_csv("../data/primary/Tourist_Helicopter_T1.csv")
glimpse(data_HeliC22_T1)

data_HeliC22_T1 <- data_HeliC22_T1 |>
  cpce_classif_to_points() |>
  separate('Frame image name',
           into = c("Drive", "A","Site","Transect","Photo"),
           sep = "\\\\"
  )|>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = 'Major Category')|>
  dplyr::select(-Drive, -A)|>
  mutate('tourist_access'= "Yes") |> 
  mutate('Year'= "2022")


data_HeliC22_T2 <- read_csv("../data/primary/Tourist_Helicopter_T2.csv")

glimpse(data_HeliC22_T2)

data_HeliC22_T2 <- data_HeliC22_T2 |>
  cpce_classif_to_points() |>
  separate('Frame image name',
           into = c("Drive", "A","Site","Transect","Photo"),
           sep = "\\\\"
  )|>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = 'Major Category')|>
  dplyr::select(-Drive, -A)|>
  mutate('tourist_access'= "Yes") |> 
  mutate('Year'= "2022")

data_HeliC22_T3 <- read_csv("../data/primary/Tourist_Helicopter_T3.csv")
glimpse(data_HeliC22_T3)



data_HeliC22_T3 <- data_HeliC22_T3 |>
  cpce_classif_to_points() |>
  separate('Frame image name',
           into = c("Drive", "A","Site","Transect","Photo"),
           sep = "\\\\"
  )|>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = 'Major Category')|>
  dplyr::select(-Drive, -A)|>
  mutate('tourist_access'= "Yes") |> 
  mutate('Year'= "2022")

data_PagaC22_T1 <- read_csv("../data/primary/Pagawanen_T1.csv")
glimpse(data_PagaC22_T1)

data_PagaC22_T1 <- data_PagaC22_T1 |>
  cpce_classif_to_points() |>
  separate('Frame image name',
           into = c("Drive", "A","Site","Transect","Photo"),
           sep = "\\\\"
  )|>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = 'Major Category')|>
  dplyr::select(-Drive, -A)|>
  mutate('tourist_access'= "Yes") |> 
  mutate('Year'= "2022")

data_PagaC22_T2 <- read_csv("../data/primary/Pagawanen_T2.csv")
glimpse(data_PagaC22_T2)

data_PagaC22_T2 <- data_PagaC22_T2 |>
  cpce_classif_to_points() |>
  separate('Frame image name',
           into = c("Drive","A","B","C","D", "E", "Site","Transect","Photo"),
           sep = "\\\\"
  )|>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = 'Major Category')|>
  dplyr::select(-Drive, -A, -B, -C, -D, -E,)|>
  mutate('tourist_access'= "Yes") |> 
  mutate('Year'= "2022")

data_PagaC22_T3 <- read_csv("../data/primary/Pagawanen_T3.csv")
glimpse(data_PagaC22_T3)

data_PagaC22_T3 <- data_PagaC22_T3 |>
  cpce_classif_to_points() |>
  separate('Frame image name',
           into = c("Drive", "A","Site","Transect","Photo"),
           sep = "\\\\"
  )|>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = 'Major Category')|>
  dplyr::select(-Drive, -A)|>
  mutate('tourist_access'= "Yes") |> 
  mutate('Year'= "2022")

All_Data2 <- bind_rows(data_HeliC22_T1,
                      data_HeliC22_T2,
                      data_HeliC22_T3,
                      data_HeliC23_T1,
                      data_HeliC23_T2,
                      data_HeliC23_T3,
                      data_PagaC22_T1,
                      data_PagaC22_T2,
                      data_PagaC22_T3,
                      data_PagaC23_T1,
                      data_PagaC23_T2,
                      data_PagaC23_T3)

save(All_Data2, file = "../data/primary/All_Data2.Rdata")


load(file = "../data/primary/All_Data2.Rdata")
