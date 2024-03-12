library(tidyverse)

source ("../Pal_Test_Repo/R/functions.R")

## to read files

data_HeliC23_T1 <- read_csv("../Pal_Test_Repo/data/primary/Helicopter_corals1_23.csv")
data_HeliC23_T2 <- read_csv("../Pal_Test_Repo/data/primary/Helicopter_corals2_23.csv")
data_HeliC23_T3 <- read_csv("../Pal_Test_Repo/data/primary/Helicopter_corals3_23.csv")
data_PagaC23_T1 <- read_csv("../Pal_Test_Repo/data/primary/Pagawanen_corals1_23.csv")
data_PagaC23_T2 <- read_csv("../Pal_Test_Repo/data/primary/Pagawanen_corals2_23.csv")
data_PagaC23_T3 <- read_csv("../Pal_Test_Repo/data/primary/Pagawanen_corals3_23.csv")


data_HeliC23_T1 <- read_csv("../Pal_Test_Repo/data/primary/Helicopter_corals1_23.csv")
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


data_HeliC23_T2 <- read_csv("../Pal_Test_Repo/data/primary/Helicopter_corals2_23.csv")

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

data_HeliC23_T3 <- read_csv("../Pal_Test_Repo/data/primary/Helicopter_corals3_23.csv")
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

data_PagaC23_T1 <- read_csv("../Pal_Test_Repo/data/primary/Pagawanen_corals1_23.csv")
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
  mutate('tourist_access'= "No") |> 
  mutate('Year'= "2023")

data_PagaC23_T2 <- read_csv("../Pal_Test_Repo/data/primary/Pagawanen_corals2_23.csv")
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
  mutate('tourist_access'= "No") |> 
  mutate('Year'= "2023")

data_PagaC23_T3 <- read_csv("../Pal_Test_Repo/data/primary/Pagawanen_corals3_23.csv")
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
  mutate('tourist_access'= "No") |> 
  mutate('Year'= "2023")



data_HeliC22_T1 <- read_csv("../Pal_Test_Repo/data/primary/Tourist_Helicopter_T1.csv")
data_HeliC22_T2 <- read_csv("../Pal_Test_Repo/data/primary/Tourist_Helicopter_T2.csv")
data_HeliC22_T3 <- read_csv("../Pal_Test_Repo/data/primary/Tourist_Helicopter_T3.csv")
data_PagaC22_T1 <- read_csv("../Pal_Test_Repo/data/primary/Pagawanen_T1.csv")
data_PagaC22_T2 <- read_csv("../Pal_Test_Repo/data/primary/Pagawanen_T2.csv")
data_PagaC22_T3 <- read_csv("../Pal_Test_Repo/data/primary/Pagawanen_T3.csv")


data_HeliC22_T1 <- read_csv("../Pal_Test_Repo/data/primary/Tourist_Helicopter_T1.csv")
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


data_HeliC22_T2 <- read_csv("../Pal_Test_Repo/data/primary/Tourist_Helicopter_T2.csv")

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

data_HeliC22_T3 <- read_csv("../Pal_Test_Repo/data/primary/Tourist_Helicopter_T3.csv")
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

data_PagaC22_T1 <- read_csv("../Pal_Test_Repo/data/primary/Pagawanen_T1.csv")
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
  mutate('tourist_access'= "No") |> 
  mutate('Year'= "2022")

data_PagaC22_T2 <- read_csv("../Pal_Test_Repo/data/primary/Pagawanen_T2.csv")
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
  mutate('tourist_access'= "No") |> 
  mutate('Year'= "2022")

data_PagaC22_T3 <- read_csv("../Pal_Test_Repo/data/primary/Pagawanen_T3.csv")
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
  mutate('tourist_access'= "No") |> 
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

save(All_Data2, file = "../Pal_Test_Repo/data/primary/All_Data2.Rdata")
load(file = "../Pal_Test_Repo/data/primary/All_Data2.Rdata")


##Tally up Points
#| label: count
All_Data2 <-
  All_Data2 |> 
  dplyr::group_by(across(
    c(starts_with("Site"),
      Year,
      Transect,
      data_tally_group,
      tourist_access))) |>
  summarise(count_groupcode = sum(total), .groups = "keep") |> 
  ungroup(Substrate) |>
  mutate(total = sum(count_groupcode)) |>
  ungroup() 
dat |> as.data.frame() |> head()

##Recode data

All_Data2 <- 
  All_Data2 |>
  mutate(Transect = paste0(Site, Year, tourist_acces)) 
dat |> as.data.frame() |> head()

##Time Series Plot
All_Data2 |>
  filter(data_tally_group == "HC") |> 
  ggplot(aes(y =  100*count_groupcode/total, x = Year, colour = factor(tourist_access))) +
  geom_point() +
  geom_line(aes(group = Transect)) + 
  scale_y_continuous("Hard coral cover (%)") +
  scale_x_discrete("Year") + 
  theme_classic() +
  facet_wrap(~data_tally_group) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
