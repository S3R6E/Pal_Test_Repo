library(tidyverse)


source("functions.R")

#to read and access file
data_T1 <- read_csv("../data/primary/DiveSite_AbdeensRock_T1.csv")
data_T2 <- read_csv("../data/primary/DiveSite_AbdeensRock_T2.csv")
data_T3 <- read_csv("../data/primary/DiveSite_AbdeensRock_T3.csv")
data_PT1 <- read_csv("../data/primary/Protected_MitriRock_T1.csv")
data_PT2 <- read_csv("../data/primary/Protected_MitriRock_T2.csv")
data_PT3 <- read_csv("../data/primary/Protected_MitriRock_T3.csv")
data2_T1 <- read_csv("../data/primary/Tourist_Helicopter_T1.csv")
data2_T2 <- read_csv("../data/primary/Tourist_Helicopter_T2.csv")
data2_T3 <- read_csv("../data/primary/Tourist_Helicopter_T3.csv")
data2_PT1 <- read_csv("../data/primary/Pagawanen_T1.csv")
data2_PT2 <- read_csv("../data/primary/Pagawanen_T2.csv")
data2_PT3 <- read_csv("../data/primary/Pagawanen_T3.csv")
glimpse(data_T1)
glimpse(data_T2)
glimpse(data_T3)
glimpse(data_PT1)
glimpse(data_PT2)
glimpse(data_PT3)

glimpse(data2_T1)
glimpse(data2_T2)
glimpse(data2_T3)
glimpse(data2_PT1)
glimpse(data2_PT2)
glimpse(data2_PT3)

#to process the data

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
  rename("data_tally_group" = Major Category)|>
  dplyr::select(-Drive, -Folder, -Project, -Something)|>
  mutate('Tourist Access'= "No")

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
  rename("data_tally_group" = Major Category)|>
  dplyr::select(-Drive, -A)|>
  mutate('Tourist Access'= "Yes")

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
  rename("data_tally_group" = Major Category)|>
  dplyr::select(-Drive, -A)|>
  mutate('Tourist Access'= "Yes")

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
  rename("data_tally_group" = Major Category)|>
  dplyr::select(-Drive, -A, -B, -C, -D, -E, -F)|>
  mutate('Tourist Access'= "No")

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

#data2_PT3
data2_PT3 <- read_csv("../data/primary/Pagawanen_T3.csv")
glimpse(data2_PT3)

data2_PT3 <- data2_PT3 |>
  cpce_classif_to_points() |>
  separate(`Frame image name`,
           into = c("Drive", "A", "B", "C", "D", "E", "F", "Site", "Transect", "Photo"),
           sep = "\\\\"
  ) |>
  filter(`Major Category1 == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = `Major Category`)|>
  dplyr::select(-Drive, -A, -B, -C, -D, -E, -F)|>
  mutate('Tourist Access'= "No")

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

#exploratory data analysis

glimpse (all_data)

plot1 <-all_data |> 
  group_by(tourist_access) |> 
  summarise(Mean = mean(cover),
            SD = sd(cover)) |> 
  mutate(lower = Mean -SD,
         upper = Mean + SD) |> 
  ungroup() |> 
  ggplot(aes(y = Mean, x = tourist_access)) +
  geom_pointrange(aes(ymin=lower, ymax=upper)) +
  scale_y_continuous("Hard coral cover (%)", labels = function(x) x*100) +
  theme_classic(10)
plot1

plot2 <- all_data |>
  group_by(Site) |>
  summarise(Mean = mean(cover),
            SD = sd(cover)) |>
  mutate(lower = Mean - SD,
         upper = Mean + SD) |>
  ungroup() |>
  ggplot(aes(y = Mean, x = Site)) +
  geom_pointrange(aes(ymin=lower, ymax=upper)) +
  scale_y_continuous("Hard coral cover (%)", labels = function(x) x*100) +
  theme_classic(10)
plot2

plot3 <- all_data |> ggplot() +
  geom_boxplot(aes(x = Site, y = cover, fill = tourist_access)) +
  ggtitle("Hard Coral Cover") +
  theme(axis.text.x = element_text(angle=30, hjust = 1))


ggsave(plot3, file = "../outputs/figures/tourist_access_plot3.png",
       width = 20, height = 10, units = "cm",
       dpi=300)

ggsave(file = "../outputs/figures/tourist_access_plot1.png",
       width = 700, height = 500, units = "px",
       dpi=300)

ggsave(file = "../outputs/figures/tourist_access_plot1.pdf",
       width = 7, height = 5, units = "in")

*plot3 <- all_data |> ggplot() +
  geom_boxplot(aes(x = Site, y = cover, fill = tourist_access)) +
  ggtitle("Hard Coral Cover") +
  theme(axis.text.x = element_text(angle=30, hjust = 1))


ggsave(plot3, file = "../outputs/figures/tourist_access_plot3.png",
       width = 20, height = 10, units = "cm",
       dpi=300)

ggsave(file = "../outputs/figures/tourist_access_plot1.png",
       width = 700, height = 500, units = "px",
       dpi=300)

ggsave(file = "../outputs/figures/tourist_access_plot1.pdf",
       width = 7, height = 5, units = "in")