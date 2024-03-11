library(tidyverse)

source("functions.R")

#to read and access file
data_T1 <- read_csv("../data/primary/DiveSite_AbdeensRock_T1.csv")
data_T2 <- read_csv("../data/primary/DiveSite_AbdeensRock_T2.csv")
data_T3 <- read_csv("../data/primary/DiveSite_AbdeensRock_T3.csv")
data_PT1 <- read_csv("../data/primary/Protected_MitriRock_T1.csv")
data_PT2 <- read_csv("../data/primary/Protected_MitriRock_T2.csv")
data_PT3 <- read_csv("../data/primary/Protected_MitriRock_T3.csv")
glimpse(data_T1)
glimpse(data_T2)
glimpse(data_T3)
glimpse(data_PT1)
glimpse(data_PT2)
glimpse(data_PT3)

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
  rename("data_tally_group" = `Major Category`)|>
  dplyr::select(-Drive, -Folder, -Project, -Something)|>
  mutate('Tourist Access'= "No")

#to bind all the data
all_data <- bind_rows(data_T1,
                      data_T2,
                      data_T3,
                      data_PT1,
                      data_PT2,
                      data_PT3)

#exploratory data analysis
plot1 <-all_data|>
  group_by(`Tourist Access`)|>
  summarise(Mean = mean (cover),
            SD = sd(cover))|>
  mutate(lower = Mean -SD,
         upper = Mean + SD)|>
  ungroup() |>
  ggplot(aes(y = Mean, x = `Tourist Access`, colour= `Tourist Access`)) +
  geom_pointrange(aes(ymin=lower, ymax=upper)) +
  scale_y_continuous("Hard coral cover (%)", labels = function(x) x*100) + 
  theme_classic(10)
plot1
ggsave(file="..//outputs/figures/Tourist_Access_Plot1.png",
       width = 700, height = 500, units = "px",
       dpi = 300)
ggsave(file="..//outputs/figures/Tourist_Access_Plot1.pdf",
       width = 7, height = 5, units = "in")

  
