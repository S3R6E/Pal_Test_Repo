library(tidyverse)


data <- read_csv("../data/primary/Siete Picados_T1.csv")

glimpse(data)
data

data <- data |>
  cpce_classif_to_points() |>
  separate(`Frame image name`,
    into = c("Drive", "Folder", "Project", "Something", "Site", "Transect", "Photo"),
    sep = "\\\\"
  ) |>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = `Major Category`)


data |> as.data.frame() |> head()

