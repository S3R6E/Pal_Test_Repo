setwd("R")

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

##New function showing richness  
data1a <- data1 |> 
  filter(`Major Category`=="HC") |> 
  group_by(Photo, Site, Transect) |> 
  summarise(Richness = length(unique(data_tally_group)))


save(data1a, file = "../data/processed/q2_data1a.RData")