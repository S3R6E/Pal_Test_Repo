setwd("R") #don't run it again, if you are already in the right directory

##Transect1
## ---- load_file_T1
load(file = "../data/primary/q2_data1.RData")
## ---- end

## Process the data:
## - convert the taxon categories into counts per photo for each category
## - extracting important spatial data from the image name (split filename
##   into Site, Transect and Photo variable)
## - add a column that is the cover of coral calculated as the number of 
##   points of coral divided by the total number of points
## - rename the Raw Data column to make it consistent across multiple data sets

## ---- Data1
data1 <- data1 |>
  cpce_raw_classif_to_points() |>
  separate(`Frame image name`,
           into = c("Drive", "Folder", "Project", "Something", "Site", "Transect", "Photo"),
           sep = "\\\\"
  ) |>
  #filter(`Major Category` == "HC") |>
  ##droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = `Raw Data`)

save(data1, file = "../data/processed/q2_data1.RData")
## ---- end

##Process data showing the richness of Hard Coral category:
## - assigning the data richness of transect 1 to data1a
## - filtering the variables for Hard coral category
## - grouped the variables Photo, site and transects to exclude it from other variables
## - assigning "Richness" for the summary of data of hard coral cover points
## - ungroup function to set back all the data sets

## ---- data_rc1
data1a <- data1 |> 
  filter(`Major Category`=="HC") |> 
  droplevels() |>
  group_by(Photo, Site, Transect) |> 
  summarise(Richness = length(unique(data_tally_group))) |>
  ungroup()

save(data1a, file = "../data/processed/q2_data1a.RData")
## ---- end


##Transect2
## ---- load_file_T2
load(file = "../data/primary/q2_data2.RData")
## ---- end

## ---- Data2
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
## ---- end

##New function showing richness transect2
## ---- data_rc2
data2a <- data2 |> 
  filter(`Major Category`=="HC") |> 
  group_by(Photo, Site, Transect) |> 
  summarise(Richness = length(unique(data_tally_group)))

save(data2a, file = "../data/processed/q2_data2a.RData")
## ----end

##Transect3
## ---- load_file_T3
load(file = "../data/primary/q2_data3.RData")
## ---- end

## ---- Data3
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
## ---- end

##New function showing richness transect3
## ---- data_rc3
data3a <- data3 |> 
  filter(`Major Category`=="HC") |> 
  group_by(Photo, Site, Transect) |> 
  summarise(Richness = length(unique(data_tally_group)))

save(data3a, file = "../data/processed/q2_data3a.RData")
## ---- end

##Transect4
## ---- load_file_T4
load(file = "../data/primary/q2_data4.RData")
## ---- end

## ---- Data4
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
## ---- end

##New function showing richness  
## ---- data_rc4
data4a <- data4 |> 
  filter(`Major Category`=="HC") |> 
  group_by(Photo, Site, Transect) |> 
  summarise(Richness = length(unique(data_tally_group)))

save(data4a, file = "../data/processed/q2_data4a.RData")
## ---- end


##All 4 transects merged
## ---- merge_hcc
data <-bind_rows(data1,
                 data2,
                 data3,
                 data4)
save(data, file = "../data/processed/q2_data.RData")

load("../data/processed/q2_data.RData")
data <- data |> 
  group_by(Site, Transect, Photo, `Major Category`) |> 
  summarise(count_groupcode = sum(count_groupcode),
           total=max(total))
save(data, file = "../data/processed/q2_data.RData")
## ---- end

## ---- merge_hc
datarich <-bind_rows(data1a,
                     data2a,
                     data3a,
                     data4a)

save(datarich, file = "../data/processed/q2_datarich.RData")
## ---- end


