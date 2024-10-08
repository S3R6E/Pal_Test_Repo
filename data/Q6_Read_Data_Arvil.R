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

save(data_T1, file = "../data/primary/Q6_data_T1.Rdata")
save(data_T2, file = "../data/primary/Q6_data_T2.Rdata")
save(data_T3, file = "../data/primary/Q6_data_T3.Rdata")
save(data_PT1, file = "../data/primary/Q6_data_PT1.Rdata")
save(data_PT2, file = "../data/primary/Q6_data_PT2.Rdata")
save(data_PT3, file = "../data/primary/Q6_data_PT3.Rdata")
