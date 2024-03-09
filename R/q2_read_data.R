setwd("R")

## Read in the data from the primary data folder (Transect1)
data1 <- read_csv("../data/primary/Siete Picados_T1.csv")

##traditional way of using a function
glimpse(data1)
##modern way of using a function
data1 |> glimpse()
data1

save(data1, file = "../data/primary/q2_data1.RData")