setwd("R") #don't run it again, if you are already in the right directory

## ---- libraries
library(tidyverse)
## ----end

## Read in the data from the primary data folder (Transect1)
## ---- read_data
data1 <- read_csv("../data/primary/Siete Picados_T1.csv")
glimpse(data1)
head(data1)
## ----end


data1

save(data1, file = "../data/primary/q2_data1.RData")

##Data2 for Transect2
data2 <- read_csv("../data/primary/Siete Picados_T2.csv")

glimpse(data2)
data2

save(data2, file = "../data/primary/q2_data2.RData")

##Data3 for Transect3
data3 <- read_csv("../data/primary/Siete Picados_T3.csv")

glimpse(data3)
data3

save(data3, file = "../data/primary/q2_data3.RData")

##Data4 for Transect4
data4 <- read_csv("../data/primary/Siete Picados_T4.csv")

glimpse(data4)
data4

save(data4, file = "../data/primary/q2_data4.RData")


