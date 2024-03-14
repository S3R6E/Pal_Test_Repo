setwd("R") #don't run it again, if you are already in the right directory

## ---- libraries
library(tidyverse)
source("../R/functions.R")
## ----end

## Read in the data from the primary data folder (Transect1)
## ---- read_data_T1
data1 <- read_csv("../data/primary/Siete Picados_T1.csv")
glimpse(data1)
head(data1)
save(data1, file = "../data/primary/q2_data1.RData")
## ----end


data1


##Data2 for Transect2
## ---- read_data_T2
data2 <- read_csv("../data/primary/Siete Picados_T2.csv")
glimpse(data2)
save(data2, file = "../data/primary/q2_data2.RData")
## ----end

##Data3 for Transect3
## ---- read_data_T3
data3 <- read_csv("../data/primary/Siete Picados_T3.csv")
glimpse(data3)
save(data3, file = "../data/primary/q2_data3.RData")
## ----end

##Data4 for Transect4
## ---- read_data_T4
data4 <- read_csv("../data/primary/Siete Picados_T4.csv")
glimpse(data4)
save(data4, file = "../data/primary/q2_data4.RData")
## ----end

