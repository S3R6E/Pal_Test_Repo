library(tidyverse) ##package tidyverse used for manipulating data

source("functions.R")

## Read in the data from the primary data folder
data1 <- read_csv("../data/primary/Siete Picados_T1.csv")

##traditional way of using a function
glimpse(data1)
##modern way of using a function
data1 |> glimpse()
data1