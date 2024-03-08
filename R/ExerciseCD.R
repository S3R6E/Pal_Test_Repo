library(tidyverse) ##package tidyverse used for manipulating data

source("functions.R")

## Read in the data from the primary data folder
data1 <- read_csv("../data/primary/Siete Picados_T1.csv")


glimpse(data1)
##modern way of function
data1 |> glimpse()
data1