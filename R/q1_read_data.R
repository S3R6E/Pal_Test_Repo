library(tidyverse)

## reading data from source
data_rc <- read.csv("../data/primary/data-coral-cover.csv")
labelset_rc <- read.csv("../data/primary/data-coral-cover-labelset.csv")

save(data_rc, file = "../data/primary/q1_data_rc.Rdata")
save(labelset_rc, file = "../data/primary/q1_labelset_rc.Rdata")