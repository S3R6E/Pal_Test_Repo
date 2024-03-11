setwd("R")
library(tidyverse)
library(brms)


load(file = "../data/processed/q2_datarich.RData")

##writing a formula for data analysis
form <- bf (Richness ~ 1)

##
datarich |> 
  ungroup() |> 
  summarise(Mean = mean(Richness),
                      Median = median(Richness),
                      SD = sd(Richness),
                      MAD = mad(Richness))

##define weekly informative priors
priors <- prior (normal(4, 1.5), class = "Intercept")

##fit a model
mod <- brm(form, 
           data = datarich,
           family = poisson(link = "log"),
           prior = priors, 
           iter = 3000, 
           chains = 3, cores = 3,
           thin = 5, warmup = 1000, 
           sample_prior = "only"#,
           #backend = "cmdstanr"
           )
