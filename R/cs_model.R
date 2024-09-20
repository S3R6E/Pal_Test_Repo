
### Case Study 1 Modelling: Spatial Variability of Coral Cover and Diversity of all PCSD survey sites

rm(list=ls())
gc()

library(tidyverse)
library(easystats)
library(knitr)
library(sf)
library(rnaturalearth)
library(brms)
library(rstan)
library(tidybayes)
library(patchwork)
library(DHARMa)
library(HDInterval)
library(emmeans)

source('~/Documents/GitHub/stats_workshop_public/ws/helperFunctions.R')

load("../data/processed/cs_cover.RData")
cs_cover <- cs_cover |>
  mutate(f.transect_name=as.factor(transect_name),
         f.site_reef_name=as.factor(site_reef_name),
         f.site_management=as.factor(site_management))

## Binomial Model
# HC ~ f.site_reef_name + (1|f.site_management) + (1|f.transect_name)

### Define Priors
cs_cover |>
  group_by(f.site_reef_name, f.site_management) |>
  summarise(
    qlogis(mean(COUNT)),
    qlogis(sd(COUNT)))

priors <- prior(normal(0, 1), class = "Intercept") +
  prior(normal(0, 0.5), class =  "b") + #b=slope
  prior(student_t(3, 0, 1), class = "sd") #sd=standard deviation

# Fit prior model with priors
form <- bf(GROUP.y | trials(TOTAL) ~ f.site_reef_name + (1|f.site_management) + (1|f.transect_name),
           family =  binomial(link =  "logit"))

model1 <- brm(form,
              data = cs_cover,
              prior = priors,
              iter =  6000,
              warmup =  1000,
              chains =  3,
              cores =  3,
              thin =  8,
              refresh = 0,
              control=list(adapt_delta=0.99),
              sample_prior = "yes"
)

model1 |>
  conditional_effects() |> 
  plot() |>
  _[[1]] +
  geom_point(data = df, aes(y = GROUP.y/total, x = f.site_reef_name), inherit.aes = FALSE)



