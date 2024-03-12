### Comparing Coral Cover between Machine and Human Estimation at image level

# Data has been wrangled in q1a_processes_data. Now we want to  estimate coral
# cover from machine and human annotations and  explore the likelyhood of those
# being different.
##Load libraries
rm(list=ls())

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


##Load the data
load("../data/processed/q1a_data_rc_cover.RData")
data_rc_cover<- data_rc_cover |>
  mutate(f.type=as.factor(type),
         f.transect_name=as.factor(transect_name),
         f.Side=as.factor(Side),
         f.image_id=as.factor(image_id))

## Binomial model
# HC ~ f.type + (1|f.transect_name)  + (1|f.Side) 

### Define priors


#| label: define priors
data_rc_cover |>
  group_by(f.type) |>
  summarise(
    qlogis(mean(cover)),
    qlogis(sd(cover)))

priors <- prior(normal(0, 1), class = "Intercept") +
  prior(normal(0, 1), class =  "b") + #b=slope
  prior(student_t(3, 0, 1), class = "sd") #sd=standard deviation


### Fit prior only model

#| label: fit prior only
#| cache: true

form <- bf(count_groupcode | trials(total) ~ f.type + (1|f.transect_name) + (1|f.Side),
           family =  binomial(link =  "logit"))

model1 <- brm(form,
              data = data_rc_cover,
              prior = priors,
              sample_prior = "only",
              iter =  5000,
              warmup =  1000,
              chains =  3,
              cores =  3,
              thin =  5,
              refresh = 0
)


model1 |>
  conditional_effects() |> 
  plot() |>
  _[[1]] +
  geom_point(data = data_rc_cover, aes(y = count_groupcode/total, x = f.type), inherit.aes = FALSE)

### Fit full model


# fit with prior
model1 <- update(model1, sample_prior = "yes") 

#partial 1

model1 |>
  conditional_effects() |> 
  plot() |>
  _[[1]] +
  geom_point(data = data_rc_cover, aes(y = count_groupcode/total, x = f.type), inherit.aes = FALSE)


#prior vs posterior

model1 |> SUYR_prior_and_posterior()



### MCMC sampling diagnostics

model1$fit |> stan_trace()
model1$fit |> stan_ac()
model1$fit |> stan_rhat()
model1$fit |> stan_ess()

### Posterior probability checks

#| label: pp checks

model1 |> pp_check(type = 'dens_overlay', ndraws =  100)


### Model validation

resids <- model1 |> make_brms_dharma_res(integerResponse = FALSE)
dev.off()
testUniformity(resids)
plotResiduals(resids, form = factor(rep(1, nrow(data_rc_cover))))
plotResiduals(resids)
testDispersion(resids)


model1 |>
  emmeans(~f.type) |>
  # regrid() |> 
  pairs() |> 
  gather_emmeans_draws() |>
  mutate(.value=exp(.value)) |> 
  summarise(median_hdci(.value),
            Pl = mean(.value < 0.95), #Understimation
            Pg = mean(.value > 1.05), # Over-estimation
            Px = mean(.value > 0.90 & .value< 1.10))



