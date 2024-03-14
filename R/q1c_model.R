### HCC analysis for Palawan & Snake Island Project

# Data has been wrangled in q1c_process.R for Snake Island and Palawan.
# Now we want to estimate coral cover in both Project.
# NOTE: Data from different project are merged to increase the sample size.

##Load libraries
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

##Load the merge data
load("../data/processed/q1c_data.RData")

df <- df |>
  mutate(f.transect_name=as.factor(transect_name),
         f.Side=as.factor(Side),
         f.site_name=as.factor(site_name),
         f.site_reef_name=as.factor(site_reef_name),
         f.year=as.factor(as.character(year(date))))

## Binomial model ####
# HC ~ f.Side + f.site_reef_name + (1|f.transect_name) + (1|f.site_name) + (1|f.year)

### Define priors


#| label: define priors
df |>
  group_by(f.Side, f.site_reef_name) |>
  summarise(
    qlogis(mean(cover)),
    qlogis(sd(cover)))

priors <- prior(normal(0, 1), class = "Intercept") +
  prior(normal(0, 1), class =  "b") +
  prior(student_t(3, 0, 1), class = "sd") +
  prior(gamma(1, 0.1), class = "phi")


### Fit model with priors

form <- bf(count_groupcode | trials(total) ~ f.Side + (1|f.site_name) + (1|f.year),
           family =  beta_binomial(link =  "logit"))

model1 <- brm(form,
              data = df,
              prior = priors,
              sample_prior = "yes",
              iter =  6000,
              warmup =  1000,
              chains =  3,
              cores =  3,
              thin =  5,
              refresh = 0,
              control = list(adapt_delta=0.99)
)


model1 |>
  conditional_effects() |> 
  plot() |>
  _[[1]] +
  geom_point(data = df, aes(y = count_groupcode/total, x = f.Side), inherit.aes = FALSE)

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
gc()
testUniformity(resids)
plotResiduals(resids, form = factor(rep(1, nrow(df))))
plotResiduals(resids)
testDispersion(resids)


newdf <- expand.grid(f.Side = NA, 
                     f.site_name=levels(df$f.site_name),
                     f.year=NA,
                     total=1
)

site_draws <- posterior_epred(model1, newdata = newdf)

pred.df<-site_draws |> 
  as.data.frame() |>  
  pivot_longer(cols=everything(),names_to = "sites") |> 
  mutate(sites.num=extract_numeric(sites)) |> 
  group_by(sites, sites.num) |> 
  median_hdci() |> 
  
  mutate(sites=base::factor(sites,levels=paste0("V",seq(1:13)))) |> 
  left_join(newdf |> 
          mutate(sites.num=seq(1:13)))

save(pred.df, file="../outputs/q1c_site_predictions.RData")

model1 |>
  emmeans(~f.Side ) |>
  # regrid() |> 
  pairs() |> 
  gather_emmeans_draws() |>
  mutate(.value=exp(.value)) |> 
  summarise(median_hdci(.value),
            Pl = mean(.value < 1),
            Pg = mean(.value > 1)
  )


save(model1, file="../data/modelled/q1c_model.RData")







