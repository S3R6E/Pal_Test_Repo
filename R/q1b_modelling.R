### Comparing Coral Cover between Machine and Human Estimation at per project and labelset

# Data has been wrangled in q1b_coralcover_si.R (Snake Island) and
# q1_data_rc_cover.R (Palawan Project). Now we want to estimate coral cover
# from machine and human annotations and explore the likelihood of those being
# different. NOTE: Data from different project are merged to increase the sample size.

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

##Load the data from Snake Island
load("../data/processed/q1b_data_si.RData")
data_si_cover=data_si_cover |> 
  mutate(project="snake") |> 
  select(-survey_start_date)

## Load the data from Palawan project
load("../data/processed/q1_data_rc_cover.RData")
data_rc_cover=data_rc_cover |> 
  mutate(project="palawan") |> 
  select(-survey_start_date)

## Merge datasets
df=data_si_cover |> 
  bind_rows(data_rc_cover)

df <- df |>
  mutate(f.type=as.factor(type),
         f.transect_name=as.factor(transect_name),
         f.Side=as.factor(Side),
         f.site_name=as.factor(site_name),
         f.project=as.factor(project))

## Binomial model ####
# HC ~ f.type + f.project + (1|f.transect_name) + (1|f.site_name) 

### Define priors


#| label: define priors
df |>
  group_by(f.type, f.project) |>
  summarise(
    qlogis(mean(cover)),
    qlogis(sd(cover)))

priors <- prior(normal(0, 1), class = "Intercept") +
  prior(normal(0, 1), class =  "b") +
  prior(student_t(3, 0, 1), class = "sd")


### Fit model with priors

form <- bf(count_groupcode | trials(total) ~ f.type*f.project + (1|f.transect_name) + (1|f.site_name),
           family =  binomial(link =  "logit"))

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
  geom_point(data = df, aes(y = count_groupcode/total, x = f.type), inherit.aes = FALSE)

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

## comparision between machine & human
res.proj <- model1 |>
  emmeans(~f.type|f.project) |>
  #regrid() |> 
  pairs() |> 
  gather_emmeans_draws() |>
  mutate(.value=exp(.value), .value=.value-1) |> 
  ungroup() |> 
  group_by(f.project) |> 
  summarise(median_hdci(.value),
            Pl = mean(.value < 0), #under-estimation
            Pg = mean(.value > 0), #over-estimation
            Px = mean(.value > -0.1 & .value < 0.1)
  ) |> 
  rename(Project=f.project, Difference=y, Low=ymin, High=ymax, 
         Cred.Int=.width, P.under=Pl, P.over=Pg, 
         P10=Px)

res.avg<- model1 |>
  emmeans(~f.type) |>
  #regrid() |> 
  pairs() |> 
  gather_emmeans_draws() |>
  mutate(.value=exp(.value), .value=.value-1) |> 
  ungroup() |> 
  summarise(median_hdci(.value),
            Pl = mean(.value < 0), #under-estimation
            Pg = mean(.value > 0), #over-estimation
            Px = mean(.value > -0.1 & .value < 0.1)
  ) |> 
  mutate(Project="Average") |> 
  rename(Difference=y, Low=ymin, High=ymax, 
         Cred.Int=.width, P.under=Pl, P.over=Pg, 
         P10=Px) |> 
  as.data.frame()

res<-res.proj |> bind_rows(res.avg)
save(res, file="../outputs/q1_machine_human.RData")





