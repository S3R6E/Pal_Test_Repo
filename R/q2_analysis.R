setwd("R")
library(tidyverse)
library(brms)
library(emmeans)
library(rstan)
library(posterior)
library(HDInterval)

load(file = "../data/processed/q2_datarich.RData")

##writing a formula for data analysis
form <- bf (Richness ~ 1 + (1|Transect))

##
datarich |> 
  ungroup() |> 
  summarise(Mean = mean(Richness),
                      Median = median(Richness),
                      SD = sd(Richness),
                      MAD = mad(Richness))

##define weekly informative priors
priors <- prior (normal(4, 1.5), class = "Intercept") +
  prior(student_t(3, 0, 1.5), class = "sd")

##fit a model
mod <- brm(form, 
           data = datarich,
           family = poisson(link = "log"),
           prior = priors, 
           iter = 3000, 
           chains = 3, cores = 3,
           thin = 5, warmup = 1000, 
           sample_prior = "only",
           backend = "cmdstanr",
           control=list(adapt_delta=0.99)
           )
save(mod, file = "../data/modelled/q2_mod_1.RData")

## Assess the priors

## Asses the predictions
mod |> emmeans(~1, type = "response")


## Rerun model including the data
mod_2 <- update(mod, sample_prior="yes")
save(mod_2, file = "../data/modelled/q2_mod_2.RData")

mod_2 |> emmeans(~1, type = "response")

## Evaluate MCMC mixing and convergence
stan_trace(mod_2$fit)  # traceplot
stan_ac(mod_2$fit)     # autocorrelation
stan_rhat(mod_2$fit) # all Rhat values < 1.01
stan_ess(mod_2$fit) # Effective Sample Size

## Regresion diagnostics
## Assumption checking
## - poisson distribution is appropriate
## - any relationships are linear

## Summarise model
mod_2 |> summary()

## P_exceed
## <0.85 no evidence
## >0.85 weak evidence
## > 0.9 evidence
## > 0.95 strong evidence

mod_2 |> ##fitted model
  as_draws_df() |> ##extracts the posteriors draw for each parameter
  mutate(b_Intercept = exp(b_Intercept)) |>  ##back transforming from log scale to the response scale
  summarise_draws(median, ##calculating the median of the posteriors
                  HDInterval::hdi,  ##Highest probability density intervals of the posteriors
                  Draws = ~ length(.x), ##total number of draws (chain samples)
                  rhat, ##convergence diagnostics
                  P_3 = ~ mean(.x > 3), ##test probability intercept greater than 3
                  P_4 = ~ mean(.x > 4), 
                  P_3_4 = ~ mean(.x > 3 & .x < 4),
                  P_l3 = ~ mean(.x < 4))

mod_2 |> as_draws_df() |> 
  mutate(b_Intercept = exp(b_Intercept)) |> 
  ggplot(aes(x = b_Intercept)) +
  geom_vline(xintercept=3) +
  geom_density()
