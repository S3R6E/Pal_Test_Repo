setwd("R")
library(tidyverse)
library(brms)
library(emmeans)
library(rstan)
library(posterior)
library(HDInterval)
library(tidybayes)
library(DHARMa)
source("functions.R")


# Richness======================================================================

load(file = "../data/processed/q2_datarich.RData")

## A hierarchical Bayesian model was used to estimate coral richness across Siete Picados reef
## Specifically, we modelled taxonomic richness against a poisson distribution (with a log link) and employed
## weakly informative priors for all parameters (see supplement. 1).
## Each of three MCMC chains were run for a total of 3000 iterations with a warmup of 1000 and a thinning rate
## of five.
## Insert sentences on diagnostics and model validation
## All models and graphics were performed within the R (4.3.2) Graphical and Statistical environment (Ref) using the
## brms (Ref) package.  Model diagnostics utilised the DHARMa (Ref) package.

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

mod_2 |> SUYR_prior_and_posterior()

mod_2 |> emmeans(~1, type = "response")

## Evaluate MCMC mixing and convergence
stan_trace(mod_2$fit)  # traceplot
stan_ac(mod_2$fit)     # autocorrelation
stan_rhat(mod_2$fit) # all Rhat values < 1.01
stan_ess(mod_2$fit) # Effective Sample Size
## THe chains were all well mixed and converged upon a stable posterior (rhat < 1.01)

## Regresion diagnostics
## Assumption checking
## - poisson distribution is appropriate
## - any relationships are linear
mod_2 |> pp_check("dens_overlay", ndraws=300) |> plot()

## Model validation
resids <- make_brms_dharma_res(mod_2)
testUniformity(resids)
testDispersion(resids)
plotResiduals(resids)
## Simulated residuals did not reveal any goodness of fit issues

## Summarise model
#mod_2 |> summary()

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


# Coral cover===================================================================
form <- bf (count_groupcode | trials(total) ~ 1 + (1|Transect))

##
data |> 
  filter(`Major Category`=="HC") |> 
  mutate(cover = count_groupcode/total) |> 
   ungroup() |> 
   summarise(Mean = mean(cover),
             Median = median(cover),
             SD = sd(cover),
             MAD = mad(cover))

##define weekly informative priors
priors <- prior (normal(0.2, 1), class = "Intercept") +
  prior(student_t(3, 0, 1), class = "sd")

##fit a model
mod <- brm(form, 
           data = data |> filter(`Major Category` == "HC"),
           family = beta_binomial(link = "logit"),
           prior = priors, 
           iter = 3000, 
           chains = 3, cores = 3,
           thin = 5, warmup = 1000, 
           sample_prior = "only",
           backend = "cmdstanr",
           control=list(adapt_delta=0.99)
)
save(mod, file = "../data/modelled/q2_mod_1a.RData")

## Assess the priors


## Asses the predictions
mod |> emmeans(~1, type = "response")


## Rerun model including the data
mod_2 <- update(mod, sample_prior="yes")
save(mod_2, file = "../data/modelled/q2_mod_2.RData")

mod_2 |> SUYR_prior_and_posterior()

mod_2 |> emmeans(~1, type = "response")

## Evaluate MCMC mixing and convergence
stan_trace(mod_2$fit)  # traceplot
stan_ac(mod_2$fit)     # autocorrelation
stan_rhat(mod_2$fit) # all Rhat values < 1.01
stan_ess(mod_2$fit) # Effective Sample Size
## THe chains were all well mixed and converged upon a stable posterior (rhat < 1.01)

## Regresion diagnostics
## Assumption checking
## - poisson distribution is appropriate
## - any relationships are linear
mod_2 |> pp_check("dens_overlay", ndraws=300) |> plot()

## Model validation
resids <- make_brms_dharma_res(mod_2)
testUniformity(resids)
testDispersion(resids)
plotResiduals(resids)
## Simulated residuals did not reveal any goodness of fit issues

## Summarise model
mod_2 |> summary()

## P_exceed
## <0.85 no evidence
## >0.85 weak evidence
## > 0.9 evidence
## > 0.95 strong evidence


#Poor (D): 0-22
#C: 22-33
#B: 33 - 44
#A: > 44
mod_2 |> ##fitted model
  as_draws_df() |> ##extracts the posteriors draw for each parameter
  mutate(b_Intercept = plogis(b_Intercept)) |>  ##back transforming from log scale to the response scale
  summarise_draws(median, ##calculating the median of the posteriors
                  HDInterval::hdi,  ##Highest probability density intervals of the posteriors
                  Draws = ~ length(.x), ##total number of draws (chain samples)
                  rhat, ##convergence diagnostics
                  P_D = ~ mean(.x > 0 & .x <=0.22), ##test probability intercept greater than 3
                  P_C = ~ mean(.x > 0.22 & .x <= 0.33), 
                  P_B = ~ mean(.x > 0.33 & .x <= 0.44),
                  P_A = ~ mean(.x > 0.44))

mod_2 |> as_draws_df() |> 
  mutate(b_Intercept = plogis(b_Intercept)) |> 
  ggplot(aes(x = b_Intercept)) +
  geom_vline(xintercept=0.22) +
  geom_vline(xintercept=0.33) +
  geom_vline(xintercept=0.44) +
  scale_x_continuous(limits = c(0,1)) +
  geom_density() +
  theme_classic()

# Binomial Coral Cover===================================================================
form <- bf (count_groupcode | trials(total) ~ 1 + (1|Transect))

##
data |> 
  filter(`Major Category`=="HC") |> 
  mutate(cover = count_groupcode/total) |> 
  ungroup() |> 
  summarise(Mean = mean(cover),
            Median = median(cover),
            SD = sd(cover),
            MAD = mad(cover))

##define weekly informative priors
priors <- prior (normal(0.2, 1), class = "Intercept") +
  prior(student_t(3, 0, 1), class = "sd")

##fit a model
mod <- brm(form, 
           data = data |> filter(`Major Category` == "HC"),
           family = binomial(link = "logit"),
           prior = priors, 
           iter = 3000, 
           chains = 3, cores = 3,
           thin = 5, warmup = 1000, 
           sample_prior = "only",
           backend = "cmdstanr",
           control=list(adapt_delta=0.99)
)
save(mod, file = "../data/modelled/q2_mod_1b.RData")

## Assess the priors


## Asses the predictions
mod |> emmeans(~1, type = "response")


## Rerun model including the data
mod_2 <- update(mod, sample_prior="yes")
save(mod_2, file = "../data/modelled/q2_mod_2b.RData")

mod_2 |> SUYR_prior_and_posterior()

mod_2 |> emmeans(~1, type = "response")

## Evaluate MCMC mixing and convergence
stan_trace(mod_2$fit)  # traceplot
stan_ac(mod_2$fit)     # autocorrelation
stan_rhat(mod_2$fit) # all Rhat values < 1.01
stan_ess(mod_2$fit) # Effective Sample Size
## THe chains were all well mixed and converged upon a stable posterior (rhat < 1.01)

## Regresion diagnostics
## Assumption checking
## - poisson distribution is appropriate
## - any relationships are linear
mod_2 |> pp_check("dens_overlay", ndraws=300) |> plot()

## Model validation
resids <- make_brms_dharma_res(mod_2)
testUniformity(resids)
testDispersion(resids)
plotResiduals(resids)
## Simulated residuals did not reveal any goodness of fit issues

## Summarise model
mod_2 |> summary()

## P_exceed
## <0.85 no evidence
## >0.85 weak evidence
## > 0.9 evidence
## > 0.95 strong evidence


#Poor (D): 0-22
#C: 22-33
#B: 33 - 44
#A: > 44
mod_2 |> ##fitted model
  as_draws_df() |> ##extracts the posteriors draw for each parameter
  mutate(b_Intercept = plogis(b_Intercept)) |>  ##back transforming from log scale to the response scale
  summarise_draws(median, ##calculating the median of the posteriors
                  HDInterval::hdi,  ##Highest probability density intervals of the posteriors
                  Draws = ~ length(.x), ##total number of draws (chain samples)
                  rhat, ##convergence diagnostics
                  P_D = ~ mean(.x > 0 & .x <=0.22), ##test probability intercept greater than 3
                  P_C = ~ mean(.x > 0.22 & .x <= 0.33), 
                  P_B = ~ mean(.x > 0.33 & .x <= 0.44),
                  P_A = ~ mean(.x > 0.44))

mod_2 |> as_draws_df() |> 
  mutate(b_Intercept = plogis(b_Intercept)) |> 
  ggplot(aes(x = b_Intercept)) +
  geom_vline(xintercept=0.22) +
  geom_vline(xintercept=0.33) +
  geom_vline(xintercept=0.44) +
  scale_x_continuous(limits = c(0,1)) +
  geom_density() +
  theme_classic()

## compare binomial and beta-binomial models
load(file = "../data/modelled/q2_mod_2b.RData")
mod_bin <- mod_2
load(file = "../data/modelled/q2_mod_2.RData")
mod_bb <- mod_2

loo(mod_bin)
loo(mod_bb)
loo::compare(
  loo(mod_bin),
  loo(mod_bb)
  )

citation()
citation(package="DHARMa")
sessionInfo()
