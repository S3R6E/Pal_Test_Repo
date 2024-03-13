setwd("R")
library(tidyverse)
library(brms)
library(emmeans)
library(rstan)
library(posterior)
library(HDInterval)
library(tidybayes)
library(DHARMa)
library(patchwork)
library(ggridges)
source("functions.R")


##Analysis for Coral Cover under Reef Type (question1)

load(file = "../data/processed/q1_data_rc_cover.RData")
data_rc_cover <- data_rc_cover |> 
  filter(type == "point_machine_classification")
#formula
form <- bf(count_groupcode | trials(total) ~ 1 + site_reef_type)

##define priors
data_rc_cover |> 
  mutate(cover = count_groupcode/total) |> 
  ungroup() |> 
  group_by(site_reef_type) |> 
  summarise(Mean = mean(cover),
            Median = median(cover),
            SD = sd(cover),
            MAD = mad(cover))
qlogis(0.254)
priors <- prior (normal(-1.08, 1), class = "Intercept") +
  prior(normal(0, 1), class = "b")


mod <- brm(form, 
           data = data_rc_cover,
           family = binomial(link = "logit"),
           prior = priors, 
           iter = 3000, 
           chains = 3, cores = 3,
           thin = 5, warmup = 1000, 
           sample_prior = "only",
           backend = "cmdstanr",
           control=list(adapt_delta=0.99)
)
save(mod, file = "../data/modelled/q1_mod_1.RData")

## Asses the priors predictions
mod |> emmeans (~ 1 + site_reef_type, type = "response")


## Rerun model including the data
mod_2 <- update(mod, sample_prior="yes")

save(mod_2, file = "../data/modelled/q1_mod_2.RData")

##gives plot for priors
mod_2 |> SUYR_prior_and_posterior()

##estimating priors with data
mod_2 |> emmeans(~site_reef_type, type = "response")

## Evaluate MCMC mixing and convergence
stan_trace(mod_2$fit)  # traceplot
stan_ac(mod_2$fit)     # autocorrelation
stan_rhat(mod_2$fit) # all Rhat values < 1.01
stan_ess(mod_2$fit) # Effective Sample Size
## THe chains were all well mixed and converged upon a stable posterior (rhat < 1.01)

## Regresion diagnostics
## Assumption checking
## - binomial distribution is appropriate
mod_2 |> pp_check("dens_overlay", ndraws=300) |> plot()

## Model validation
resids <- make_brms_dharma_res(mod_2)
testUniformity(resids)
testDispersion(resids)
plotResiduals(resids)
##simulated residuals indicated a lack of model fit

##fit a new model: beta-binomial
##define the priors
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

##fit the model
mod_3 <- brm(form, 
           data = data_rc_cover,
           family = beta_binomial(link = "logit"),
           prior = priors, 
           iter = 3000, 
           chains = 3, cores = 3,
           thin = 5, warmup = 1000, 
           sample_prior = "only",
           backend = "cmdstanr",
           control=list(adapt_delta=0.99)
)
save(mod_3, file = "../data/modelled/q1_mod_3.RData")

## Assess the priors


## Asses the predictions
mod_3 |> emmeans(~1, type = "response")


## Rerun model including the data
mod_4 <- update(mod_3, sample_prior="yes")
save(mod_4, file = "../data/modelled/q1_mod_3.RData")

mod_4 |> SUYR_prior_and_posterior()

mod_4 |> emmeans(~1, type = "response")

## Evaluate MCMC mixing and convergence
stan_trace(mod_4$fit)  # traceplot
stan_ac(mod_4$fit)     # autocorrelation
stan_rhat(mod_4$fit) # all Rhat values < 1.01
stan_ess(mod_4$fit) # Effective Sample Size
## THe chains were all well mixed and converged upon a stable posterior (rhat < 1.01)

## Regresion diagnostics
## Assumption checking
## - poisson distribution is appropriate
## - any relationships are linear
mod_4 |> pp_check("dens_overlay", ndraws=300) |> plot()

## Model validation
resids <- make_brms_dharma_res(mod_4)
testUniformity(resids)
testDispersion(resids)
plotResiduals(resids)
## Simulated residuals did not reveal any goodness of fit issues

## Summarise model
mod_4 |> summary()


mod_4 |> emmeans(~site_reef_type, type="response") ##means of each group
mod_4 |> emmeans(~site_reef_type, type="response") |> ##comparison between the groups, patch is 60% higher than fringing
  pairs(reverse=TRUE)
mod_4 |> emmeans(~site_reef_type) |> ##plot the distribution of the ratio (group 2/group 1)
  pairs(reverse=TRUE) |> 
  gather_emmeans_draws() |> 
  mutate(.value=exp(.value)) |> 
  as.data.frame() |> 
  ggplot(aes(x=.value)) +
  geom_vline(xintercept=1) +
  geom_density()

mod_4 |> emmeans(~site_reef_type) |> 
  pairs(reverse=TRUE) |> 
  gather_emmeans_draws() |> 
  mutate(.value=exp(.value)) |> 
  dplyr::select(-.chain) |> 
  summarise_draws(median,
                  HDInterval::hdi,  ##Highest probability density intervals of the posteriors
                  Draws = ~ length(.x), ##total number of draws (chain samples)
                  rhat, 
                  P = ~ mean(.x > 1))

##checking the probability of the group 2 > group 1
mod_4 |> emmeans(~site_reef_type, type="response") |> 
  as.data.frame() |> 
  ggplot(aes(y = prob, x=site_reef_type)) +
  geom_pointrange(aes(ymin = lower.HPD, ymax=upper.HPD))
##result shows that there is no evidence (p=0.748) < p=0.85, more over the value of 1 is included in the interval (lower=0.159, upper=5.61)



##Analysis for Coral Cover over time (years) - question2

load(file = "../data/processed/q1_data_rc_cover.RData")
data_rc_cover <- data_rc_cover |> 
  mutate(Year = factor(lubridate::year(survey_start_date))) |> 
  filter(type == "point_machine_classification")


#formula
form <- bf(count_groupcode | trials(total) ~ 1 + Year)

##define priors
data_rc_cover |> 
  mutate(cover = count_groupcode/total) |> 
  ungroup() |> 
  group_by(Year) |>
  summarise(Mean = mean(cover),
            Median = median(cover),
            SD = sd(cover),
            MAD = mad(cover),
            n=n())


qlogis(0.359)
priors <- prior (normal(-0.6, 1), class = "Intercept") +
  prior(normal(0, 1), class = "b")

#fit the model
mod_rcy <- brm(form, 
             data = data_rc_cover,
             family = binomial(link = "logit"),
             prior = priors, 
             iter = 3000, 
             chains = 3, cores = 3,
             thin = 5, warmup = 1000, 
             sample_prior = "only",
             backend = "cmdstanr",
             control=list(adapt_delta=0.99)
)
save(mod_rcy, file = "../data/modelled/q1_mod_rcy.RData")


## Asses the predictions
mod_rcy |> emmeans(~1 + Year, type = "response")


## Rerun model including the data
mod_rcy1 <- update(mod_rcy, sample_prior="yes")
save(mod_rcy1, file = "../data/modelled/q1_mod_rcy.RData")

mod_rcy1 |> SUYR_prior_and_posterior()
##the plot suggests that the posteriors are the result of the data rather than the priors 
##and yet, the priors are not so vague as to allow the MCMC sampler to drift into unsupported areas of the posterior

##including the data with priors
mod_rcy1 |> emmeans(~1 + Year, type = "response")
##the impact of the data showed much narrower intervals

## Evaluate MCMC mixing and convergence
stan_trace(mod_rcy1$fit)  # traceplot
stan_ac(mod_rcy1$fit)     # autocorrelation
stan_rhat(mod_rcy1$fit) # all Rhat values < 1.01
stan_ess(mod_rcy1$fit) # Effective Sample Size
## THe chains were all well mixed and converged upon a stable posterior (rhat < 1.01)

## Regresion diagnostics
## Assumption checking
## - binomial distribution is appropriate
mod_rcy1 |> pp_check("dens_overlay", ndraws=300) |> plot()

## Model validation
resids <- make_brms_dharma_res(mod_rcy1)
testUniformity(resids)
testDispersion(resids)
plotResiduals(resids)
plotResiduals(resids, quantreg = FALSE)
## Simulated residuals of the QQ Plot suggested that the binomial distribution was insufficient, and there was evidence of over dispersion



##fit a new model: beta-binomial
##define the priors
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

##fit the model
mod_3 <- brm(form, 
             data = data_rc_cover,
             family = beta_binomial(link = "logit"),
             prior = priors, 
             iter = 3000, 
             chains = 3, cores = 3,
             thin = 5, warmup = 1000, 
             sample_prior = "only",
             backend = "cmdstanr",
             control=list(adapt_delta=0.99)
)
save(mod_3, file = "../data/modelled/q1_mod_3.RData")

## Assess the priors


## Asses the predictions
mod_3 |> emmeans(~1, type = "response")


## Rerun model including the data
mod_4 <- update(mod_3, sample_prior="yes")
save(mod_4, file = "../data/modelled/q1_mod_3.RData")

mod_4 |> SUYR_prior_and_posterior()

mod_4 |> emmeans(~1, type = "response")

## Evaluate MCMC mixing and convergence
stan_trace(mod_4$fit)  # traceplot
stan_ac(mod_4$fit)     # autocorrelation
stan_rhat(mod_4$fit) # all Rhat values < 1.01
stan_ess(mod_4$fit) # Effective Sample Size
## THe chains were all well mixed and converged upon a stable posterior (rhat < 1.01)

## Regresion diagnostics
## Assumption checking
## - poisson distribution is appropriate
## - any relationships are linear
mod_4 |> pp_check("dens_overlay", ndraws=300) |> plot()

## Model validation
resids <- make_brms_dharma_res(mod_4)
testUniformity(resids)
testDispersion(resids)
plotResiduals(resids)
## Simulated residuals did not reveal any goodness of fit issues

## Summarise model
mod_4 |> summary()


mod_4 |> emmeans(~site_reef_type, type="response") ##means of each group
mod_4 |> emmeans(~site_reef_type, type="response") |> ##comparison between the groups, patch is 60% higher than fringing
  pairs(reverse=TRUE)
mod_4 |> emmeans(~site_reef_type) |> ##plot the distribution of the ratio (group 2/group 1)
  pairs(reverse=TRUE) |> 
  gather_emmeans_draws() |> 
  mutate(.value=exp(.value)) |> 
  as.data.frame() |> 
  ggplot(aes(x=.value)) +
  geom_vline(xintercept=1) +
  geom_density()

mod_4 |> emmeans(~site_reef_type) |> 
  pairs(reverse=TRUE) |> 
  gather_emmeans_draws() |> 
  mutate(.value=exp(.value)) |> 
  dplyr::select(-.chain) |> 
  summarise_draws(median,
                  HDInterval::hdi,  ##Highest probability density intervals of the posteriors
                  Draws = ~ length(.x), ##total number of draws (chain samples)
                  rhat, 
                  P = ~ mean(.x > 1))

##checking the probability of the group 2 > group 1
mod_4 |> emmeans(~site_reef_type, type="response") |> 
  as.data.frame() |> 
  ggplot(aes(y = prob, x=site_reef_type)) +
  geom_pointrange(aes(ymin = lower.HPD, ymax=upper.HPD))
##result shows that there is no evidence (p=0.748) < p=0.85, more over the value of 1 is included in the interval (lower=0.159, upper=5.61)



##Analysis for Coral Cover over time (years) - question2

load(file = "../data/processed/q1_data_rc_cover.RData")
data_rc_cover <- data_rc_cover |> 
  mutate(Year = factor(lubridate::year(survey_start_date))) |> 
  filter(type == "point_machine_classification")


#formula
form <- bf(count_groupcode | trials(total) ~ 1 + Year)

##define priors
data_rc_cover |> 
  mutate(cover = count_groupcode/total) |> 
  ungroup() |> 
  group_by(Year) |>
  summarise(Mean = mean(cover),
            Median = median(cover),
            SD = sd(cover),
            MAD = mad(cover),
            n=n())


qlogis(0.359)
priors <- prior (normal(-0.6, 1), class = "Intercept") +
  prior(normal(0, 1), class = "b")

#fit the model
mod_rcy <- brm(form, 
               data = data_rc_cover,
               family = binomial(link = "logit"),
               prior = priors, 
               iter = 3000, 
               chains = 3, cores = 3,
               thin = 5, warmup = 1000, 
               sample_prior = "only",
               backend = "cmdstanr",
               control=list(adapt_delta=0.99)
)
save(mod_rcy, file = "../data/modelled/q1_mod_rcy.RData")


## Asses the predictions
mod_rcy |> emmeans(~1 + Year, type = "response")


## Rerun model including the data
mod_rcy1 <- update(mod_rcy, sample_prior="yes")
save(mod_rcy1, file = "../data/modelled/q1_mod_rcy.RData")

mod_rcy1 |> SUYR_prior_and_posterior()
##the plot suggests that the posteriors are the result of the data rather than the priors 
##and yet, the priors are not so vague as to allow the MCMC sampler to drift into unsupported areas of the posterior

##including the data with priors
mod_rcy1 |> emmeans(~1 + Year, type = "response")
##the impact of the data showed much narrower intervals

## Evaluate MCMC mixing and convergence
stan_trace(mod_rcy1$fit)  # traceplot
stan_ac(mod_rcy1$fit)     # autocorrelation
stan_rhat(mod_rcy1$fit) # all Rhat values < 1.01
stan_ess(mod_rcy1$fit) # Effective Sample Size
## THe chains were all well mixed and converged upon a stable posterior (rhat < 1.01)

## Regresion diagnostics
## Assumption checking
## - binomial distribution is appropriate
mod_rcy1 |> pp_check("dens_overlay", ndraws=300) |> plot()

## Model validation
resids <- make_brms_dharma_res(mod_rcy1)
testUniformity(resids)
testDispersion(resids)
plotResiduals(resids)
plotResiduals(resids, quantreg = FALSE)
## Simulated residuals of the QQ Plot suggested that the binomial distribution was insufficient, and there was evidence of over dispersion


##fit a new model: beta-binomial
##define the priors
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

##fit the model
mod_3 <- brm(form, 
           data = data_rc_cover,
           family = beta_binomial(link = "logit"),
           prior = priors, 
           iter = 3000, 
           chains = 3, cores = 3,
           thin = 5, warmup = 1000, 
           sample_prior = "only",
           backend = "cmdstanr",
           control=list(adapt_delta=0.99)
)
save(mod_3, file = "../data/modelled/q1_mod_3.RData")

## Assess the priors


## Asses the predictions
mod_3 |> emmeans(~1, type = "response")


## Rerun model including the data
mod_4 <- update(mod_3, sample_prior="yes")
save(mod_4, file = "../data/modelled/q1_mod_3.RData")

mod_4 |> SUYR_prior_and_posterior()

mod_4 |> emmeans(~1, type = "response")

## Evaluate MCMC mixing and convergence
stan_trace(mod_4$fit)  # traceplot
stan_ac(mod_4$fit)     # autocorrelation
stan_rhat(mod_4$fit) # all Rhat values < 1.01
stan_ess(mod_4$fit) # Effective Sample Size
## THe chains were all well mixed and converged upon a stable posterior (rhat < 1.01)

## Regresion diagnostics
## Assumption checking
## - poisson distribution is appropriate
## - any relationships are linear
mod_4 |> pp_check("dens_overlay", ndraws=300) |> plot()

## Model validation
resids <- make_brms_dharma_res(mod_4)
testUniformity(resids)
testDispersion(resids)
plotResiduals(resids)
## Simulated residuals did not reveal any goodness of fit issues

## Summarise model
mod_4 |> summary()


mod_4 |> emmeans(~site_reef_type, type="response") ##means of each group
mod_4 |> emmeans(~site_reef_type, type="response") |> ##comparison between the groups, patch is 60% higher than fringing
  pairs(reverse=TRUE)
mod_4 |> emmeans(~site_reef_type) |> ##plot the distribution of the ratio (group 2/group 1)
  pairs(reverse=TRUE) |> 
  gather_emmeans_draws() |> 
  mutate(.value=exp(.value)) |> 
  as.data.frame() |> 
  ggplot(aes(x=.value)) +
  geom_vline(xintercept=1) +
  geom_density()

mod_4 |> emmeans(~site_reef_type) |> 
  pairs(reverse=TRUE) |> 
  gather_emmeans_draws() |> 
  mutate(.value=exp(.value)) |> 
  dplyr::select(-.chain) |> 
  summarise_draws(median,
                  HDInterval::hdi,  ##Highest probability density intervals of the posteriors
                  Draws = ~ length(.x), ##total number of draws (chain samples)
                  rhat, 
                  P = ~ mean(.x > 1))

##checking the probability of the group 2 > group 1
mod_4 |> emmeans(~site_reef_type, type="response") |> 
  as.data.frame() |> 
  ggplot(aes(y = prob, x=site_reef_type)) +
  geom_pointrange(aes(ymin = lower.HPD, ymax=upper.HPD))
##result shows that there is no evidence (p=0.748) < p=0.85, more over the value of 1 is included in the interval (lower=0.159, upper=5.61)



##Analysis for Coral Cover over time (years) - question2

load(file = "../data/processed/q1_data_rc_cover.RData")
data_rc_cover <- data_rc_cover |> 
  mutate(Year = factor(lubridate::year(survey_start_date))) |> 
  filter(type == "point_machine_classification")


#formula
form <- bf(count_groupcode | trials(total) ~ 1 + Year)

##define priors
data_rc_cover |> 
  mutate(cover = count_groupcode/total) |> 
  ungroup() |> 
  group_by(Year) |>
  summarise(Mean = mean(cover),
            Median = median(cover),
            SD = sd(cover),
            MAD = mad(cover),
            n=n())


qlogis(0.359)
priors <- prior (normal(-0.6, 1), class = "Intercept") +
  prior(normal(0, 1), class = "b")

#fit the model
mod_rcy <- brm(form, 
             data = data_rc_cover,
             family = binomial(link = "logit"),
             prior = priors, 
             iter = 3000, 
             chains = 3, cores = 3,
             thin = 5, warmup = 1000, 
             sample_prior = "only",
             backend = "cmdstanr",
             control=list(adapt_delta=0.99)
)
save(mod_rcy, file = "../data/modelled/q1_mod_rcy.RData")


## Asses the predictions
mod_rcy |> emmeans(~1 + Year, type = "response")


## Rerun model including the data
mod_rcy1 <- update(mod_rcy, sample_prior="yes")
save(mod_rcy1, file = "../data/modelled/q1_mod_rcy.RData")

mod_rcy1 |> SUYR_prior_and_posterior()
##the plot suggests that the posteriors are the result of the data rather than the priors 
##and yet, the priors are not so vague as to allow the MCMC sampler to drift into unsupported areas of the posterior

##including the data with priors
mod_rcy1 |> emmeans(~1 + Year, type = "response")
##the impact of the data showed much narrower intervals

## Evaluate MCMC mixing and convergence
stan_trace(mod_rcy1$fit)  # traceplot
stan_ac(mod_rcy1$fit)     # autocorrelation
stan_rhat(mod_rcy1$fit) # all Rhat values < 1.01
stan_ess(mod_rcy1$fit) # Effective Sample Size
## THe chains were all well mixed and converged upon a stable posterior (rhat < 1.01)

## Regresion diagnostics
## Assumption checking
## - binomial distribution is appropriate
mod_rcy1 |> pp_check("dens_overlay", ndraws=300) |> plot()

## Model validation
resids <- make_brms_dharma_res(mod_rcy1)
testUniformity(resids)
testDispersion(resids)
plotResiduals(resids)
plotResiduals(resids, quantreg = FALSE)
## Simulated residuals of the QQ Plot suggested that the binomial distribution was insufficient, and there was evidence of over dispersion


##fit a new model: beta-binomial
##define the priors
data_rc_cover |> 
  mutate(cover = count_groupcode/total) |> 
  ungroup() |> 
  group_by(Year) |>
  summarise(Mean = mean(cover),
            Median = median(cover),
            SD = sd(cover),
            MAD = mad(cover),
            n=n())

qlogis(0.359)
priors <- prior (normal(-0.6, 1), class = "Intercept") +
  prior(normal(0, 1), class = "b")

#fit the model
mod_rcy2 <- brm(form, 
               data = data_rc_cover,
               family = beta_binomial(link = "logit"),
               prior = priors, 
               iter = 3000, 
               chains = 3, cores = 3,
               thin = 5, warmup = 1000, 
               sample_prior = "only",
               backend = "cmdstanr",
               control=list(adapt_delta=0.99)
)
save(mod_rcy2, file = "../data/modelled/q1_mod_rcy2.RData")

## Assess the priors


## Asses the predictions
mod_rcy2 |> emmeans(~1 + Year, type = "response")


## Rerun model including the data
mod_rcy3 <- update(mod_rcy2, sample_prior="yes")
save(mod_rcy3, file = "../data/modelled/q1_mod_rcy2.RData")

mod_rcy3 |> SUYR_prior_and_posterior()

##added data
mod_rcy3 |> emmeans(~1 + Year, type = "response")

## Evaluate MCMC mixing and convergence
stan_trace(mod_rcy3$fit)  # traceplot
stan_ac(mod_rcy3$fit)     # autocorrelation
stan_rhat(mod_rcy3$fit) # all Rhat values < 1.01
stan_ess(mod_rcy3$fit) # Effective Sample Size
## THe chains were all well mixed and converged upon a stable posterior (rhat < 1.01)

## Regresion diagnostics
## Assumption checking
## - beta-binomial distribution is appropriate

mod_rcy3 |> pp_check("dens_overlay", ndraws=300) |> plot()

## Model validation
resids <- make_brms_dharma_res(mod_rcy3)
testUniformity(resids)
testDispersion(resids)
plotResiduals(resids,quantreg = FALSE)
## Simulated residuals did not reveal any goodness of fit issues

##looking at the estimate posteriors
mod_rcy3 |> emmeans(~Year, type="response") ##predicted medians on the response scale of eavh group (year)
mod_rcy3 |> emmeans(~Year, type="response") |> ##comparison between each pair of year
  pairs(reverse=TRUE)
mod_rcy3 |> emmeans(~Year, type="response") |> ##comparison between each pair of year
  pairs(reverse=FALSE)
mod_rcy3 |> emmeans(~Year) |> ##plot the distribution of the ratio 
  pairs(reverse=TRUE) |> 
  gather_emmeans_draws() |> 
  mutate(.value=exp(.value)) |> 
  as.data.frame() |> 
  ggplot(aes(x=.value)) +
  geom_vline(xintercept=1) +
  geom_density() +
  facet_wrap(~contrast, scales = "free")

mod_rcy3 |> emmeans(~Year) |> 
  pairs(reverse=TRUE) |> 
  gather_emmeans_draws() |> 
  mutate(.value=exp(.value)) |> 
  dplyr::select(-.chain) |> 
  summarise_draws(median,
                  HDInterval::hdi,  ##Highest probability density intervals of the posteriors
                  Draws = ~ length(.x), ##total number of draws (chain samples)
                  rhat, 
                  P = ~ mean(.x > 1),
                  Pl = ~ mean(.x < 1))

##compare the yr 2019, 2021 against year 2022 and 2023 (Comparison 1)
##compare the yr 2019, against year 2021, 2022 and 2023 (Comparison 2)


##contrast coefficients
##      Comp1  Comp2  Comp3
##2019  1/2    1       0
##2021  1/2    -1/3    1
##2022  -1/2   -1/3   -1/2
##2023  -1/2   -1/3   -1/2

cmat <- cbind(Comp1=c(1/2, 1/2, -1/2, -1/2),
              Comp2=c(1, -1/3, -1/3, -1/3),
              Comp3=c(0, 1, -1/2, -1/2))

mod_rcy3 |> emmeans(~Year, type="response") |> 
  contrast(method=list(Year=cmat)) |> 
  gather_emmeans_draws() |> 
  mutate(.value=exp(.value)) |> 
  dplyr::select(-.chain) |> 
  summarise_draws(median,
                  HDInterval::hdi,  ##Highest probability density intervals of the posteriors
                  Draws = ~ length(.x), ##total number of draws (chain samples)
                  rhat, 
                  P = ~ mean(.x > 1),
                  Pl = ~ mean(.x < 1))
  
##In comparison 1, there is a strong evidence (p=0.98) that coral cover was higher in years 2019 and 2021 against years 2022 and 2023
##In Comparison 2, there is no evidence that coral cover has decline in years 2021, 2022 and 2023 against 2019
##In Comparison 3, there is a strong evidence (p=0.98) that coral cover was higher in 2021 than years 2022 and 2023.
##  On average, the coral cover has an average decline of 82.33% (100*((1/5.66)-1),) *console)

##Plots

##Plot1 for data of hard coral cover for each year
plot1 <- 
mod_rcy3 |> 
  emmeans(~Year, type="response") |> #data to display
  as.data.frame() |> 
  mutate(Before = ifelse(as.numeric(as.character(Year)) < 2022, TRUE, FALSE)) |> 
  ggplot(aes(y = prob, x = Year, colour= Before)) +
  annotate(geom="rect", xmin=-Inf, xmax=2.9, ymin=-Inf, ymax=Inf, fill="green", alpha=0.2) +
  annotate(geom="rect", xmin=2.9, xmax=Inf, ymin=-Inf, ymax=Inf, fill="red", alpha=0.2) +
  geom_pointrange(aes(ymin = lower.HPD, ymax = upper.HPD)) + #
  geom_vline(xintercept=2.9, linetype="dashed") + #to make a vertical line for the date of the typhoon (note that the year are coded like 2019=1, 2021=2,...)
  scale_y_continuous("Hard coral cover (%)", labels = function(x) 100*x) + #change the unit of the scale into %coral cover to make it more readable
  scale_colour_discrete("", labels = c("Post typhoon", "Pre typhoon")) +
  theme_classic() +
  theme(legend.position = c(0.99, 0.99), legend.justification = c(1,1),
        axis.title.y = element_text(margin=margin(l=0, t=0, r=12, b=0, unit = "pt")),
        legend.title=element_blank()) +
  ggtitle("a)")
  #annotate(geom="text", x=-Inf, y = Inf, label="a)", hjust = 0, vjust = 1)

plot1
##saving the figure (plot) to output's folder
ggsave(filename = "../outputs/figures/q1_figure.png",
       plot1,
       width=150, height=100, units="mm",
       dpi = 300)

##Plot 2
plot2 <- 
mod_rcy3 |> emmeans(~Year) |> 
  pairs(reverse=TRUE) |> 
  gather_emmeans_draws() |> 
  mutate(.value=exp(.value)) |> 
  dplyr::select(-.chain) |> 
  summarise_draws(median,
                  HDInterval::hdi,  ##Highest probability density intervals of the posteriors
                  Draws = ~ length(.x), ##total number of draws (chain samples)
                  rhat, 
                  P = ~ mean(.x > 1),
                  Pl = ~ mean(.x < 1)) |> 
  mutate(Pv = max(P, Pl)) |> 
  mutate(Evidence = ifelse(as.numeric(as.character(Pv)) < 0.85, FALSE, TRUE)) |> 
  ggplot(aes(y=contrast, x=median)) +
  geom_vline(xintercept=1, linetype ="dashed") +
  geom_pointrange(aes(xmin=lower, xmax=upper)) +
  geom_text(aes(y = as.numeric(as.factor(contrast)) + 0.2, label = round(Pv, 2), colour = Evidence)) +
  scale_x_continuous("Effect size (fold scale)", trans = scales::log2_trans(), 
                     breaks = c(0.25, 0.5, 1, 2, 4)) +
  theme_classic() +
  theme(axis.title.y = element_blank(), legend.position = "none") +
  scale_color_manual(values=c("red", "darkgreen"), breaks=c(FALSE, TRUE)) +
  ggtitle("b)")
  #annotate(geom="text", x=-1, y = Inf, label="b)", hjust = 0, vjust = 1)
  
plot2

plot1 + plot2


mod_rcy3 |> emmeans(~Year) |> 
  pairs(reverse=TRUE) |> 
  gather_emmeans_draws() |> 
  mutate(.value=exp(.value)) |> 
  dplyr::select(-.chain) |> 
  
  ggplot(aes(y=contrast, x=.value)) +
  #geom_vline(xintercept=1, linetype ="dashed") +
  #geom_pointrange(aes(xmin=lower, xmax=upper)) +
  geom_density_ridges(alpha=0.3) +
  #geom_density_ridges_gradient(aes(fill=after_stat(x)), alpha=0.4, colour="white", quantile_lines=TRUE, 
  #                             quantiles = c(0.025, 0.975)) +
  #geom_text(aes(y = as.numeric(as.factor(contrast)) + 0.2, label = round(Pv, 2), colour = Evidence)) +
  geom_vline(xintercept=1, linetype ="dashed") +
  scale_x_continuous("Effect size (fold scale)", trans = scales::log2_trans(), 
                     breaks = c(0.25, 0.5, 1, 2, 4)) +
  theme_classic() +
  theme(axis.title.y = element_blank(), legend.position = "none") +
  scale_color_manual(values=c("red", "darkgreen"), breaks=c(FALSE, TRUE)) +
  scale_fill_viridis_c(option="C") +
  ggtitle("b)")
