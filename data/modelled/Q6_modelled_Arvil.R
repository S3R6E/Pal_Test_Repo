library(brms)
load(file = "../data/processed/Q6_all_data_Arvil.Rdata")
source("functions.R")

glimpse(all_data)

form <- bf(count_groupcode | trials(total) ~ tourist_access + (1 | Site) + (1| Transect),
           family = binomial(link = "logit"))
model1 <- brm(form, 
              data = all_data)
model1 |> conditional_effects() |> plot()

model1 |> plot()

get_prior(form, data=all_data)

summary(model1)


library(rstan)
library(brms)
library(DHARMa)
library(patchwork)

glimpse(all_data)



form <- bf(count_groupcode | trials(total) ~ tourist_access + (1 | Site),
           family = binomial(link = "logit"))

get_prior(form, data=all_data)

priors <- prior(normal(0,1), class = "Intercept") +
  prior(normal(0,1), class = "b") +
  prior(student_t(3,0,1), class = "sd")

model1 <- brm(form, 
              data = all_data,
              prior = priors,
              chains = 4,
              iter = 4000,
              warmup = 1000,
              seed = 3000,
              thin = 10,
              sample_prior = "only",
              backend = "rstan")

model1 |> conditional_effects() |> plot()

model1 <- model1 |> 
  update(sample_prior = "yes")

model1 |> conditional_effects() |> plot()

model1 |> plot()

summary(model1)

model1$fit |> stan_trace()

model1$fit |> stan_ac()

model1$fit |> stan_rhat()

model1$fit |> stan_ess()

model1 |> pp_check(type = "dens_overlay", ndraws = 100)

resids <- model1 |> make_brms_dharma_res(integerResponse = FALSE)

testUniformity(resids)

plotResiduals(resids, form = factor(rep(1, nrow(all_data))))

plotResiduals(resids)

testDispersion(resids)

summary(model1)
``