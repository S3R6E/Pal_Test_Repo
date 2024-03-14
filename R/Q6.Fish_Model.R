library(tidyverse) 

source ("functions.R")


data_Helicopter_Fish2022 <- read_csv("../data/primary/Helicopter_Fish_22.csv")
data_Helicopter_Fish2023 <- read_csv("../data/primary/Helicopter_fish_23.csv")
data_Pagawanen_Fish2022 <- read_csv("../data/primary/Pagawanen_Fish_22.csv")
data_Pagawanen_Fish2023 <- read_csv("../data/primary/Pagawanen_Fish_23.csv")

glimpse(data_Helicopter_Fish2022)

data_Helicopter_Fish2022 <- fish_survey_to_summary(data_Helicopter_Fish2022) |>
  mutate(Site = "Helicopter", Year = "2022")
data_Helicopter_Fish2023 <- fish_survey_to_summary(data_Helicopter_Fish2023) |>
  mutate(Site = "Helicopter", Year = "2023")
data_Pagawanen_Fish2022 <- fish_survey_to_summary(data_Pagawanen_Fish2022) |>
  mutate(Site = "Pagawanen", Year = "2022")
data_Pagawanen_Fish2023 <- fish_survey_to_summary(data_Pagawanen_Fish2023) |>
  mutate(Site = "Pagawanen", Year = "2023")

data_Fish <- data_Helicopter_Fish2022 |>
  bind_rows(data_Helicopter_Fish2023) |>
  bind_rows(data_Pagawanen_Fish2022) |>
  bind_rows(data_Pagawanen_Fish2023)

data_Fish |> ggplot() +
  geom_point(aes(x = Site, y = spp_rich, colour = Year))

data_Fish |> ggplot() +
  geom_point(aes(x = Site, y = total_count, colour = Year))

data_Fish |> ggplot() +
  geom_point(aes(x = Site, y = total_biomass, colour = Year))

library(tidyverse)
library(rstan)
library(brms)
library(tidybayes)
library(DHARMa)
library(emmeans)
library(patchwork)

form <- bf(spp_rich ~ Year + (1 | Site),
           family = poisson(link = "log"))

get_prior(form, data=data_Fish)

priors <- prior(normal(0,1), class = "Intercept") +
  prior(normal(0,1), class = "b") +
  prior(student_t(3,0,1), class = "sd")

model2 <- brm(form, 
              data = data_Fish,
              prior = priors,
              chains = 3,
              iter = 2000,
              warmup = 1000,
              thin = 10,
              sample_prior = "yes",
              backend = "rstan")

model2 |> conditional_effects() |> plot()

model2 |> plot()

summary(model1)

model2$fit |> stan_trace()

model2$fit |> stan_ac()

model2$fit |> stan_rhat()

model2$fit |> stan_ess()

model2 |> pp_check(type = "dens_overlay", ndraws = 100)

resids <- model2 |> make_brms_dharma_res(integerResponse = FALSE)

testUniformity(resids)

plotResiduals(resids, form = factor(rep(1, nrow(data_Fish))))

plotResiduals(resids)

testDispersion(resids)

summary(model2)
