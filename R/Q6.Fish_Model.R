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
library(rnaturalearth)
library(sf)
library(ggspatial)



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
              sample_prior = "Yes",
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

model2 |> emmeans(~ spp_rich, type = "response")

model2 |> emmeans(~ tourist_access, type = "response") |>
  pairs()

model2 |> 
  emmeans(~tourist_access) |> 
  regrid() |> 
  pairs() |> 
  gather_emmeans_draws() |> 
  ggplot(aes(x = .value, y = contrast)) +
  stat_halfeye(aes(fill = after_stat(level)), .width = c(0.66, 0.95, 1)) +
  scale_fill_brewer() +
  geom_vline(xintercept = 0, linetype = "dashed")


plot1 <-all_data |> 
  group_by(tourist_access) |> 
  summarise(Mean = mean(cover),
            SD = sd(cover)) |> 
  mutate(lower = Mean -SD,
         upper = Mean + SD) |> 
  ungroup() |> 
  ggplot(aes(y = Mean, x = tourist_access)) +
  geom_pointrange(aes(ymin=lower, ymax=upper)) +
  scale_y_continuous("Hard coral cover (%)", labels = function(x) x*100) +
  theme_classic(10)
plot1

plot2 <- all_data |>
  group_by(Site) |>
  summarise(Mean = mean(cover),
            SD = sd(cover)) |>
  mutate(lower = Mean - SD,
         upper = Mean + SD) |>
  ungroup() |>
  ggplot(aes(y = Mean, x = Site)) +
  geom_pointrange(aes(ymin=lower, ymax=upper)) +
  scale_y_continuous("Hard coral cover (%)", labels = function(x) x*100) +
  theme_classic(10)
plot2

plot3 <- all_data |> ggplot() +
  geom_boxplot(aes(x = Site, y = cover, fill = tourist_access)) +
  ggtitle("Hard Coral Cover") +
  theme(axis.text.x = element_text(angle=30, hjust = 1))


ggsave(plot3, file = "../outputs/figures/tourist_access_plot3.png",
       width = 20, height = 10, units = "cm",
       dpi=300)


