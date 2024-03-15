library(tidyverse)

source("../Pal_Test_Repo/R/functions.R")


##reading the data from the primary data folder
data <- read_csv("../Pal_Test_Repo/data/primary/DiveSite_AbdeensRock_T1.csv")
data_T2 <- read_csv("../Pal_Test_Repo/data/primary/DiveSite_AbdeensRock_T2.csv")
data_T3 <- read_csv("../Pal_Test_Repo/data/primary/DiveSite_AbdeensRock_T3.csv")
data_PT1 <- read_csv("../Pal_Test_Repo/data/primary/Protected_MitriRock_T1.csv")
data_PT2 <- read_csv("../Pal_Test_Repo/data/primary/Protected_MitriRock_T2.csv")
data_PT3 <- read_csv("../Pal_Test_Repo/data/primary/Protected_MitriRock_T3.csv")

##checking data
data_T3 <- read_csv("../Pal_Test_Repo/data/primary/DiveSite_AbdeensRock_T3.csv")
data_PT1 <- read_csv("../Pal_Test_Repo/data/primary/Protected_MitriRock_T1.csv")
data_PT2 <- read_csv("../Pal_Test_Repo/data/primary/Protected_MitriRock_T2.csv")
data_PT3 <- read_csv("../Pal_Test_Repo/data/primary/Protected_MitriRock_T3.csv")

glimpse(data)


##To process the data

##Transect 1
data <- data |>
  cpce_classif_to_points() |>
  separate(`Frame image name`,
           into = c("Drive", "Folder",  "Site", "Transect", "Photo"),
           sep = "\\\\"
  ) |>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = `Major Category`)|>
  dplyr::select(-Drive, -Folder) |>
  mutate('Tourist Access'= "yes")


##Transect 2
data_T2 <- data_T2 |>
  cpce_classif_to_points() |>
  separate(`Frame image name`,
           into = c("Drive", "Folder",  "Site", "Transect", "Photo"),
           sep = "\\\\"
  ) |>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = `Major Category`) |>
  dplyr::select(-Drive, -Folder)|>
  mutate('Tourist Access'= "yes")

##Transect 3
data_T3 <- data_T3 |>
  cpce_classif_to_points() |>
  separate(`Frame image name`,
           into = c("Drive", "Folder",  "Site", "Transect", "Photo"),
           sep = "\\\\"
  ) |>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = `Major Category`)|>
  dplyr::select(-Drive, -Folder)|>
  mutate('Tourist Access'= "yes")


##Protected Transect 1
data_PT1 <- data_PT1 |>
  cpce_classif_to_points() |>
  separate(`Frame image name`,
           into = c("Drive", "Folder", "A", "B", "Site", "Transect", "Photo"),
           sep = "\\\\"
  ) |>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = `Major Category`)|>
  dplyr::select(-Drive, -Folder,-A,-B)|>
  mutate('Tourist Access'= "no")

##Protected Transect 2
data_PT2 <- data_PT2 |>
  cpce_classif_to_points() |>
  separate(`Frame image name`,
           into = c("Drive", "A", "B", "Folder",  "Site", "Transect", "Photo"),
           sep = "\\\\"
  ) |>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = `Major Category`)|>
  dplyr::select(-Drive, -Folder,-A,-B)|>
  mutate('Tourist Access'= "no")


##Protected Transect 3
data_PT3 <- data_PT3 |>
  cpce_classif_to_points() |>
  separate(`Frame image name`,
           into = c("Drive", "A", "B", "Folder",  "Site", "Transect", "Photo"),
           sep = "\\\\"
  ) |>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = `Major Category`) |>
  dplyr::select(-Drive, -Folder,-A,-B)|>
  mutate('Tourist Access'= "no")

## to bind all the data
all_data <- bind_rows(data,
                      data_T2,
                      data_T3,
                      data_PT1,
                      data_PT2,
                      data_PT3) |>
  dplyr::rename(tourist_access = `Tourist Access`)

##Tally up Points
#| label: count
All_Dat2 <-
  All_Dat2 |> 
  dplyr::group_by(across(
    c(starts_with("Site"),
      Year,
      Transect,
      data_tally_group,
      tourist_access))) |>
  summarise(count_groupcode = sum(total), .groups = "keep") |> 
  ungroup(Substrate) |>
  mutate(total = sum(count_groupcode)) |>
  ungroup() 
dat |> as.data.frame() |> head()

##Recode data

All_Dat2 <- 
  All_Dat2 |>
  mutate(Transect = paste0(Site, Year, tourist_acces)) 
dat |> as.data.frame() |> head()

##Time Series Plot
All_Data2 |>
  filter(data_tally_group == "HC") |> 
  ggplot(aes(y =  100*count_groupcode/total, x = Year, colour = factor(tourist_access))) +
  geom_point() +
  geom_line(aes(group = Transect)) + 
  scale_y_continuous("Hard coral cover (%)") +
  scale_x_discrete("Year") + 
  theme_classic() +
  facet_wrap(~data_tally_group) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
 
## Exploratory data analysis

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
ggsave(file = "../outputs/figures/tourist_access_plot1.png",
       width = 700, height = 500, units = "px",
      dpi=300)

ggsave(file = "../outputs/figures/tourist_access_plot1.pdf",
       width = 7, height = 5, units = "in")

library(brms)

glimpse(all_data)

form <- bf(count_groupcode | trials(total) ~ tourist_access + (1 | Site) + (1| Transect),
           family = binomial(link = "logit"))

model1 <- brm(form,
              data=all_data)

library(tidyverse)
library(rstan)
library(brms)
library(tidybayes)
library(DHARMa)
library(emmeans)
library(patchwork)
library(knitr)
library(HDInterval)

all_data <- read_csv("../Pal_Test_Repo/data/processed/q6_all_data.csv")

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
              chains = 3,
              iter = 2000,
              warmup = 1000,
              thin = 10,
              sample_prior = "only",
              backend = "rstan")

model1 |> conditional_effects() |> plot()

model1 <- model1 |> 
  update(sample_prior = "yes")

model1 |> conditional_effects() |> plot()

model1 |> plot()

priors <- prior(normal(0, 1), class = "Intercept") +
  prior(normal(0, 3), class = "b") +
  prior(gamma(0.01, 0.01), class = "phi")

form <- bf(count_groupcode | trials(total) ~ tourist_access + (1 | Site),
           family = beta_binomial(link = "logit"))

model2 <- brm(form, 
              data = all_data,
              prior = priors,
              chains = 3,
              iter = 2000,
              warmup = 1000,
              thin = 10,
              sample_prior = "only",
              backend = "rstan",
              control = list(adapt_delta = 0.99, max_treedepth = 20))

model2 |> conditional_effects() |> plot()

model2 <- model2 |> 
  update(sample_prior = "yes")

model2 |> plot()

get_prior(form, data=all_data)

summary(model2)

summary(model1)

model2$fit |> stan_trace()

model2$fit |> stan_ac()

model2$fit |> stan_rhat()

model2$fit |> stan_ess()

model2 |> pp_check(type = "dens_overlay", ndraws = 100)

resids <- model2 |> make_brms_dharma_res(integerResponse = FALSE)

testUniformity(resids)

plotResiduals(resids, form = factor(rep(1, nrow(all_data))))

plotResiduals(resids)

testDispersion(resids)

summary(model1)

model2 |> emmeans(~ tourist_access, type = "response")

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
  geom_vline(xintercept = 0,  linetype = "dashed")

model2 |> 
  bayes_R2(summary = FALSE) |> 
  median_hdci()

plot3 <- all_data |> ggplot() +
  geom_boxplot(aes(x = Site, y = cover, fill = tourist_access)) +
  ggtitle("Hard Coral Cover") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
