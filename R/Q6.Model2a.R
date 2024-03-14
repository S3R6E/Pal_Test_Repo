library(tidyverse) 

setwd("R")

source ("functions.R")

## to read 2023 files

data_HeliC23_T1 <- read_csv("../data/primary/Helicopter_corals1_23.csv")
data_HeliC23_T2 <- read_csv("../data/primary/Helicopter_corals2_23.csv")
data_HeliC23_T3 <- read_csv("../data/primary/Helicopter_corals3_23.csv")
data_PagaC23_T1 <- read_csv("../data/primary/Pagawanen_corals1_23.csv")
data_PagaC23_T2 <- read_csv("../data/primary/Pagawanen_corals2_23.csv")
data_PagaC23_T3 <- read_csv("../data/primary/Pagawanen_corals3_23.csv")


##To read and process files

data_HeliC23_T1 <- read_csv("../data/primary/Helicopter_corals1_23.csv")
glimpse(data_HeliC23_T1)

data_HeliC23_T1 <- data_HeliC23_T1 |>
  cpce_classif_to_points() |>
  separate('Frame image name',
           into = c("Drive", "A","Site","Transect","Photo"),
           sep = "\\\\"
  )|>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = 'Major Category')|>
  dplyr::select(-Drive, -A)|>
  mutate('tourist_access'= "Yes") |> 
  mutate('Year'= "2023")


data_HeliC23_T2 <- read_csv("../data/primary/Helicopter_corals2_23.csv")

glimpse(data_HeliC23_T2)

data_HeliC23_T2 <- data_HeliC23_T2 |>
  cpce_classif_to_points() |>
  separate('Frame image name',
           into = c("Drive", "A","Site","Transect","Photo"),
           sep = "\\\\"
  )|>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = 'Major Category')|>
  dplyr::select(-Drive, -A)|>
  mutate('tourist_access'= "Yes") |> 
  mutate('Year'= "2023")

data_HeliC23_T3 <- read_csv("../data/primary/Helicopter_corals3_23.csv")
glimpse(data_HeliC23_T3)

data_HeliC23_T3 <- data_HeliC23_T3 |>
  cpce_classif_to_points() |>
  separate('Frame image name',
           into = c("Drive", "A","Site","Transect","Photo"),
           sep = "\\\\"
  )|>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = 'Major Category')|>
  dplyr::select(-Drive, -A)|>
  mutate('tourist_access'= "Yes") |> 
  mutate('Year'= "2023")

data_PagaC23_T1 <- read_csv("../data/primary/Pagawanen_corals1_23.csv")
glimpse(data_PagaC23_T1)

data_PagaC23_T1 <- data_PagaC23_T1 |>
  cpce_classif_to_points() |>
  separate('Frame image name',
           into = c("Drive", "A","Site","Transect","Photo"),
           sep = "\\\\"
  )|>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = 'Major Category')|>
  dplyr::select(-Drive, -A)|>
  mutate('tourist_access'= "No") |> 
  mutate('Year'= "2023")

data_PagaC23_T2 <- read_csv("../data/primary/Pagawanen_corals2_23.csv")
glimpse(data_PagaC23_T2)



data_PagaC23_T2 <- data_PagaC23_T2 |>
  cpce_classif_to_points() |>
  separate('Frame image name',
           into = c("Drive", "A","Site","Transect","Photo"),
           sep = "\\\\"
  )|>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = 'Major Category')|>
  dplyr::select(-Drive, -A)|>
  mutate('tourist_access'= "No") |> 
  mutate('Year'= "2023")

data_PagaC23_T3 <- read_csv("../data/primary/Pagawanen_corals3_23.csv")
glimpse(data_PagaC23_T3)

data_PagaC23_T3 <- data_PagaC23_T3 |>
  cpce_classif_to_points() |>
  separate('Frame image name',
           into = c("Drive", "A","Site","Transect","Photo"),
           sep = "\\\\"
  )|>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = 'Major Category')|>
  dplyr::select(-Drive, -A)|>
  mutate('tourist_access'= "No") |> 
  mutate('Year'= "2023")

##to read 2022 files

data_HeliC22_T1 <- read_csv("../data/primary/Tourist_Helicopter_T1.csv")
data_HeliC22_T2 <- read_csv("../data/primary/Tourist_Helicopter_T2.csv")
data_HeliC22_T3 <- read_csv("../data/primary/Tourist_Helicopter_T3.csv")
data_PagaC22_T1 <- read_csv("../data/primary/Pagawanen_T1.csv")
data_PagaC22_T2 <- read_csv("../data/primary/Pagawanen_T2.csv")
data_PagaC22_T3 <- read_csv("../data/primary/Pagawanen_T3.csv")

##to read and process files
data_HeliC22_T1 <- read_csv("../data/primary/Tourist_Helicopter_T1.csv")
glimpse(data_HeliC22_T1)

data_HeliC22_T1 <- data_HeliC22_T1 |>
  cpce_classif_to_points() |>
  separate('Frame image name',
           into = c("Drive", "A","Site","Transect","Photo"),
           sep = "\\\\"
  )|>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = 'Major Category')|>
  dplyr::select(-Drive, -A)|>
  mutate('tourist_access'= "Yes") |> 
  mutate('Year'= "2022")


data_HeliC22_T2 <- read_csv("../data/primary/Tourist_Helicopter_T2.csv")

glimpse(data_HeliC22_T2)

data_HeliC22_T2 <- data_HeliC22_T2 |>
  cpce_classif_to_points() |>
  separate('Frame image name',
           into = c("Drive", "A","Site","Transect","Photo"),
           sep = "\\\\"
  )|>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = 'Major Category')|>
  dplyr::select(-Drive, -A)|>
  mutate('tourist_access'= "Yes") |> 
  mutate('Year'= "2022")

data_HeliC22_T3 <- read_csv("../data/primary/Tourist_Helicopter_T3.csv")
glimpse(data_HeliC22_T3)



data_HeliC22_T3 <- data_HeliC22_T3 |>
  cpce_classif_to_points() |>
  separate('Frame image name',
           into = c("Drive", "A","Site","Transect","Photo"),
           sep = "\\\\"
  )|>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = 'Major Category')|>
  dplyr::select(-Drive, -A)|>
  mutate('tourist_access'= "Yes") |> 
  mutate('Year'= "2022")



data_PagaC22_T1 <- read_csv("../data/primary/Pagawanen_T1.csv")
glimpse(data_PagaC22_T1)

data_PagaC22_T1 <- data_PagaC22_T1 |>
  cpce_classif_to_points() |>
  separate('Frame image name',
           into = c("Drive","A","B","C","D","E","F","Site","Transect","Photo"),
           sep = "\\\\"
  )|>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = 'Major Category')|>
  dplyr::select(-Drive, -A, -B, -C, -D, -E, -F )|>
  mutate('tourist_access'= "No") |> 
  mutate('Year'= "2022")

data_PagaC22_T2 <- read_csv("../data/primary/Pagawanen_T2.csv")
glimpse(data_PagaC22_T2)

data_PagaC22_T2 <- data_PagaC22_T2 |>
  cpce_classif_to_points() |>
  separate('Frame image name',
           into = c("Drive","A","B","C","D","E","F","Site","Transect","Photo"),
           sep = "\\\\"
  )|>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = 'Major Category')|>
  dplyr::select(-Drive, -A, -B, -C, -D, -E, -F )|>
  mutate('tourist_access'="No") |> 
  mutate('Year'= "2022")

data_PagaC22_T3 <- read_csv("../data/primary/Pagawanen_T3.csv")
glimpse(data_PagaC22_T3)

data_PagaC22_T3 <- data_PagaC22_T3 |>
  cpce_classif_to_points() |>
  separate('Frame image name',
           into = c("Drive","A","B","C","D","E","F","Site","Transect","Photo"),
           sep = "\\\\"
  )|>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = 'Major Category')|>
  dplyr::select(-Drive, -A, -B, -C, -D, -E, -F )|>
  mutate('tourist_access'= "No") |> 
  mutate('Year'= "2022")

##to bind files
Q6.All_Data2 <- bind_rows(data_HeliC22_T1,
                      data_HeliC22_T2,
                      data_HeliC22_T3,
                      data_HeliC23_T1,
                      data_HeliC23_T2,
                      data_HeliC23_T3,
                      data_PagaC22_T1,
                      data_PagaC22_T2,
                      data_PagaC22_T3,
                      data_PagaC23_T1,
                      data_PagaC23_T2,
                      data_PagaC23_T3)

save(Q6.All_Data2, file = "../data/processed/Q6.All_Data2.Rdata")

load(file = "../data/processed/Q6.All_Data2.Rdata")

##to plot the Hardcoral cover in each site
plot1b <- Q6.All_Data2 |> 
  group_by(Site, Year) |> 
  summarise(Mean = mean(cover),
            SD = sd(cover)) |> 
  mutate(lower = Mean -SD,
         upper = Mean + SD) |> 
  ungroup() |> 
  ggplot(aes(y = Mean, x = Site)) +
  geom_pointrange(aes(ymin=lower, ymax=upper, color = Year)) +
  scale_y_continuous("Hard coral cover (%)", labels = function(x) x*100) +
  theme_classic(10)
plot1b

##to see each name of sites
Q6.All_Data2$Site |> unique()

#to rename sites into 2 site names

Q6.All_Data2 <- Q6.All_Data2 |>
  mutate(Site = case_when(
    Site == "Site2-Helicopter island" ~ "Helicopter",
    Site == "Helicopter Island" ~ "Helicopter",
    Site == "Site4-Pagawanen beach" ~ "Pagawanen",
    Site == "So. Pagawanen" ~ "Pagawanen"
  ))

##to check if the changing of names worked

Q6.All_Data2$Site |> unique()

##to plot the data by Year ---You can change the "Year" to "tourist_access"

plot1a <- Q6.All_Data2 |> ggplot() +
  geom_boxplot(aes(x = Site, y = cover, fill = Year)) +
  ggtitle("Hard Coral Cover") +
  theme_bw(base_size = 16) +
  theme(axis.text.x = element_text(angle=30, hjust = 1))
  
plot1a

<<<<<<< HEAD

library(tidyverse)
library(rstan)
library(brms)
library(tidybayes)
library(DHARMa)
library(emmeans)
library(patchwork)
=======
ggsave(plot1a, file = "../outputs/figures/yearsite_plot1.png",
       width = 10, height = 10, units = "in",
       dpi=300)
>>>>>>> 74cc0fd6b104e74e8ec575966264556afab6fff9

##Binomial Model by Site. 

form2 <- bf(count_groupcode | trials(total) ~ Site + (1 | Transect),
           family = binomial(link = "logit"))

bf##Modelling -- To set the priors

get_prior(form2, data=Q6.All_Data2)

priors <- prior(normal(0,1), class = "Intercept") +
  prior(normal(0,1), class = "b") +
  prior(student_t(3,0,1), class = "sd")

##Modelling -- to test the priors

model_time1 <- brm(form2, 
              data = Q6.All_Data2,
              prior = priors,
              chains = 3,
              iter = 2000,
              warmup = 1000,
              thin = 10,
              sample_prior = "only",
              backend = "rstan")

model_time1 |> conditional_effects() |> plot()

##Modelling -- testing the data with the priors

model_time1 <- model_time1 |> 
  update(sample_prior = "yes")

## to check if the model fits the data (Model Validation)

model_time1$fit |> stan_trace()

model_time1$fit |> stan_ac()

model_time1$fit |> stan_rhat()

model_time1$fit |> stan_ess()

##posterior probability checks

model_time1 |> pp_check(type = "dens_overlay", ndraws = 100)

resids <- model_time1 |> make_brms_dharma_res(integerResponse = FALSE)

testUniformity(resids)

plotResiduals(resids, form = factor(rep(1, nrow(Q6.All_Data2))))

plotResiduals(resids)

testDispersion(resids)

summary(model_time1)

##pairwise contrasts

model_time1 |> emmeans(~ Site, type = "response")

model_time1  |> emmeans(~ Site, type = "response") |>
  pairs()

model_time1  |> 
  emmeans(~Site) |> 
  regrid() |> 
  pairs() |> 
  gather_emmeans_draws() |> 
  ggplot(aes(x = .value, y = contrast)) +
  stat_halfeye(aes(fill = after_stat(level)), .width = c(0.66, 0.95, 1)) +
  scale_fill_brewer() +
  geom_vline(xintercept = 0, linetype = "dashed")


form3 <- bf(count_groupcode | trials(total) ~ Year + (1 | Site),
            family = binomial(link = "logit"))

plot1c <-Q6.All_Data2 |> 
  group_by(Year) |> 
  summarise(Mean = mean(cover),
            SD = sd(cover)) |> 
  mutate(lower = Mean -SD,
         upper = Mean + SD) |> 
  ungroup() |> 
  ggplot(aes(y = Mean, x = Year)) +
  geom_pointrange(aes(ymin=lower, ymax=upper)) +
  scale_y_continuous("Hard coral cover (%)", labels = function(x) x*100) +
  theme_classic(10)

plot1c


plot3a <- Q6.All_Data2 |> ggplot() +
    geom_boxplot(aes(x = Year, y = cover, fill = Site)) +
    ggtitle("Hard Coral Cover") +
    theme(axis.text.x = element_text(angle=30, hjust = 1))
plot3a

<<<<<<< HEAD
plot2 <- Q6.All_Data2 |>
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

plot3 <- Q6.All_Data2|> ggplot() +
  geom_boxplot(aes(x = Site, y = cover, fill = tourist_access)) +
  ggtitle("Hard Coral Cover") +
  theme(axis.text.x = element_text(angle=30, hjust = 1))
=======
##Modelling -- To set the priors

get_prior(form3, data=Q6.All_Data2)

priors <- prior(normal(0,1), class = "Intercept") +
  prior(normal(0,1), class = "b") +
  prior(student_t(3,0,1), class = "sd")

##Modelling -- to test the priors

model_year <- brm(form3, 
              data = Q6.All_Data2,
              prior = priors,
              chains = 3,
              iter = 4000,
              warmup = 2000,
              thin = 10,
              sample_prior = "yes",
              backend = "rstan")

model_year |> conditional_effects() |> plot()

##Modelling -- testing the data with the priors

model_year <- model_year |> 
  update(sample_prior = "yes")

## to check if the model fits the data (Model Validation)

model_year$fit |> stan_trace()

model_year$fit |> stan_ac()

model_year$fit |> stan_rhat()

model_year$fit |> stan_ess()

##posterior probability checks

model_year |> pp_check(type = "dens_overlay", ndraws = 100)

resids <- model_year |> make_brms_dharma_res(integerResponse = FALSE)

testUniformity(resids)

plotResiduals(resids, form = factor(rep(1, nrow(Q6.All_Data2))))

plotResiduals(resids)

testDispersion(resids)

summary(model_year)

##pairwise contrasts

model_year |> emmeans(~Year, type = "response")

model_year  |> emmeans(~ Year, type = "response") |>
  pairs()

model_year  |> 
  emmeans(~Year) |> 
  regrid() |> 
  pairs() |> 
  gather_emmeans_draws() |> 
  ggplot(aes(x = .value, y = contrast)) +
  stat_halfeye(aes(fill = after_stat(level)), .width = c(0.66, 0.95, 1)) +
  scale_fill_brewer() +
  geom_vline(xintercept = 0, linetype = "dashed")
>>>>>>> 74cc0fd6b104e74e8ec575966264556afab6fff9


