library(tidyverse)

<<<<<<< HEAD
source("../Pal_Test_Repo/R/functions.R")
=======

source("C:/Users/asust/OneDrive/Desktop/Rstudio/Pal_Test_Repo/Copyoffunctions.R")
>>>>>>> 5ab1b7f0cd791e66a48ba4abe3060336a0f22963

##reading the data from the primary data folder
data <- read_csv("../Pal_Test_Repo/data/primary/DiveSite_AbdeensRock_T1.csv")
glimpse(data)
data_T2 <- read_csv("../Pal_Test_Repo/data/primary/DiveSite_AbdeensRock_T2.csv")
glimpse(data_T2)
<<<<<<< HEAD
data_T3 <- read_csv("../Pal_Test_Repo/data/primary/DiveSite_AbdeensRock_T3.csv")
data_PT1 <- read_csv("../Pal_Test_Repo/data/primary/Protected_MitriRock_T1.csv")
data_PT2 <- read_csv("../Pal_Test_Repo/data/primary/Protected_MitriRock_T2.csv")
data_PT3 <- read_csv("../Pal_Test_Repo/data/primary/Protected_MitriRock_T3.csv")

##checking data
=======
data_T3 <- read_csv("../data/primary/DiveSite_AbdeensRock_T3.csv")
data_PT1 <- read_csv("../data/primary/Protected_MitriRock_T1.csv")
data_PT2 <- read_csv("../data/primary/Protected_MitriRock_T2.csv")
data_PT3 <- read_csv("../data/primary/Protected_MitriRock_T3.csv")
>>>>>>> 5ab1b7f0cd791e66a48ba4abe3060336a0f22963
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
<<<<<<< HEAD
                      data_PT3) |> 
  dplyr::rename(tourist_access = 'Tourist Access')
=======
                      data_PT3) |>
  dplyr::rename(tourist_access = `Tourist Access`)
>>>>>>> 8ea8d0f00a93e74bba6572529d8e837b80992256
 
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

<<<<<<< HEAD
<<<<<<< HEAD
library(brms)

glimpse(all_data)

form <- bf(count_groupcode | trials(total) ~ tourist_access + (1 | Site) + (1| Transect),
           family = binomial(link = "logit"))

model1 <- brm(form,
              data=all_data)
=======
=======
library(tidyverse)
>>>>>>> 5ab1b7f0cd791e66a48ba4abe3060336a0f22963
library(rstan)
library(brms)
library(tidybayes)
library(DHARMa)
library(emmeans)
library(patchwork)

glimpse(all_data)

form <- bf(count_groupcode | trials(total) ~ tourist_access + (1 | Site),
           family = binomial(link = "logit"))

get_prior(form, data=all_data)

priors <- prior(normal(0,1), class = "Intercept") +
  prior(normal(0,1), class = "b") +
  prior(student_t(3,0,3), class = "sd")

model1 <- brm(form, 
              data = all_data,
              prior = priors,
              chains = 3,
              iter = 4000,
              warmup = 1000,
              thin = 10,
              sample_prior = "only",
              backend = "rstan")

model1 |> conditional_effects() |> plot()

model1 <- model1 |> 
  update(sample_prior = "yes")

>>>>>>> 8ea8d0f00a93e74bba6572529d8e837b80992256
model1 |> conditional_effects() |> plot()

model1 |> plot()

<<<<<<< HEAD
form <- bf(count_groupcode | trials(total) ~ tourist_access + (1 | Site) + (1| Transect),
           family = beta_binomial(link = "logit"))
model2 <- brm(form,
             data=all_data)
model2 |> conditional_effects() |> plot()
model2 |> plot()

get_prior(form, data=all_data)

summary(model2)
=======
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
<<<<<<< HEAD
``
>>>>>>> 8ea8d0f00a93e74bba6572529d8e837b80992256
=======

summary(model1)

model1 |> emmeans(~ tourist_access, type = "response")

model1 |> emmeans(~ tourist_access, type = "response") |>
  pairs()
>>>>>>> 5ab1b7f0cd791e66a48ba4abe3060336a0f22963
