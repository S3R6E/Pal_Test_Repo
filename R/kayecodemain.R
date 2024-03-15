library(tidyverse) 

source ("functions.R")

## to read files

data_PT1 <- read_csv("../data/primary/Protected_MitriRock_T1.csv")
data_PT2 <- read_csv("../data/primary/Protected_MitriRock_T2.csv")
data_PT3 <- read_csv("../data/primary/Protected_MitriRock_T3.csv")
data_T1 <- read_csv("../data/primary/DiveSite_AbdeensRock_T1.csv")
data_T2 <- read_csv("../data/primary/DiveSite_AbdeensRock_T2.csv")
data_T3 <- read_csv("../data/primary/DiveSite_AbdeensRock_T3.csv")
data_PPT1 <- read_csv("../data/primary/Tourist_Helicopter_T1.csv")
data_PPT2 <- read_csv("../data/primary/Tourist_Helicopter_T2.csv")
data_PPT3 <- read_csv("../data/primary/Tourist_Helicopter_T3.csv")
data_TT1 <- read_csv("../data/primary/Pagawanen_T1.csv")
data_TT2 <- read_csv("../data/primary/Pagawanen_T2.csv")
data_TT3 <- read_csv("../data/primary/Pagawanen_T3.csv")

##to process data, modify the graph, eliminate the columns or data that we don't need 
##and to add another column for tourist access to differentiate the data.

<<<<<<< HEAD
##to process data, modify the graph, eliminate the columns or data that we don't need 
##and to add another column for tourist access to differentiate the data.

=======
>>>>>>> 74c4c9f6c8646f9f20852f088c1014927b436203
##transect PT1 

glimpse(data_PT1)

data_PT1 <- data_PT1 |>
  cpce_classif_to_points() |>
  separate('Frame image name',
           into = c("Drive", "A", "B", "Folder","Site","Transect","Photo"),
           sep = "\\\\"
  )|>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = 'Major Category')|>
  dplyr::select(-A, -B, -Drive, -Folder)|>
  mutate('Tourist Access'= "no")

##do the same for the rest of the transect and site.

##transect PT2

data_PT2 <- read_csv("../data/primary/Protected_MitriRock_PT2.csv")

glimpse(data_PT2)

data_PT2 <- data_PT2 |>
  cpce_classif_to_points() |>
  separate('Frame image name',
           into = c("Drive", "A", "B", "Folder","Site","Transect","Photo"),
           sep = "\\\\"
  ) |>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = 'Major Category')|>
  dplyr::select(-A, -B, -Drive, -Folder)|>
  mutate('Tourist Access'= "no")


##transect PT3

data_PT3 <- read_csv("../data/primary/Protected_MitriRock_T3.csv")

glimpse(data_PT3)

data_PT3 <- data_PT3 |>
  cpce_classif_to_points() |>
  separate('Frame image name',
           into = c("Drive", "A", "B", "Folder","Site","Transect","Photo"),
           sep = "\\\\"
  ) |>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = 'Major Category')|>
  dplyr::select(-A, -B, -Drive, -Folder)|>
  mutate('Tourist Access'= "no")

##transect T1

data_T1 <- read_csv("../data/primary/DiveSite_AbdeensRock_T1.csv")

glimpse(data_T1)

data_T1 <- data_T1 |>
  cpce_classif_to_points() |>
  separate('Frame image name',
           into = c("Drive", "Folder","Site","Transect","Photo"),
           sep = "\\\\"
  ) |>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = 'Major Category')|>
  dplyr::select(-Drive, -Folder)|>
  mutate('Tourist Access'= "yes")

##transect T2
data_T2 <- read_csv("../data/primary/DiveSite_AbdeensRock_T2.csv")

glimpse(data_T2)

data_T2 <- data_T2 |>
  cpce_classif_to_points() |>
  separate('Frame image name',
           into = c("Drive", "Folder","Site","Transect","Photo"),
           sep = "\\\\"
  ) |>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = 'Major Category')|>
  dplyr::select(-Drive, -Folder)|>
  mutate('Tourist Access'= "yes")

##transect T3
data_T3 <- read_csv("../data/primary/DiveSite_AbdeensRock_T2.csv")

glimpse(data_T3)

data_T3 <- data_T3 |>
  cpce_classif_to_points() |>
  separate('Frame image name',
           into = c("Drive", "Folder","Site","Transect","Photo"),
           sep = "\\\\"
  ) |>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = 'Major Category')|>
  dplyr::select(-Drive, -Folder)|>
  mutate('Tourist Access'= "yes")

<<<<<<< HEAD
=======

##data PPT1
glimpse(data_PPT1)

data_PPT1 <- data_PPT1 |>
  cpce_classif_to_points() |>
  separate('Frame image name',
           into = c("Drive", "A", "B", "Folder","Site","Transect","Photo"),
           sep = "\\\\"
  )|>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = 'Major Category')|>
  dplyr::select(-A, -B, -Drive, -Folder)|>
  mutate('Tourist Access'= "no")

##data PPT2
glimpse(data_PPT2)

data_PPT2 <- data_PPT2 |>
  cpce_classif_to_points() |>
  separate('Frame image name',
           into = c("Drive", "A", "B", "Folder","Site","Transect","Photo"),
           sep = "\\\\"
  )|>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = 'Major Category')|>
  dplyr::select(-A, -B, -Drive, -Folder)|>
  mutate('Tourist Access'= "no")


##data PPT3
glimpse(data_PPT3)

data_PPT3 <- data_PPT3 |>
  cpce_classif_to_points() |>
  separate('Frame image name',
           into = c("Drive", "A", "B", "Folder","Site","Transect","Photo"),
           sep = "\\\\"
  )|>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = 'Major Category')|>
  dplyr::select(-A, -B, -Drive, -Folder)|>
  mutate('Tourist Access'= "no")

##data TT1
glimpse(data_TT1)

data_TT1 <- data_TT1 |>
  cpce_classif_to_points() |>
  separate('Frame image name',
           into = c("Drive", "A", "B", "Folder","Site","Transect","Photo"),
           sep = "\\\\"
  )|>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = 'Major Category')|>
  dplyr::select(-A, -B, -Drive, -Folder)|>
  mutate('Tourist Access'= "yes")

##data TT2
glimpse(data_TT2)

data_TT2 <- data_TT2 |>
  cpce_classif_to_points() |>
  separate('Frame image name',
           into = c("Drive", "A", "B", "Folder","Site","Transect","Photo"),
           sep = "\\\\"
  )|>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = 'Major Category')|>
  dplyr::select(-A, -B, -Drive, -Folder)|>
  mutate('Tourist Access'= "yes")

##data TT3
glimpse(data_TT3)

data_TT3 <- data_TT3 |>
  cpce_classif_to_points() |>
  separate('Frame image name',
           into = c("Drive", "A", "B", "Folder","Site","Transect","Photo"),
           sep = "\\\\"
  )|>
  filter(`Major Category` == "HC") |>
  droplevels() |>
  mutate(cover = count_groupcode / total) |>
  rename("data_tally_group" = 'Major Category')|>
  dplyr::select(-A, -B, -Drive, -Folder)|>
  mutate('Tourist Access'= "yes")

>>>>>>> 74c4c9f6c8646f9f20852f088c1014927b436203
##to combine all the data into one

all_data <- bind_rows(data_T1,
                      data_T2,
                      data_T3,
                      data_PT1,
                      data_PT2,
                      data_PT3,
                      data_PPT1,
                      data_PPT2,
                      data_PPT3,
                      data_TT1,
                      data_TT2,
                      data_TT3)|>
  dplyr::rename(tourist_access = `Tourist Access`)

## Exploratory data analysis and plotting the graph from all data

plot1 <-all_data |> 
  group_by(tourist_access) |> 
  summarise(Mean = mean(cover),
            SD = sd(cover)) |> 
  mutate(lower = Mean -SD,
         upper = Mean + SD) |> 
  ungroup() |> 
<<<<<<< HEAD
  ggplot(aes(y = Mean, x = `Tourist Access`, colour= `Tourist Access`))+
=======
  ggplot(aes(y = Mean, x = tourist_access, colour= tourist_access))+
>>>>>>> 74c4c9f6c8646f9f20852f088c1014927b436203
  geom_pointrange(aes(ymin=lower, ymax=upper)) +
  scale_y_continuous("Hard coral cover (%)", labels = function(x) x*100)+
  scale_colour_manual(values = c("magenta", "cyan"))+
  theme_classic(10) 

plot1 

##to make and save a PNG file of the graph
ggsave(file = "../outputs/figures/tourist_access_plot1.png",
       width = 700, height = 500, units = "px",
       dpi=300)

##to make and save pdf copy of the graph 
ggsave(file = "../outputs/figures/tourist_access_plot1.pdf",
       width = 7, height = 5, units = "in")

<<<<<<< HEAD
=======
##making a model 

library(tidyverse)
library(rstan)
library(brms)
library(tidybayes)
library(DHARMa)
library(emmeans)
library(patchwork)

all_data <- read_csv("../data/processed/q6_all_data.csv")

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

model1 |> emmeans(~ tourist_access, type = "response")

model1 |> emmeans(~ tourist_access, type = "response") |>
  pairs()

model1 |> 
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

>>>>>>> 74c4c9f6c8646f9f20852f088c1014927b436203
