library(tidyverse)
library(brms)
library(cmdstanr)

data_rc <- read.csv("../data/primary/data-coral-cover.csv")
labelset_rc <- read.csv("../data/primary/data-coral-cover-labelset.csv")

data_rc |> glimpse()

data_rc <- data_rc |> 
  dplyr::select(project_id,
                project_name,
                site_id,
                site_name,
                site_latitude,
                site_longitude,
                site_reef_name,
                site_exposure,
                site_reef_type,
                site_reef_zone,
                site_management,
                survey_id,
                survey_title,
                survey_start_date..UTC.,
                survey_depth,
                survey_transect_number, 
                image_id,
                image_disabled,
                point_machine_classification,
                point_human_classification
  ) |> 
  rename(survey_start_date = survey_start_date..UTC.) |> 
  dplyr::filter(image_disabled == "False") |> 
  select(-image_disabled)

data_rc <- data_rc |> 
  pivot_longer(cols = matches("point_.*_classification"),
               names_to = "type",
               values_to = "classification"
  ) 

data_rc <-
  data_rc |>
  left_join(labelset_rc |>
              dplyr::select(CODE, GROUP = `FUNCTIONAL.GROUP`),
            by = c("classification" = "CODE")
  ) |> 
  mutate(transect_name = paste(site_name, year(survey_start_date), survey_transect_number, sep ="_"),
         transect_id = paste0(site_id, year(survey_start_date), survey_transect_number)) |>
  mutate(year = lubridate::year(survey_start_date))

data_rc_cover <- 
  data_rc |> 
  group_by(across(c(starts_with("site"),
                    starts_with("survey"),
                    starts_with("transect"),
                    year,
                    type,
                    image_id,
                    classification,
                    GROUP))
  ) |>
  summarise(COUNT = n(), .groups = "keep") |> 
  ungroup(GROUP) |>
  mutate(TOTAL = sum(COUNT)) |>
  ungroup() 

GROUPS <- data_rc_cover |> pull(GROUP) |> unique()
filler <- data_rc_cover %>%
  dplyr::select(
    starts_with("site"),
    survey_start_date,
    survey_depth,
    transect_name,
    transect_id,
    image_id,
    type,
    TOTAL) |> 
  distinct() |> 
  tidyr::crossing(GROUP = GROUPS) 

data_rc_cover <-
  data_rc_cover |> 
  full_join(filler) |>
  group_by(
    across(c(starts_with("site"),
             survey_start_date,
             survey_depth,
             transect_name,
             transect_id,
             image_id,
             type,
             GROUP
    ))) |> 
  mutate(COUNT = ifelse(is.na(COUNT), 0, COUNT),
         TOTAL = max(TOTAL, na.rm = TRUE)
  )

data_rc_cover |> glimpse()
data_rc_cover$site_name |> unique() 

data_rc_cover <- data_rc_cover |>
  filter(type == "point_machine_classification") |>
  mutate(site_management = case_when(
    site_name %in% c("DENR_Snake_S3", "PAMO_Helicopter","WWF_Ar_Cambari", "WWF_Ar_Catad") ~ "Protected",
    site_name %in% c("WPU_PB_Aquarium","WPU_PB_Fantastic", "WWF_Ar_Langoy", "WPU_PB_Manta") ~ "Tourist_Site",
  ))

data_rc_cover$site_management <- factor(data_rc_cover$site_management, levels = c("Protected","Tourist_Site"))
levels(data_rc_cover$site_management)

data_rc_cover_site <- data_rc_cover |>
  group_by(
  across(c(starts_with("site"),
           survey_start_date,
           survey_depth,
           transect_name,
           transect_id,
           image_id,
           type,
           GROUP
  ))) |> 
  summarise(COUNT = sum(COUNT),
            TOTAL = sum(TOTAL)
  ) |> 
  ungroup() 
  

##boxplot
plotRC1 <- data_rc_cover |> 
  filter(GROUP == "HC") |> ggplot() +
  geom_boxplot(aes(x = site_name, y = COUNT/TOTAL, fill = site_management)) +
  ggtitle("Hard Coral Cover") +
  theme_bw(base_size = 16) +
  theme(axis.text.x = element_text(angle=30, hjust = 1))

plotRC1


formRC1 <- bf(COUNT | trials(TOTAL) ~ site_management + (1 | site_name),
              family = beta_binomial(link = "logit"))

get_prior(formRC1, data=data_rc_cover)

data_rc_cover %>% 
  mutate(cover = COUNT/TOTAL) %>% 
  group_by(site_management) %>% 
  summarise(across(cover, list(mean, sd, median, sd)))

qlogis(0.148)

priors <- prior(normal(-1.75, 1), class = "Intercept") +
  prior(normal(0,1), class = "b") +
  prior(student_t(3,0,1), class = "sd")

model_RC <- brm(formRC1, 
                   data = data_rc_cover,
                   prior = priors,
                   chains = 3, cores = 3,
                   iter = 3000,
                   warmup = 1000,
                   thin = 10,
                   sample_prior = "yes",
                   control=list(adapt_delta=0.99),
                   backend = "rstan")

model_RC |> conditional_effects() |> plot() 

model_RC <- model_RC |> 
  update(sample_prior = "yes")

model_RC |> conditional_effects() |> plot()

model_RC |> summary()

model_RC$fit |> stan_trace()

model_RC$fit |> stan_ac()

model_RC$fit |> stan_rhat()

model_RC$fit |> stan_ess()

model_RC |> pp_check(type = "dens_overlay", ndraws = 100)

resids <- model_RC |> make_brms_dharma_res(integerResponse = FALSE)

testUniformity(resids)

plotResiduals(resids, form = factor(rep(1, nrow(Q6.All_Data2))))

plotResiduals(resids)

testDispersion(resids)


model_RC |> emmeans(~ site_management, type = "response")

model_RC  |> emmeans(~ site_management, type = "response") |>
  pairs()

model_RC  |> 
  emmeans(~site_management) |> 
  regrid() |> 
  pairs() |> 
  gather_emmeans_draws() |> 
  ggplot(aes(x = .value, y = contrast)) +
  stat_halfeye(aes(fill = after_stat(level)), .width = c(0.66, 0.95, 1)) +
  scale_fill_brewer() +
  geom_vline(xintercept = 0, linetype = "dashed")


