## coral cover eda

data_rc_cover |> as.data.frame() |> glimpse()


data_rc_cover |>
  filter(type=="point_machine_classification") |>
  ggplot(aes(y = cover, x = date, colour=site_reef_name)) +
  geom_line()+
  geom_point()

data_rc_cover |>
  filter(type=="point_machine_classification") |>
  ggplot(aes(y = cover, x = date, colour=site_reef_type, group=site_reef_zone)) +
  geom_line()+
  geom_point()

data_rc_cover |>
  filter(type=="point_machine_classification") |>
  ggplot(aes(y = cover, x = date, colour=Side, group=site_reef_zone)) +
  geom_line()+
  geom_point()

data_rc_cover |>
  filter(type=="point_machine_classification") |>
  ggplot(aes(y = cover, x = Side)) +
  geom_point()

write_csv(data_rc_cover, file="../data/processed/coral_cover_rc.csv")

data_rc_cover %>%
  ggplot(aes (y = cover, x = survey_start_date)) +
  geom_point()

data_rc_cover %>% 
  filter(type=="point_machine_classification") %>% 
  ggplot(aes(y = cover, x = date, colours=site_reef_type, group=site_reef_zone)) +
  geom_line()+
  geom_point()

