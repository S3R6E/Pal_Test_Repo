## coral cover eda
load(file = "../data/processed/q1_data_rc_cover.RData")

data_rc_cover |> as.data.frame() |> glimpse()


plot1 <-
data_rc_cover |>
  filter(type=="point_machine_classification") |>
  ggplot(aes(y = cover, 
             x = date, 
             colour=site_reef_name)) +
  geom_line()+
  geom_point(shape = 17, size=3) +
  scale_y_continuous("Hard coral cover (%)", labels=function(x) x*100) +
  scale_x_date("Year") +
  scale_colour_discrete("Reef name") +
  theme_classic(8) +
  theme(panel.grid.major = element_line(size=0.5))
plot1

ggsave(filename = "../outputs/figures/q1_figure1.png",
       dpi = 300, width=150, height=100, units="mm")
ggsave(filename = "../outputs/figures/q1_figure1.pdf",
       width=150, height=100, units="mm")


plot2 <-
  data_rc_cover |>
  filter(type=="point_human_classification") |>
  ggplot(aes(y = cover, 
             x = date, 
             colour=site_reef_name)) +
  geom_line()+
  geom_point(shape = 17, size=3) +
  scale_y_continuous("Hard coral cover (%)", labels=function(x) x*100) +
  scale_x_date("Year") +
  scale_colour_discrete("Reef Name Human Classified") +
  theme_classic(8) +
  theme(panel.grid.major = element_line(size=0.5))
plot2

ggsave(filename = "../outputs/figures/q1_figure2.png",
       dpi = 300, width=150, height=100, units="mm")
ggsave(filename = "../outputs/figures/q1_figure2.pdf",
       width=150, height=100, units="mm")

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

