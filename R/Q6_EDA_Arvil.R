#exploratory data analysis
load(file = "../data/processed/Q6_all_data.Rdata")
plot1 <-all_data|>
  group_by(`Tourist Access`)|>
  summarise(Mean = mean (cover),
            SD = sd(cover))|>
  mutate(lower = Mean -SD,
         upper = Mean + SD)|>
  ungroup() |>
  ggplot(aes(y = Mean, x = `Tourist Access`, colour= `Tourist Access`)) +
  geom_pointrange(aes(ymin=lower, ymax=upper)) +
  scale_y_continuous("Hard coral cover (%)", labels = function(x) x*100) + 
  theme_classic(10)
plot1
ggsave(file="..//outputs/figures/Tourist_Access_Plot1.png",
       width = 700, height = 500, units = "px",
       dpi = 300)
ggsave(file="..//outputs/figures/Tourist_Access_Plot1.pdf",
       width = 7, height = 5, units = "in")

