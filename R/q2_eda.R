setwd("R")

load(file = "../data/process/q2_datarich.RData")

q2_richplot2 <-
  datarich |>  ggplot() +
  geom_boxplot(mapping = aes(x = Transect, y = Richness, fill = Transect), show.legend = FALSE) +
  scale_fill_manual(breaks = c("Transect 1", "Transect 2", "Transect 3", "Transect 4"),
                    values = c("red", "orange", "purple", "#c6c6c6")) +
  scale_y_continuous("Species Richness") +
  theme_classic()
q2_richplot2

ggsave(q2_richplot2, 
       file = "../outputs/figures/q2_richplot2.png",
       units = "cm", 
       width = 12, 
       height = 8,
       dpi = 300)



