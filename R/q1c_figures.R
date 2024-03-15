## Produced figures for q1c: HHC

rm(list=ls())
gc()

## Load libraries
## ---- loadlibraries
library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggspatial)
library(patchwork)
library(gridExtra)
library(easystats)
library(knitr)
library(brms)
library(rstan)
library(tidybayes)
library(patchwork)
library(DHARMa)
library(HDInterval)
library(emmeans)
library(patchwork)
sf::sf_use_s2(FALSE)
## ---- end

## ---- loaddata
# Load dataset and variables
load("../data/modelled/q1c_model.RData") ## load model
load("../data/processed/q1c_data.RData") ## load data
## ---- end

## Figure 1: Map of Sampling Locations ####
## ----- figure1
# Base map

coast_php <- rnaturalearth::ne_countries(
  scale = 10, type = "countries",
  country = "Philippines",
  returnclass = "sf"
)

## bbox <- sf::st_bbox(coast_php)

## Lets make a bounding box for Palawan
bbox <- sf::st_bbox(c(
  xmin = 116,
  xmax = 121,
  ymin = 7.8,
  ymax = 12.6
), crs = sf::st_crs(coast_php))

## Read in the Tropical Coral Reefs of the World shapefile
reef <- sf::read_sf("../data/GIS/reef_500_poly.shp")

reef_pal <- reef |>
  sf::st_transform(crs = sf::st_crs(coast_php)) |>
  sf::st_make_valid() |>
  sf::st_crop(bbox)

coast_pal <- coast_php |>
  sf::st_crop(bbox)

## Make a map of Palawan
bbox.pal=sf::st_bbox(c(
  xmin = 118.2,
  xmax = 120.5,
  ymin = 9.3,
  ymax = 11.3
), crs = sf::st_crs(coast_php))


pal_map <- ggplot() +
  geom_sf(data = coast_pal, fill = "lightgrey", colour="black") +
  geom_sf(data = reef_pal, colour = "darkgrey") +
  geom_sf(dat = sf::st_as_sfc(bbox.pal), fill = "#c6c6c640") +
  theme_bw() +
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank()
  )


# Zoom in on a specific area
# survey map
sur_map <- pal_map +
  ggspatial::annotation_scale(location = "br",
                              width_hint =  0.4,
                              bar_cols = c("grey20", "white"))+
  annotation_north_arrow(location = "br",
                         which_north = "true", 
                         pad_x = unit(0.2, "npc"),
                         pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(118.2, 120.5),
           ylim = c(9.3,11.3), expand = FALSE)


# Create sites

sites.sf<- df |> 
  select(Side, site_name, site_latitude, site_longitude) |> 
  unique() |> 
  st_as_sf(coords = c(x="site_longitude", y="site_latitude"), crs=st_crs(pal_map))

sites<- df |> 
  select(site_name, site_latitude, site_longitude, Side) |> 
  unique() 

## create final figure1
  
  
  sur_map <- sur_map +
  geom_point(data = sites, aes(y = site_latitude, x = site_longitude,
                                       colour = "Survey Sites"), shape = 16, size =  2) +
  scale_colour_manual("", values = c("blue"))
  
  f1 <- sur_map +
    inset_element(pal_map, left = 0.01, bottom = 0.6, right = 0.4, top = 0.99)
## ---- end 

ggsave(plot=f1, filename= "../outputs/figures/q1c_f1_surveyMap.png")


## Figure 2: HCC East vs West ####

## ----- figure2
newdf<- expand_grid(f.Side=c("East", "West", NA), f.year=NA,f.site_name=NA, total=1)

side.draws <- posterior_epred(model1, newdata = newdf)

pred.df<-side.draws |> 
  as.data.frame() |>  
  pivot_longer(cols=everything(),names_to = "Sides") |> 
  group_by(Sides) |> 
  median_hdci() |> 
  cbind(newdf) |> 
  select(f.Side,value,.lower,.upper,.width,.point,.interval) |> 
  rename(Side=f.Side, HCC.median=value, HCC.min=.lower,HCC.max=.upper) |> 
  mutate(Side=ifelse(is.na(Side), "Average", Side)) |> 
  mutate(HCC.median=HCC.median*100, HCC.min=HCC.min*100, HCC.max=HCC.max*100)

# Plot2
f2 <- pred.df |> 
  ggplot(aes(x = Side, y = HCC.median)) +
  geom_point(position = position_dodge(width=0.9), size=3) +
  geom_errorbar(aes(ymin=HCC.min, ymax=HCC.max), width=0.2, linewidth=0.1) +
  labs(x="Palawan Coast", y="Hard Coral Cover (%)")+
  theme_bw() +
  theme(panel.grid=element_blank())
## ---- end


ggsave(plot=f2, filename= "../outputs/figures/q1c_f2_east_west.png")
## ---- figure3
## Figure 3: Comparison over the years for Araceli ####
a.sites<-c("WWF_Ar_Cambari",
           "WWF_Ar_Catad",
           "WWF_Ar_Langoy")
df |> 
  select(site_name, date) |> 
  filter(site_name %in% a.sites)

#Years: Before:2021 - After: 2022

a.sites<-c("WWF_Ar_Cambari",
           "WWF_Ar_Catad",
           "WWF_Ar_Langoy")

newdf<- expand_grid(f.Side=c("East"), f.year=factor(c("2021", "2022")),f.site_name=factor(a.sites), total=1)

year.draws <- posterior_epred(model1, newdata = newdf)

pred.df<-year.draws |> 
  as.data.frame() |>  
  pivot_longer(cols=everything(),names_to = "Var") |> 
  group_by(Var) |> 
  median_hdci() |> 
  cbind(newdf) |> 
  select(f.year, f.site_name, value,.lower,.upper,.width,.point,.interval) |> 
  rename(site_name=f.site_name, year=f.year, HCC.median=value, HCC.min=.lower,HCC.max=.upper) |> 
  mutate(HCC.median=HCC.median*100, HCC.min=HCC.min*100, HCC.max=HCC.max*100)

# Plot3
f3 <- pred.df |> 
  mutate(tc_odette=ifelse(year=="2021", "Before", "After"), tc_odette=as.factor(tc_odette), 
         tc_odette=relevel(tc_odette, 2)) |> 
  ggplot(aes(x = tc_odette, y = HCC.median)) +
  geom_point(aes(color=site_name), position = position_dodge(width=0.9), size=3) +
  geom_errorbar(aes(ymin=HCC.min, ymax=HCC.max, group=site_name), width=0.2, linewidth=0.1, 
                position=position_dodge(width=0.9)) +
  labs(x="Typhoon Odette", y="Hard Coral Cover (%)")+
  theme_bw() +
  theme(panel.grid=element_blank())
## ---- end

ggsave(plot=f3, filename= "../outputs/figures/q1c_f3_odette.png")


# Figure 4: Map HCC West & East ####
## ----- figure4
# Zoom in on a specific area
# Zoom in to the West
bbox.west<-st_bbox(sites.sf |>  filter(Side=="West") |> st_buffer(dist = 0.1))

sur_map.w <- pal_map +
  ggspatial::annotation_scale(location = "br",
                              width_hint =  0.4,
                              bar_cols = c("grey20", "white"))+
  annotation_north_arrow(location = "br",
                         which_north = "true", 
                         pad_x = unit(0.2, "npc"),
                         pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(bbox.west[1]-0.2, bbox.west[3]+0.2),
           ylim = c(bbox.west[2], bbox.west[4]), expand = FALSE)


# Zoom in to the East South
bbox.east<-st_bbox(sites.sf |>  filter(Side=="East") |> st_buffer(dist = 0.1))

sur_map.e.s <- pal_map +
  ggspatial::annotation_scale(location = "br",
                              width_hint =  0.4,
                              bar_cols = c("grey20", "white"))+
  annotation_north_arrow(location = "br",
                         which_north = "true", 
                         pad_x = unit(0.2, "npc"),
                         pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(bbox.east[1], 119),
           ylim = c(bbox.east[2], 10), expand = FALSE)



# Zoom in to the East North
bbox.east<-st_bbox(sites.sf |>  filter(Side=="East") |> st_buffer(dist = 0.1))

sur_map.e.n <- pal_map +
  ggspatial::annotation_scale(location = "br",
                              width_hint =  0.4,
                              bar_cols = c("grey20", "white"))+
  annotation_north_arrow(location = "br",
                         which_north = "true", 
                         pad_x = unit(0.2, "npc"),
                         pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(119.5, 120.2),
           ylim = c(10.3, 10.8), expand = FALSE)

# Predictions
newdf<- df |> 
  mutate(year=year(date),f.Side=as.factor(Side), 
         f.site_name=as.factor(site_name), f.year=as.factor(year) ) |> 
  select(f.Side,f.year,f.site_name) |> 
  distinct() |> 
  mutate(total=1, Var=paste0("V", seq(1:16)),
         inc=case_when(
           (f.site_name %in% a.sites) & (f.year=="2021") ~ FALSE,
           .default=TRUE
         ))
  
map.draws <- posterior_epred(model1, newdata = newdf)

pred.df<-map.draws |> 
  as.data.frame() |>  
  pivot_longer(cols=everything(),names_to = "Var") |> 
  left_join(newdf) |> 
  filter(inc==TRUE) |> 
  group_by(f.Side,f.site_name) |> 
  summarise(median_hdci(value) ) |> 
  rename(Side=f.Side,site_name=f.site_name, HCC.median=y, HCC.min=ymin,HCC.max=ymax) |> 
  mutate(HCC.median=HCC.median*100, HCC.min=HCC.min*100, HCC.max=HCC.max*100) |> 
  left_join(sites)

# Map 1: HCC West
sur_map.w<- sur_map.w +
  geom_point(data = pred.df |> 
               filter(Side=="West"), 
             aes(y = site_latitude, x = site_longitude, size=HCC.median), 
             shape = 16, color="blue", alpha=0.35) +
  scale_size_continuous(range=c(1,8)) +
  geom_text(data = pred.df |> 
              filter(Side=="West"), 
            aes(y = site_latitude, x = site_longitude, label=site_name), size=2)+
  ggtitle("A. Palawan West")+
  theme(axis.text = element_text(size=5),
        axis.title=element_blank(),
        legend.position = "none")


# Map 2: HCC East South


sur_map.e.s<-sur_map.e.s +
  geom_point(data = pred.df |> 
               filter(Side=="East"), 
             aes(y = site_latitude, x = site_longitude, size=HCC.median), 
             shape = 16, color="blue", alpha=0.35) +
  scale_size_continuous(range=c(1,8)) +
  geom_text(data = pred.df |> 
              filter(Side=="East"), 
            aes(y = site_latitude, x = site_longitude, label=site_name), size=2)+
  ggtitle("B. Palawan South-East")+
  theme(axis.text = element_text(size=5),
        axis.title=element_blank(),
        legend.position = "none")

# Map 2: HCC East North

sur_map.e.n<-sur_map.e.n +
  geom_point(data = pred.df |> 
               filter(Side=="East"), 
             aes(y = site_latitude, x = site_longitude, size=HCC.median), 
             shape = 16, color="blue", alpha=0.35) +
  scale_size_continuous(range=c(1,8)) +
  geom_text(data = pred.df |> 
              filter(Side=="East"), 
            aes(y = site_latitude, x = site_longitude, label=site_name), size=2)+
  ggtitle("C. Palawan North-East")+
  theme(axis.text = element_text(size=5),
        axis.title=element_blank())

f4<-(sur_map.w+sur_map.e.s)/sur_map.e.n
## ----- end 


sur_map.e.n
ggsave(sur_map.w, filename="../outputs/figures/q1c_MapHCC_West.png")
ggsave(sur_map.e.n, filename="../outputs/figures/q1c_MapHCC_East_North.png")
ggsave(sur_map.e.s, filename="../outputs/figures/q1c_MapHCC_East_South.png")


## ----- figure5
f5<-pred.df |> 
  ggplot(aes(x=site_name, y=HCC.median))+
  geom_point(position = position_dodge(width=0.9), size=3) +
  geom_errorbar(aes(ymin=HCC.min, ymax=HCC.max), width=0.2, linewidth=0.1) +
  labs(x="Sites", y="Hard Coral Cover (%)")+
  facet_wrap("Side", scales = "free_x")+
  theme_bw() +
  theme(panel.grid=element_blank(),
        axis.text.x=element_text(angle = 45, hjust = 1))
## ---- end