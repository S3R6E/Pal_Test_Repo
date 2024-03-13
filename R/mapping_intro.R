library(tidyverse)        ## for data wrangling
library(sf)               ## for spatial features in R
library(rnaturalearth)    ## for interacting with natural earth
library(ggspatial)        ## map scalebars and arrows
library(patchwork)        ## combining multiple points
library(remotes)

sf::sf_use_s2 (FALSE) ## FALSE is turning off s2. 
## s2 is for more spatial accuracy, but it slows down the process. you can turn it off in most cases

remotes::install_github("ropensci/rnaturalearthhires")



# Step 1 : Dowload the map of the Philippines and display it
coast_php <- rnaturalearth::ne_countries (
    scale = 10, ## 10 is the highest resolution, needed to map a signle country
    type = "countries", 
    country = "Philippines", 
    returnclass = "sf" ## sf is the format of the output
)

ggplot() + 
    geom_sf(data = coast_php) ## note that the map does not get distorted when it's exported as image. the map stays true to the right dimensions. 



# Step 2 : Zoom on Palawan. 
    
  ##Make a bounding box for Palawan. 

bbox <- sf::st_bbox (c( 
  xmin = 116, ## the unit of x and y is degrees
  xmax = 121,
  ymin = 6, 
  ymax = 13),
  crs=sf::st_crs(coast_php)
  )

  ## Read in the Tropical Coral Reefs of the World shapefile
reef <- sf::read_sf("../data/GIS/reef_500_poly.shp")

reef_pal <- reef %>% 




