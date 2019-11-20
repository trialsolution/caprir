library(tidykml)
library(tidyverse)
library(ggmap)

nuts2_map <- get_map(kml_bounds("map/capri_map.kml"))

save(nuts2_map, file = "data/nuts2_map.RData")