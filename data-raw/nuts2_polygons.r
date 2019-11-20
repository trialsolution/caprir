library(tidykml)
library(tidyverse)

nuts2_polygons <- kml_polygons("map/capri_map.kml")

save(nuts2_polygons, file = "data/nuts2_polygons.RData")