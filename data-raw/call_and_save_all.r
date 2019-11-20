source("data-raw/xmlconversion.r")
source("data-raw/nomenclatures.r")
source("data-raw/eu_region.r")
source("data-raw/nuts2_map.r")
source("data-raw/nuts2_polygons.r")

devtools::use_data(coco, activities, dim5s, products, regions, ms_list, activity_list, eu_region, nuts2_map, nuts2_polygons,
                   internal = TRUE,
                   overwrite = TRUE)

