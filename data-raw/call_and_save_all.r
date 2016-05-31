source("data-raw/xmlconversion.r")
source("data-raw/nomenclatures.r")
source("data-raw/eu_region.r")

devtools::use_data(coco, activities, dim5s, products, regions, ms_list, activity_list, eu_region, internal = TRUE,
                   overwrite = TRUE)

