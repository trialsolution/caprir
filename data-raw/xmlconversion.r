
  xml_folder <-  "inst/extdata/xmldata"

  library(XML)
  library(plyr)
  library(dplyr)


# Coco tables
coco <- ldply(xmlToList(paste(xml_folder, "/coco_tables.xml", sep = "")), data.frame)
coco <- tbl_df(coco)
coco$regionSel <- gsub('Regions and Countries', 'Regions and C', coco$regionSel, ignore.case = TRUE)


# Extract codes from dimdef (regions, activities, products)
dimdef      <- ldply(xmlToList(paste(xml_folder, "/dimdefs.xml", sep = "")), data.frame)
dimdef$.id  <- factor(dimdef$.id)
dimdef      <- tbl_df(dimdef)

# how to use grepl() to find matching products/activities to a group?
#dimdef %>% filter(grepl("Economic Account for Agriculture", sel)) %>% View()

# replace 'Regions and Countries' to avoid confusion with 'Countries'
dimdef$sel <- gsub('Regions and Countries', 'Regions and C', dimdef$sel, ignore.case = TRUE)

# extract activities/prods...
activities      <- dimdef %>% filter(.id == "activity")
dim5s           <- dimdef %>% filter(.id == "dim5")
products        <- dimdef %>% filter(.id == "product")
regions         <- dimdef %>% filter(.id == "region")
selgroups       <- dimdef %>% filter(.id == "selgroup")


#  make the dimensions available to package users => package useres can use data()
#  Note that in call_and_save_all.r all of the above is saved to R/sysdata.rda, i.e. only available for the package functions

save(activities, file = "data/xml_activities.RData")
save(dim5s,      file = "data/xml_dim5s.RData")
save(products,   file = "data/xml_products.RData")
save(regions,    file = "data/xml_regions.RData")
save(coco, file = "data/table_coco.RData")

# these wider groups need to be filtered further down
#countries  <- regions %>% filter(grepl("Countries", sel) & (!grepl("Regions and Countries", sel)))



