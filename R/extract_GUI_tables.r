

#' Extract GUI tables from the results data cube
#'
#' @param datacube A dplyr table with the raw capmod results
#' @param region_list A character list of regions
#' @param product_list List of commodities
#' @param scenario Scenario for which you want to retrieve results
#'
#' @return A dply table containing the market balance
#'
filter_results_cube <- function(datacube, region_list, dim5_list, cols_list, rows_list, scenario_name){


  datacube <- datacube %>%
    filter(i1 %in% region_list, i2 %in% dim5_list, i3 %in% cols_list, i4 %in% rows_list, i5 != "BAS") %>%
    select(i1, i2, i3, i4, i5, value)

  datacube$scenario <- scenario_name
  return(datacube)

}

extract_gui_table <- function(region_list, dim5_list, cols_list, rows_list, scenario_list, folder = "mydata"){

# load all scenario results into memory
  for(i in 1:length(scenario_list)) {load(file=paste(folder, "/", scenario_list[i], ".RData", sep=""))}

# get selected results (defined by the region, dim5, cols and rows lists) for the first scenario
  GUI_table <- filter_results_cube(get(scenario_list[1]),  region_list, dim5_list, cols_list, rows_list, scenario_list[1])

# continue with the other scenarios
  if (length(scenario_list) > 1) {
    for(i in 2:length(scenario_list)){



     GUI_table <- rbind(GUI_table, filter_results_cube(get(scenario_list[i]),  region_list, dim5_list, cols_list, rows_list, scenario_list[i]))


    }
  }


  return(GUI_table)
}

get_GUI_table <- function(table = "supply details", scenario_list, folder = "mydata"){



# extract 'supply tables'
 if(table == "supply details"){


 selection_regions <- regions %>% filter(grepl("Regions and C", sel, ignore.case = TRUE)) %>% select(key)
 selection_dim5s   <- tbl_df(data.frame(key = c("")))
 selection_cols    <- activities %>% select(key)



 selection_regions <- selection_regions$key
 selection_dim5s   <- selection_dim5s$key
 selection_cols    <- selection_cols$key


# load("data/table_coco.RData")

 # initialize code list
 code_list <- tbl_df(data.frame())

 # add the columns without numbering
  x           <- coco %>% filter(name == "Supply details") %>% select(item.key,
                                                                      item.itemName,
                                                                      item.unit)
  colnames(x) <- c("key","name","unit")
  code_list   <- rbind(code_list, x)

 for(i in 1:17){
  x           <- coco %>% filter(name == "Supply details") %>% select(get(paste("item.key.", i, sep = "")),
                                                                      get(paste("item.itemName.", i, sep = "")),
                                                                      get(paste("item.unit.", i, sep = "")))
  colnames(x) <- c("key","name","unit")
  code_list   <- rbind(code_list, x)

 }




# remove NA's and duplicates
 x <- code_list %>% filter(!is.na(key)) %>% filter(!duplicated(key))

 selection_rows <- x$key
 name_rows      <- x$name
 unit_rows      <- x$unit


 supply_details <- extract_gui_table(region_list = selection_regions, dim5_list = selection_dim5s, cols_list = selection_cols,
                  rows_list = selection_rows,
                  scenario_list = scenario_list, folder = "mydata")

 supply_details <- left_join(supply_details, x, c("i4" = "key"))

 supply_details <- supply_details %>% select(i1, i3, i4, name, unit, i5, scenario, value)

 products <- products %>% select(key, itemName)
 supply_details <- left_join(supply_details, products, c("i3" = "key"))
 supply_details <- supply_details %>% select(i1, i3,itemName, i4, name, unit, i5, scenario, value)

 colnames(supply_details) <- c("region", "activity_code", "activity_name", "variable_code", "variable_name", "unit", "year", "scenario", "value")
 rm(x)

 # calculate additional columns

 # 1 a. crop shares UAAR.LEVL:
 levels <- supply_details %>% group_by(region, activity_code) %>% filter(variable_code == "LEVL")
 uaars  <- supply_details %>% filter(variable_code == "LEVL") %>% filter(activity_code == "UAAR")
 uaars  <- uaars %>% select(region, activity_code, year, scenario, value)


 x <- left_join(levels, uaars, c("region","year","scenario"))
 rm(levels, uaars)

 colnames(x) <- c("region","activity_code","activity_name","variable_code","variable_name","unit","year","scenario","valuex","todelete","valuey")
 x <- x %>% mutate(value = valuex/valuey)
 x <- x %>% select(-valuex, -valuey, -todelete)

 x$variable_code <- gsub('LEVL', 'UAAR', x$variable_code, ignore.case = TRUE)
 x$variable_name <- rep("Crop Share", length(x$variable_name))
 x$unit <- rep("% or 0.01 animals/ha", length(x$unit))
 x <- x %>% filter(activity_code != "UAAR")

 supply_details <- rbind(supply_details, x)
 rm(x)

# rename UAAR to CropShare to avoid misunderstandings
 setDT(supply_details)[variable_name == "Crop Share", variable_code := "CropShare"]
 supply_details <- tbl_df(supply_details)


  # 1 b. crop shares per arable land ARAB.LEVL:
 levels <- supply_details %>% group_by(region, activity_code) %>% filter(variable_code == "LEVL")
 arabs  <- supply_details %>% filter(variable_code == "LEVL") %>% filter(activity_code == "ARAB")
 arabs  <- arabs          %>% select(region, activity_code, year, scenario, value)


 x <- left_join(levels, arabs, c("region","year","scenario"))
 rm(levels, arabs)

 colnames(x) <- c("region","activity_code","activity_name","variable_code","variable_name","unit","year","scenario","valuex","todelete","valuey")
 x <- x %>% mutate(value = valuex/valuey)
 x <- x %>% select(-valuex, -valuey, -todelete)

 x$variable_code <- gsub('LEVL', 'ARAB', x$variable_code, ignore.case = TRUE)
 x$variable_name <- rep("Crop Share per Arable land", length(x$variable_name))
 x$unit <- rep("% or 0.01 animals/ha", length(x$unit))
 x <- x %>% filter(activity_code != "ARAB")

 supply_details <- rbind(supply_details, x)
 rm(x)

# rename ARAB to CropShareArab to avoid misunderstandings
 setDT(supply_details)[variable_name == "Crop Share per Arable land", variable_code := "CropShareArab"]
 supply_details <- tbl_df(supply_details)





 # 2. Supply = yield * levl
 levels <- supply_details %>% group_by(region, activity_code) %>% filter(variable_code == "LEVL")
 yields <- supply_details %>% group_by(region, activity_code) %>% filter(variable_code == "YILD")

 x <- inner_join(levels, yields, c("region","year","scenario","activity_code","activity_name"))
 x  <- x %>% mutate(value = value.x * value.y)
 x <- x %>% select(-value.x, -value.y, -variable_name.x, -variable_name.y, -unit.x, -unit.y)
 x <- x %>% select(-variable_code.x, -variable_code.y)

 x$variable_name <- rep("Supply", length(x$value))
 x$variable_code <- rep("Supply", length(x$value))
 x$unit          <- rep("1000 t", length(x$value))

 supply_details <- rbind(supply_details, x)
 rm(x)

 return(supply_details)
 }

}







# calculated columns



