#' Get Aglink time series from the _Ori.gdx file
#' 
#' @param datacube A dplyr table with the raw capmod results
#' @param region_list A list of regions
#' @param attrib1_list A character vector of first attributes (usually balance items)
#' @param attrib2_list A character vector of second attributes (usually commodities)
#' 
#' @return A dply table containing time series (all available years)
#' 
#' 
#' 
get_time_serie <- function(datacube, region_list, attrib1_list, attrib2_list){
  re_table <- datacube %>% filter(region %in% region_list, attrib1 %in% attrib1_list, attrib2 %in% attrib2_list)
  
  return(re_table)
}

#' Aggregate Aglink results from E15 and NMS to EUN
#' Reason: sometimes the totals are missing.
#' 
#' @param datacube Aglink dataset as prepared by data-raw/aglink_timeseries.r
#' @param attrib Aglink attributes (e.g. DEL: deliveries)
#' @param product Aglink product code (e.g. MK: milk)
#' 
aggregate_eun <- function(datacube, attrib, product){
  
# remove EUN if available in the original datacube (re-calculation)
  datacube <- datacube %>% filter(!(region == "EUN" & attrib1 == attrib & attrib2 == product))
  
# calculate EUN aggregates in separate table  
  re_table <- datacube %>% filter(region %in% c("E15","NMS"), attrib1 == attrib, attrib2 == product)
  
if(nrow(re_table) > 0){  
  re_table <- re_table %>% group_by(attrib1, attrib2, year) %>% summarise( EUN = sum(value) )
  re_table $ region <- "EUN"
  re_table <- re_table %>% select(region, attrib1, attrib2, year, EUN)
  colnames(re_table)[5]  <- "value"
  
# attach calculated table to the original one
  datacube <- rbind(datacube, re_table)
}

  return(datacube)
}

#' Aggregate Aglink results from E15 and NMS to EUN
#' This function expects lists of attributes and products
#' Reason: sometimes the totals are missing.
#' 
#' @param datacube Aglink dataset as prepared by data-raw/aglink_timeseries.r
#' @param attrib_list Aglink attributes (e.g. DEL: deliveries)
#' @param product_list Aglink product codes (e.g. MK: milk)
#' 
aggregate_eun_list <- function(datacube, attrib_list, product_list){
  re_table <- datacube
  for(attrib in attrib_list){
    for(product in product_list){
      re_table <- aggregate_eun(re_table, attrib, product)  
    }
  }
  return(re_table)
}