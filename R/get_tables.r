#' Get market balances without intra trade from CAPMOD results
#' 
#' @param datacube A dplyr table with the raw capmod results
#' @param region_list A character list of regions
#' @param product_list List of commodities
#' @param scenario Scenario for which you want to retrieve results
#' 
#' @return A dply table containing the market balance 
#' 
get_market_balance <- function(datacube, region_list, product_list){
  
  bal_item    <- c("PROD", "HCON", "PROC", "BIOF", "FEED", "ISCH", "Imports_noIntra", "Exports_noIntra")


  datacube <- datacube %>% 
    filter(i1 %in% region_list, i2 == "", i3 %in% bal_item, i4 %in% product_list, i5 != "BAS") %>% 
    select(i1, i3, i4, i5, value)
  
  return(datacube)
  
}

#' Get product balances from CAPMOD results
#' 
#' 
get_product_balance <- function(datacube, region_list, product_list){
  
  bal_item    <- c("GROF", "LOSF", "SEDF", "INTF", "MAPR", "IMPT", "INTP", "EXPT", "NTRD", "DOMM", "FEDM", "SEDM", "PRCM", "BIOF", "INDM", "LOSM", "STCM")
  
  datacube <- datacube %>% 
    filter(i1 %in% region_list, i2 == "", i3 %in% bal_item, i4 %in% product_list, i5 != "BAS") %>% 
    select(i1, i3, i4, i5, value)
  
  return(datacube)
  
}


#' Get the table Farm|Supply details
#' 
#' 
get_supply_detail <- function(datacube, region_list, product_list){
  
  bal_item    <- c("MGVA", "YILD", "LEVL")
  
  # note that supply must be calculated = YILD * LEVL
  
  
  datacube <- datacube %>% 
    filter(i1 %in% region_list, i2 == "", i3 %in% product_list, i4 %in% bal_item, i5 != "BAS") %>% 
    select(i1, i3, i4, i5, value)
  
  return(datacube)
  
}



#' Convert market balances into a pre-defined format (for reporting purposes)
#' 
#' 1: calcualate nettrade
#' 2: append years (2010, 2013, 2020, 2025, 2030)
#' 
#' @param region_list A character list of regions
#' @param product_list List of commodities
#' @param year_list List of years
#' 

convert_balance_ntrd <- function(region_list, product_list, scenario_list, folder = "mydata"){
  
  for(i in 1:length(scenario_list)) {load(file=paste(folder, "/", scenario_list[i], ".RData", sep=""))}
  
  
  balance <- get_market_balance(get(scenario_list[1]), region_list, product_list)
  
  if (length(scenario_list) > 1) {
    for(i in 2:length(scenario_list)){
  
   
      balance <- rbind(balance, get_market_balance(get(scenario_list[i]), region_list, product_list))
     
      
    }
  }
  
  # calculate net trade (exports - imports)
  balance <- spread(balance, i3, value)
  
  # replace NAs with zeros
  balance[is.na(balance)] <- 0

  # add columns if missing (missing data)
  if(!("PROD" %in% colnames(balance))) { balance <- mutate(balance, PROD = 0)} 
  if(!("HCON" %in% colnames(balance))) { balance <- mutate(balance, HCON = 0)} 
  if(!("PROC" %in% colnames(balance))) { balance <- mutate(balance, PROC = 0)} 
  if(!("BIOF" %in% colnames(balance))) { balance <- mutate(balance, BIOF = 0)} 
  if(!("FEED" %in% colnames(balance))) { balance <- mutate(balance, FEED = 0)} 
  if(!("ISCH" %in% colnames(balance))) { balance <- mutate(balance, ISCH = 0)} 
  if(!("Imports_noIntra" %in% colnames(balance))) { balance <- mutate(balance, Imports_noIntra = 0)} 
  if(!("Exports_noIntra" %in% colnames(balance))) { balance <- mutate(balance, Exports_noIntra = 0)} 
  

# calculate net trade
  balance <- balance %>% mutate(nettrade = Exports_noIntra - Imports_noIntra) 

# calculate aggregate demand/supply (product specific demand positions)
  meat_item   <- c("PROD", "HCON", "ISCH", "Imports_noIntra", "Exports_noIntra")
  dairy_item  <- c("PROD", "HCON", "PROC", "BIOF", "FEED", "ISCH", "Imports_noIntra", "Exports_noIntra")
     
  meat_product       <- c("PORK", "POUM", "BEEF", "SGMT")
  dairy_product      <- c("MILK", "BUTT", "CREM", "FRMI", "CHES", "SMIP", "COCM", "WMIO", "CASE", "WHEP")
  cereal_product     <- c("CERE", "RYEM", "WHEA", "OATS", "BARL", "OCER", "MAIZ", "RICE") 
  oilseeds_product   <- c("RAPE", "SOYA", "SUNF")
  cakes_product      <- c("RAPC", "SUNC", "SOYC", "CAKS")
  oils_product       <- c("RAPO", "SUNO", "SOYO", "OLIO", "PLMO")
  
  
  meats   <- balance %>% filter(i4 %in% meat_product)
  dairy   <- balance %>% filter(i4 %in% dairy_product)
  cereals <- balance %>% filter(i4 %in% cereal_product)
  oilseeds<- balance %>% filter(i4 %in% oilseeds_product)
  cakes   <- balance %>% filter(i4 %in% cakes_product)
  oils    <- balance %>% filter(i4 %in% oils_product)
  
  rm(balance)
  
  meats <- meats %>% 
    mutate(supply = PROD + ISCH) %>% 
    mutate(demand = HCON) 
  
  meats <- meats %>% select(i1, i4, i5, supply, demand, Exports_noIntra, Imports_noIntra, nettrade)
  
  cereals <- cereals %>% 
     mutate(supply = PROD + ISCH) %>% 
     mutate(demand = HCON + PROC + BIOF + FEED)
   
  cereals <- cereals %>% select(i1, i4, i5, supply, demand, Exports_noIntra, Imports_noIntra, nettrade)
  
  dairy <- dairy %>% 
    mutate(supply = PROD) %>% 
    mutate(demand = HCON + PROC + FEED) 
  
  dairy <- dairy %>% select(i1, i4, i5, supply, demand, Exports_noIntra, Imports_noIntra, nettrade)
  
  oilseeds <- oilseeds %>% 
    mutate(supply = PROD) %>% 
    mutate(demand = HCON + PROC + FEED) 
  
  oilseeds <- oilseeds %>% select(i1, i4, i5, supply, demand, Exports_noIntra, Imports_noIntra, nettrade)
  
  cakes <- cakes %>% 
    mutate(supply = PROD) %>% 
    mutate(demand = HCON + FEED) 
  
  cakes <- cakes %>% select(i1, i4, i5, supply, demand, Exports_noIntra, Imports_noIntra, nettrade)
  
  oils <- oils %>% 
    mutate(supply = PROD) %>% 
    mutate(demand = HCON + PROC + FEED + BIOF) 
  
  oils <- oils %>% select(i1, i4, i5, supply, demand, Exports_noIntra, Imports_noIntra, nettrade)
  
  
  balance <- rbind(meats, dairy, cereals, oilseeds, cakes, oils)
  
# get rid of WHEA if not selected by the user
  balance <- balance %>% filter(i4 %in% product_list)
  
  balance <- balance %>% 
      arrange(i4, i1, i5) %>% 
      select(i4, i1, i5, supply, demand, Exports_noIntra, Imports_noIntra, nettrade)


  return(balance)
}

#' Auxiliary function to load results (LEVL, YILD) related to dairying.
#' Both low and high-intensity variants included
#' 
#' 
get_dairy_aux <- function(datacube, region_list, product_list = c("DCOH", "DCOL")){
  
# herd size and milk yield  
   bal_item    <- c("LEVL", "YILD")

  
# select relevant slice of the whole capri datacube  
  datacube <- datacube %>% 
    filter(i1 %in% region_list, i2 == "", i4 %in% bal_item, i3 %in% product_list, i5 != "BAS") %>% 
    select(i1, i3, i4, i5, value)
  
  
  
# calculate milk supply from dairy yield and dairy cow herds
  datacube <- spread(datacube, i4, value)
  datacube <- datacube %>% mutate(supply = YILD * LEVL / 1000)
  
  
# aggregate high- and low-yield variants  
  datacube <- melt(datacube, id=c("i1","i3","i5"))
  datacube <- datacube %>% group_by(i1, i5, variable) %>% summarise(value=sum(value))
  
# recalculate aggregate yields
  datacube <- spread(datacube, variable, value)
  datacube <- datacube %>% mutate(YILD = supply / LEVL)
  
  return(datacube)

  
}


#'  Auxiliary function: extract milk deliveries
#'
#'
get_milk_deliveries <- function(datacube, region_list, product_list = c("PRCC")){
  
  bal_item    <- c("COMI")
  
  # select relevant slice of the whole capri datacube  
  datacube <- datacube %>% 
    filter(i1 %in% region_list, i2 == "", i4 %in% bal_item, i3 %in% product_list, i5 != "BAS") %>% 
    select(i1, i5, value)
  
  return(datacube)
  
}
  



#' @param region_list List of regions
#' @param year_list List of simulation years
#' @param folder Folder where the baseline .RData files are stored. Default "mydata"
#' 
#' 
get_dairy <- function(region_list, product_list = c("DCOH", "DCOL"), scenario_list, folder = "mydata"){
  
# dairy cow prod. activity divided into low/high-yield variants  

# load baselines for all year
  for(i in 1:length(scenario_list)) {load(file=paste(folder, "/", scenario_list[i], ".RData", sep=""))}

# part 1) get herds/yields/supply  
    
# start with the first year...  
  dairy_herd <- get_dairy_aux(get(scenario_list[1]), region_list, product_list)
  
# ...continue with the other years (if those exist)
  if (length(scenario_list) > 1) {
    for(i in 2:length(scenario_list)){
      
      
      
      dairy_herd <- rbind(dairy_herd, get_dairy_aux(get(scenario_list[i]), region_list, product_list))
      
      
    }
  }
  
  
# part 2) get milk deliveries (not the same as supply...)
  deliveries <- get_milk_deliveries(get(scenario_list[1]), region_list)
  
  # ...continue with the other years (if those exist)
  if (length(scenario_list) > 1) {
    for(i in 2:length(scenario_list)){
      
      
      
      deliveries <- rbind(deliveries, get_milk_deliveries(get(scenario_list[i]), region_list))
      
      
    }
  }
  
  dairy_herd <- left_join(dairy_herd, deliveries, by = c("i1", "i5"))
  
  return(dairy_herd)
}



get_cowmilk <- function(region_list, year_list){
  
  attrib3_list <- c("COMI", "LEVL")
  
  # load baselines for all year indicated in year_list  
  for(i in 1:length(year_list)) {load(file=paste("data/baseline",year_list[i], ".RData", sep=""))}
  
  # start with the first year...  
  cowmilk <- get_cowmilk_aux(get(paste("baseline",year_list[1], sep="")), region_list, attrib3_list, year_list[1])
  
  # ...continue with the others in a for loop  
  if (length(year_list) > 1) {
    for(i in 2:length(year_list)){
      
      
      
      cowmilk <- rbind(cowmilk, get_dairy_aux(get(paste("baseline",year_list[i], sep="")), region_list, attrib3_list, year_list[i]))
      
      
    }
  }
  
  return(cowmilk)
}

get_cowmilk_aux <- function(datacube, region_list, attrib3_list = c("GROF", "PRCC", "DCOW"), year){
  
  attrib4_list    <- c("COMI", "LEVL")
  
  
  datacube <- datacube %>% 
    filter(i1 %in% region_list, i2 == "", i4 %in% attrib4_list, i3 %in% attrib3_list, i5 == year) %>% 
    select(i1, i3, i4, i5, value)
  
  
  
  return(datacube)
  
  
}