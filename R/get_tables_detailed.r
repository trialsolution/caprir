#' Get detailed balance tables (demand broken down to its comoponents)
#'
#' @param region_list List of regions (CAPRI code) for which the market balances should be derived
#' @param product_list List of commodities (CAPRI code) for which the market balances should be derived
#' @param scenario_list List of CAPRI scenarios for which the market balances should be derived
#' @param folder Path to folder containing the CAPRI result files
#'
#' @return
#'
#' @export
#'
#' @examples
#'
#' # CAPRI BALANCES (NO INTRA-TRADE)
#' #-------------------------------
#'
#' # define regions and commodities -- for which products do you need the market balances?
#' load("data/eu_region.RData")
#'
#' meat_product       <- c("PORK", "POUM", "BEEF", "SGMT")
#' dairy_product      <- c("MILK", "BUTT", "CREM", "FRMI", "CHES", "SMIP", "COCM", "WMIO", "CASE", "WHEP")
#' cereal_product     <- c("CERE", "RYEM", "WHEA", "OATS", "BARL", "OCER", "MAIZ", "RICE")
#' oilseeds_product   <- c("RAPE", "SOYA", "SUNF")
#' cakes_product      <- c("RAPC", "SUNC", "SOYC", "CAKS")
#' oils_product       <- c("RAPO", "SUNO", "SOYO", "OLIO", "PLMO")
#'
#' baseline_scenarios <- c("res_2_0810mtr_rd_ref", "res_2_0813mtr_rd_ref", "res_2_0820mtr_rd_ref", "res_2_0825mtr_rd_ref", "res_2_0830mtr_rd_ref")
#'
#' # get market balances
#'
#' meat_balance       <- convert_balance_detailed(eu_region, meat_product, baseline_scenarios, folder = "mydata")
#' cereal_balance     <- convert_balance_detailed(eu_region, cereal_product, baseline_scenarios, folder = "mydata")
#' dairy_balance      <- convert_balance_detailed(eu_region, dairy_product, baseline_scenarios, folder = "mydata")
#' oilseeds_balance   <- convert_balance_detailed(eu_region, oilseeds_product, baseline_scenarios, folder = "mydata")
#' cakes_balance      <- convert_balance_detailed(eu_region, cakes_product, baseline_scenarios, folder = "mydata")
#' oils_balance       <- convert_balance_detailed(eu_region, oils_product, baseline_scenarios, folder = "mydata")
#' sugar              <- convert_balance_detailed(eu_region, c("SUGA"), baseline_scenarios, folder = "mydata")
#'
convert_balance_detailed <- function(region_list, product_list, scenario_list, folder = "mydata"){

# load all scenario results into memory
  for(i in 1:length(scenario_list)) {

    assign(scenario_list[i], rgdx.param(paste(folder, "/", scenario_list[i], ".gdx", sep = ""), "dataout"))

    assign(scenario_list[i], as_tibble(get(scenario_list[i])))

#    load(file=paste(folder, "/", scenario_list[i], ".RData", sep=""))

    }

# get market balances for the first scenario
  balance <- get_market_balance(get(scenario_list[1]), region_list, product_list)

# continue with the other scenarios
  if (length(scenario_list) > 1) {
    for(i in 2:length(scenario_list)){



      balance <- rbind(balance, get_market_balance(get(scenario_list[i]), region_list, product_list))


    }
  }

  balance <- spread(balance, .i3, dataOut)

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


# TODO: change the structure to a uniform 'balance', i.e. no distinction by product groups

  balance <- balance %>%
    mutate(supply = PROD) %>%
    mutate(human_cons = HCON) %>%
    mutate(processing = PROC) %>%
    mutate(biofuels = BIOF) %>%
    mutate(feed = FEED) %>%
    mutate(interv_ch = ISCH) %>%
    mutate(imports = Imports_noIntra) %>%
    mutate(exports = Exports_noIntra)

  balance <- balance %>%
    select(.i1, .i4, .i5, supply, human_cons, processing, biofuels, feed, interv_ch, imports, exports, nettrade)


  balance <- balance %>%
    arrange(.i4, .i1, .i5) %>%
    select(.i4, .i1, .i5, supply, human_cons, processing, biofuels, feed, interv_ch, imports, exports, nettrade)


  return(balance)
}


#' Get the product balances for all baseline years and all products
#'
#' @param region_list List of regions (CAPRI code) for which balances should be derived
#' @param product_list List of commodities (CAPRI code) for which balances should be derived
#' @param scenario_list List of CAPRI scenarios for which  balances should be derived
#' @param folder Path to folder containing the CAPRI result files
#'
#' @return A tibble with product balances
#'
#' @export
#'
convert_product_balance <- function(region_list, product_list, scenario_list, folder = "mydata"){

  for(i in 1:length(scenario_list)) {


    assign(scenario_list[i], rgdx.param(paste(folder, "/", scenario_list[i], ".gdx", sep = ""), "dataout"))

    assign(scenario_list[i], as_tibble(get(scenario_list[i])))

#    load(file=paste(folder, "/", scenario_list[i], ".RData", sep=""))

    }


  balance <- get_product_balance(get(scenario_list[1]), region_list, product_list)

  if (length(scenario_list) > 1) {
    for(i in 2:length(scenario_list)){



      balance <- rbind(balance, get_product_balance(get(scenario_list[i]), region_list, product_list))


    }
  }


  balance <- spread(balance, .i3, dataOut)

  # replace NAs with zeros
  balance[is.na(balance)] <- 0

  # add potentially missing columns
  if(!("LOSF" %in% colnames(balance))) { balance <- mutate(balance, LOSF = 0)}
  if(!("MAPR" %in% colnames(balance))) { balance <- mutate(balance, MAPR = 0)}
  if(!("GROF" %in% colnames(balance))) { balance <- mutate(balance, GROF = 0)}
  if(!("SEDF" %in% colnames(balance))) { balance <- mutate(balance, SEDF = 0)}
  if(!("INTF" %in% colnames(balance))) { balance <- mutate(balance, INTF = 0)}
  if(!("IMPT" %in% colnames(balance))) { balance <- mutate(balance, IMPT = 0)}
  if(!("INTP" %in% colnames(balance))) { balance <- mutate(balance, INTP = 0)}
  if(!("EXPT" %in% colnames(balance))) { balance <- mutate(balance, EXPT = 0)}
  if(!("NTRD" %in% colnames(balance))) { balance <- mutate(balance, NTRD = 0)}
  if(!("HCOM" %in% colnames(balance))) { balance <- mutate(balance, HCOM = 0)}
  if(!("DOMM" %in% colnames(balance))) { balance <- mutate(balance, DOMM = 0)}
  if(!("FEDM" %in% colnames(balance))) { balance <- mutate(balance, FEDM = 0)}
  if(!("SEDM" %in% colnames(balance))) { balance <- mutate(balance, SEDM = 0)}
  if(!("PRCM" %in% colnames(balance))) { balance <- mutate(balance, PRCM = 0)}
  if(!("BIOF" %in% colnames(balance))) { balance <- mutate(balance, BIOF = 0)}
  if(!("INDM" %in% colnames(balance))) { balance <- mutate(balance, INDM = 0)}
  if(!("LOSM" %in% colnames(balance))) { balance <- mutate(balance, LOSM = 0)}
  if(!("STCM" %in% colnames(balance))) { balance <- mutate(balance, STCM = 0)}


#  load("data/product_list.RData")

  all_product <- product_list$code

  my_all    <- balance %>% filter(.i4 %in% all_product)


  my_all <- my_all %>%
    mutate(supply = GROF) %>%
    mutate(mark_prod = MAPR) %>%
    mutate(losses_farm = LOSF + INTF) %>%
    mutate(seed_farm = SEDF) %>%
    mutate(imports = IMPT) %>%
    mutate(exports = EXPT) %>%
    mutate(nettrade = NTRD) %>%
    mutate(intervention = INTP) %>%
    mutate(dom_use = DOMM) %>%
    mutate(human_cons = HCOM) %>%
    mutate(feed = FEDM) %>%
    mutate(seed_market = SEDM) %>%
    mutate(processing_sec = PRCM) %>%
    mutate(biofuels = BIOF) %>%
    mutate(other_ind = INDM) %>%
    mutate(losses_market = LOSM) %>%
    mutate(stockch_market = STCM)


#select columns
  my_all <- my_all %>%
    select(.i1, .i4, .i5, supply, mark_prod, losses_farm, seed_farm, imports, exports
           , nettrade, intervention, dom_use, human_cons, feed, seed_market, processing_sec
           , biofuels, other_ind, losses_market, stockch_market)

#re-arrange ordering
  my_all <- my_all %>%
    arrange(.i4, .i1, .i5)

# meaningful column names

  colnames(my_all)[1] <- "region"
  colnames(my_all)[2] <- "product"
  colnames(my_all)[3] <- "year"


  return(my_all)
}




#' Get the Farm|Supply details tables for all regions/activities/year
#'
#' @param region_list List of regions (CAPRI code) for which product data should be derived
#' @param product_list List of commodities (CAPRI code) for which product data should be derived
#' @param scenario_list List of CAPRI scenarios for which  product datashould be derived
#' @param folder Path to folder containing the CAPRI result files
#'
#' @export
#'
convert_supply_details <- function(region_list, product_list, scenario_list, folder = "mydata"){

  for(i in 1:length(scenario_list)) {

    assign(scenario_list[i], rgdx.param(paste(folder, "/", scenario_list[i], ".gdx", sep = ""), "dataout"))

    assign(scenario_list[i], as_tibble(get(scenario_list[i])))

#    load(file=paste(folder, "/", scenario_list[i], ".RData", sep=""))

    }



  balance <- get_supply_detail(get(scenario_list[1]), region_list, product_list)

  if (length(scenario_list) > 1) {
    for(i in 2:length(scenario_list)){


      balance <- rbind(balance, get_supply_detail(get(scenario_list[i]), region_list, product_list))


    }
  }


  balance <- spread(balance, .i4, dataOut)

  # replace NAs with zeros
  balance[is.na(balance)] <- 0

  # add potentially missing columns
  if(!("MGVA" %in% colnames(balance))) { balance <- mutate(balance, MGVA = 0)}
  if(!("YILD" %in% colnames(balance))) { balance <- mutate(balance, YILD = 0)}
  if(!("LEVL" %in% colnames(balance))) { balance <- mutate(balance, LEVL = 0)}


#  load("data/activity_list.RData")

  all_activity <- activity_list$code

  my_all    <- balance %>% filter(.i3 %in% all_activity)


  my_all <- my_all %>%
    mutate(gross_value_added = MGVA) %>%
    mutate(yield = YILD) %>%
    mutate(level = LEVL)

  my_all <- my_all %>%
    mutate(supply = yield * level)

  #select columns
  my_all <- my_all %>%
    select(.i1, .i3, .i5, supply, yield, level, gross_value_added)

  #re-arrange ordering
  my_all <- my_all %>%
    arrange(.i3, .i1, .i5)

  # meaningful column names

  colnames(my_all)[1] <- "region"
  colnames(my_all)[2] <- "product"
  colnames(my_all)[3] <- "year"


  return(my_all)
}

