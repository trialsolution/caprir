#' CAPRI gdx utilities
#'
#' simply loads the data cube from a CAPMOD result file
#'
#' @param filename Name of the .gdx file to be loaded
#'
#' @examples
#' load_dataout("test.gdx")
#'

load_dataout <- function(filename){

  dataout <- rgdx.param(filename, "dataOut")
  dataout <- as.tibble(dataout)
  colnames(dataout) <- c("region","dim5","cols","rows","year","value")

  return(dataout)
}




#' Load CAPRI results and filter to a user-specified subset of results
#'
#' @param filename Name of the file
#' @param selregion Character vector of regions, default = "all"
#' @param seldim5 Selection of the elements in the fifth dimension of the CAPRI data cube. By default it is the empty element
#' @param selcols Selection of columns in the CAPRI data cube
#' @param selrows Selection of rows in the CAPRI data cube
#' @param simyear Simulation year to be loaded/filtered
#' @param scenarionam Name of the scenario
#'
#' @example
#'
#'
#' # Extract pork balance from many scenario versions
#'
#'
#' get_pork_balance <- function(version) {
#'
#'   return(capri_filter(filename  = paste("gdx/baseline_", version, ".gdx", sep = ""),
#'                       seldim5   = "",
#'                       selcols   = c("prod","hcon","proc","Imports_noIntra","Exports_noIntra"),
#'                       selrows   = "pork",
#'                       selregion = c("eu","EU028000"),
#'                       simyear   = "2030",
#'                       scenarioname = version))
#'
#'
#' }
#'
#'
#'
#'
#' get_pork_balance_n <- function(versions){
#'
#'   pork_balance <- get_pork_balance(versions[1])
#'
#'   for(i in versions){
#'
#'     if(i != versions[1]){pork_balance <- rbind(pork_balance, get_pork_balance(i))}
#'
#'   }
#'
#'   return(pork_balance)
#' }
#'
#'
#' versions <- c("v5","v6","v7","v8","v9")
#'
#' pork_balance <- get_pork_balance_n(versions = versions)
#'
#'
#' @export
#'
capri_filter <- function(filename, selregion = "all", seldim5 = "", selcols, selrows, simyear = "2030", scenarioname = "baseline"){


  x <- load_dataout(filename)

# the grepl-paste trick is needed for case insensitive filtering
  if(length(seldim5))       {x <- x %>% filter(grepl(paste("\\b", paste(seldim5, collapse = "\\b|"), "\\b", sep =""),
                                                     dim5, ignore.case = TRUE))}

# special case for the filter (empty cell often specified for dim5 in the .gdx cube)
  if(seldim5 == "")         {x <- x %>% filter(dim5 == "")}

  if(seldim5[1] == "-1")    {x <- x %>% select(-dim5)}



if(selregion[1] != "all") {
  if(length(selregion))       {x <- x %>% filter(grepl(paste("\\b", paste(selregion, collapse = "\\b|"), "\\b", sep =""),
                                                       region, ignore.case = TRUE))}
  if(selregion[1] == "-1")    {x <- x %>% select(-region)}
}


  if(length(selcols))       {x <- x %>% filter(grepl(paste("\\b", paste(selcols, collapse = "\\b|"), "\\b", sep =""),
                                                     cols, ignore.case = TRUE))}
  if(selcols[1] == "-1")    {x <- x %>% select(-cols)}


  if(length(selrows))       {x <- x %>% filter(grepl(paste("\\b", paste(selrows, collapse = "\\b|"), "\\b", sep =""),
                                                     rows, ignore.case = TRUE))}
  if(selrows[1] == "-1")    {x <- x %>% select(-rows)}


  if(length(simyear))       {x <- x %>% filter(year %in% simyear)}
  if(simyear[1] == "-1")    {x <- x %>% select(-year)}


  # optional regional selection
  if(selregion[1] == "nuts0") {x <- x %>% filter(grepl("*000000", region))}

  # optional scenario name to attach
  if(nrow(x) > 0) {x$scenario <- scenarioname}

  return(x)
}

#' function to calculate percentage changes
#'
#' @param a Base number of the percentage change calculation
#' @param b Target number of the percentage change calculation
#'
pchange <- function(a,b){(b-a)/abs(a)*100}


#' write a parameter into a .gdx file
#'
#' @param x R object
#' @param file Name of the output .gdx
#' @param symname Name of the GAMS parameter in the output .gdx file
#' @param ts GAMS parameter description
#'
#' @export
#'
write_param_togdx <- function(x, file, symname = "default", ts = "default") {

  x <- data.frame(x)
  attr(x, "symName") <- symname
  attr(x, "ts")      <- ts

  wgdx.lst(file, x)

}
