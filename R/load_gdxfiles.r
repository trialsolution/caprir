#' Convert original CAPRI results into R format: gdx --> tbl_df --> RData
#' Results are only saved to the out_folder but not loaded into memory
#' You have to load() the RData file first before you can use it in R, but
#' this is usually done by the extraction scripts and not manually. See e.g. extract_gui_table().
#'
#' @param scenario Scenario file name. The usual structure applies:
#'        res_ + regional break-down(0,2,999) + _ + baseyear + simulation year
#' @param in_folder Folder containing result file. Usually 'results/capmod' in the CAPRI installation
#' @param out_folder Folder for the converted formats
#' @param gamspath Path to gams installation. GAMS libraries are needed for the gdxrrw package
#'
#' @example get_capmod_res("res_2_0830_baseline", in_folder = "results/capmod", out_folder = "mydata", gamspath = "/opt/GAMS")
#'
get_capmod_res <- function(scenario, in_folder, out_folder, gamspath = "/opt/GAMS"){

  igdx(gamspath)

  assign(scenario, rgdx.param(paste(in_folder, "/", scenario, ".gdx", sep = ""), "dataout"))
  assign(scenario, tbl_df(get(scenario)))
  save( list = scenario, file = paste(out_folder, "/", scenario, ".RData", sep = ""))

# substr(strsplit(scenario, "_")[[1]][3], start = 5, stop = nchar(strsplit(scenario, "_"[[1]][3])))

}
