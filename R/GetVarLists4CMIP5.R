#' GetVarLists4CMIP5
#'
#' Extract necessary list from NetCDF file names according to column number (coln: 1: variables, 2: time-step, 3: models, 4: scenarios, 5: ensambles, 6: periods)
#'
#' @param basedir Directory path containing NetCDF files
#' @param coln Column number for extracting available list (1: variables, 2: time-step, 3: models, 4: scenarios, 5: ensambles, 6: periods)
#'
#' @return varnm List of available variable
#' @export
#'
GetVarLists4CMIP5 <- function(basedir, coln) {


  srchstr = "*.nc"
  flist = list.files(basedir, pattern = glob2rx(srchstr), full.names = F)
  varnm = sapply(strsplit(flist, "_"), function(x) x[[coln]])
  varnm = unique(varnm)

  return(varnm)

}