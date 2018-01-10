#' SetWorkingDir
#'
#' Create a working directory if it is not available
#'
#' @param wdir Working directory path
#'
#' @export
#'
SetWorkingDir <- function(wdir) {

  # Creat working dir if not exists
  dir.create(wdir, showWarnings=F,recursive=T)
  setwd(wdir)

}
