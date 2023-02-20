#' gogoopendir
#' Opens your working directory!
#' @param dir  getwd()
#'
#' @return opens a directory window
#' @export
#'
gogoopendir <- function(dir = getwd()){
  if (.Platform['OS.type'] == "windows"){
    shell.exec(dir)
  } else {
    system(paste(Sys.getenv("R_BROWSER"), dir))
  }
}
