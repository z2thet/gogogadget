#' gogogadgetexport
#'
#' @param dfr what to save; saves a .csv file in `F: A=Rexports ``
#' @param loc where to save
#'
#' @return a csv file
#' @export
#'
gogogadgetexport <- function(dfr, loc = 'F:\\A=Rexports\\'){
  x<-deparse(substitute(dfr));
  utils::write.csv(dfr,file=paste0(loc,x,'.csv'), row.names=F)
}
