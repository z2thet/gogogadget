#'  Write CSV file to specified location No row names
#'
#' @param dfr A data frame name.
#' @param loc Output folder path. Default is "F:\\A=Rexports\\" for Moffitt employees.
#' @return Outputs a csv file
#' @examples
#' gogogadgetexport(mtcars)
#'
#' @export

gogogadgetexport<-function(dfr, loc = 'F:\\A=Rexports\\'){
  x<-deparse(substitute(dfr));
   utils::write.csv(dfr,file=paste0(loc,x,'.csv'), row.names=F)
  };
