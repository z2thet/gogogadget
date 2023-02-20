#' gogofindvar
#'
#' @param var  a character string to find in the names of the data frame or tibble maybe. should be in quotes
#' @param dataset a data frame or maybe a tibble in this day and age whatever your data identifies as, right?
#'
#' @return a data frame
#' @export
#'
gogofindvar <- function(var,dataset){ #Var must be in quotes...
  return(data.frame(nam=grep(var, names(dataset), ignore.case = TRUE, value = T),
                    nom=grep(var, names(dataset), ignore.case = TRUE, value = F)));
}
