#' Find a variable in a data set or partial matches of names.
#'
#' @param var A quoted string ie variable name or part of the name.
#' @param dataset A data frame name.
#' @return names and column number of variables or matches.
#' @examples
#' gogofindvar("mgp", mtcars)
#' gogofindvar("gear", mtcars)
#'
#' @export

 gogofindvar<-function(var,dataset){ #Var must be in quotes...
  return(data.frame(nam=grep(var, names(dataset), ignore.case = TRUE, value = T),
                    nom=grep(var, names(dataset), ignore.case = TRUE, value = F)));
}
