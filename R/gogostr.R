#' gogostr
#'
#' @param dfr an object I think
#'
#' @return str about an object
#' @export
#'
gogostr <- function(dfr){utils::str(dfr,
                         max.level = 2,
                         vec.len  = 2, #strO$vec.len,
                         digits.d = 3, #strO$digits.d,
                         nchar.max = 128,
                         give.attr = TRUE,
                         drop.deparse.attr = TRUE,#strO$drop.deparse.attr,
                         give.head = TRUE,
                         give.length = TRUE,
                         width = getOption("width"),
                         nest.lev = 0,
                         indent.str = "!--WTFIT->",#paste(rep.int(" ", max(0, nest.lev + 1)), collapse = ".."),
                         comp.str = "$ ",
                         no.list = FALSE,
                         envir = baseenv() )}
