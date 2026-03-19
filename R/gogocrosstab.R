#' Create Cross-Tabulation with Flexible Options
#'
#' @param data A data frame
#' @param var1 First variable (row variable) - can be quoted or unquoted
#' @param var2 Second variable (column variable) - can be quoted or unquoted
#' @param show_na Logical, whether to show missing values (default: TRUE)
#' @param show_totals Logical, whether to show row and column totals (default: TRUE)
#' @param percentages Character, type of percentages to show: "none", "row", "col", or "all" (default: "none")
#' @param digits Integer, number of decimal places for percentages (default: 1)
#'
#' @return A formatted cross-tabulation table
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic cross-tab
#' gogocrosstab(mtcars, cyl, gear)
#' 
#' # With row percentages
#' gogocrosstab(mtcars, cyl, gear, percentages = "row")
#' 
#' # With all percentages and totals
#' gogocrosstab(mtcars, cyl, gear, percentages = "all", show_totals = TRUE)
#' }
gogocrosstab <- function(data, var1, var2, 
                        show_na = TRUE, 
                        show_totals = TRUE,
                        percentages = c("none", "row", "col", "all"),
                        digits = 1) {
  
  # Load required packages
  if (!requireNamespace("janitor", quietly = TRUE)) {
    stop("Package 'janitor' is required but not installed.")
  }
  
  # Match arguments
  percentages <- match.arg(percentages)
  
  # Capture variable names for labeling
  var1_name <- deparse(substitute(var1))
  var2_name <- deparse(substitute(var2))
  
  # Create basic cross-tabulation
  if (show_na) {
    result <- janitor::tabyl(data, {{ var1 }}, {{ var2 }}, show_na = TRUE)
  } else {
    result <- janitor::tabyl(data, {{ var1 }}, {{ var2 }}, show_na = FALSE)
  }
  
  # Add totals FIRST if requested (must come before percentages)
  if (show_totals) {
    result <- janitor::adorn_totals(result, where = c("row", "col"))
  }
  
  # Add percentages based on user choice (must come AFTER totals)
  if (percentages == "row") {
    result <- janitor::adorn_percentages(result, denominator = "row") |>
      janitor::adorn_pct_formatting(digits = digits) |>
      janitor::adorn_ns(position = "front")
  } else if (percentages == "col") {
    result <- janitor::adorn_percentages(result, denominator = "col") |>
      janitor::adorn_pct_formatting(digits = digits) |>
      janitor::adorn_ns(position = "front")
  } else if (percentages == "all") {
    result <- janitor::adorn_percentages(result, denominator = "all") |>
      janitor::adorn_pct_formatting(digits = digits) |>
      janitor::adorn_ns(position = "front")
  }
  
  # Add title with variable names
  title <- paste("Cross-tabulation:", var1_name, "by", var2_name)
  
  # Print with title
  cat(title, "\n")
  cat(paste(rep("=", nchar(title)), collapse = ""), "\n\n")
  
  # Return the formatted result
  result
}