#' Open a directory in the system file browser
#'
#' Opens a folder path in the operating system's default file browser.
#' On Windows, the path is normalized to Windows-style backslashes before
#' being passed to Explorer. This helps avoid failures when `getwd()`
#' returns a UNC/network path with forward slashes.
#'
#' @param dir A directory path. Defaults to the current working directory.
#'
#' @return No returned value. Called for its side effect of opening the folder.
#' @export
#'
#' @examples
#' \dontrun{
#' gogoopendir()
#' gogoopendir("Z:/Projects")
#' }
gogoopendir <- function(dir = getwd()) {

  if (.Platform["OS.type"] == "windows") {

    dir <- normalizePath(dir, winslash = "\\", mustWork = FALSE)
    shell(sprintf('explorer.exe "%s"', dir))

  } else {

    system(paste(Sys.getenv("R_BROWSER"), shQuote(dir)))

  }

  invisible(NULL)
}
