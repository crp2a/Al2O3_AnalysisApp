# SHINY

#' Run a Shiny App
#'
#' A wrapper for [shiny::shinyAppDir()].
#' @param browser A [`logical`] scalar: should the app be run in
#'  the browser?
#' @param display A [`character`] string specifying the mode in which
#'  to display the application (see [shiny::runApp()]).
#' @examples
#' \dontrun{
#' run_app()
#' }
#' @return A \pkg{shiny} application object.
#' @family shiny
#' @author N. Frerebeau
#' @export
run_app <- function(browser = TRUE, display = "auto") {
  app_dir <- system.file("app", package = "Al2O3AnalysisApp")
  if (app_dir == "")
    stop("Could not find the app. ",
         "Try re-installing 'Al2O3AnalysisApp'.", call. = FALSE)
  shiny::shinyAppDir(
    appDir = app_dir,
    options = list(launch.browser = browser, display.mode = display)
  )
}
