#' Shiny App Server Function
#'
#' @param input provided by \pkg{Shiny}.
#' @param output provided by \pkg{Shiny}.
#' @param session provided by \pkg{Shiny}.
#' @author S.Kreutzer, N. Frerebeau
#' @keywords internal
#' @noRd
shiny_server <- function(input, output, session) {
  # Set reactive values
  # 'user_data' is a list with the following elements:
  # * 'data': a list as returned by Luminescence::read_XSYG2R(import = TRUE)
  # * 'info': a data.frame as returned by Luminescence::read_XSYG2R(import = FALSE)
  # * 'samples': a data.frame of metadata
  # * 'dose': a data.frame of (corrected) dose rates (one row per chip)
  # * 'final': a data.frame of gamma dose rates (one row per sample)
  user_data <- reactiveValues()
  user_settings <- reactiveValues()
  module_import_server("import", user_data, user_settings)
  module_analysis_server("analysis", user_data, user_settings)
  module_processing_server("processing", user_data, user_settings)
  module_settings_server("settings", user_settings)
  session$onSessionEnded(stopApp)
}
