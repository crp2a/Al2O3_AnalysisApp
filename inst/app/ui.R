#' Shiny App User Interface Object
#'
#' @author S.Kreutzer, N. Frerebeau
#' @keywords internal
#' @noRd
shiny_ui <- function(request) {
  fluidPage(
    navbarPage(
      title = HTML("Al<sub>2</sub>O<sub>3</sub>:C Analysis App"),
      id = "navbar",
      windowTitle = "Al2O3:C Analysis App",
      module_import_ui("import"),
      module_analysis_ui("analysis"),
      module_processing_ui("processing"),
      module_settings_ui("settings"),
      module_about_ui("about"),
      footer = tags$footer(
        style = "width: 100%;text-align: center;",
        "This software comes WITHOUT ANY WARRANTY.",
        icon("github"),
        a(href = "https://github.com/crp2a/Al2O3AnalysisApp/issues",
          rel = "external", title = "Issue",
          "Report a bug or request.")
      )
    )
  )
}
