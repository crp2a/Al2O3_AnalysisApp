#' Import UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [module_import_server()]
#' @family modules
#' @keywords internal
#' @export
module_import_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tabPanel(
    title = "1. Import",
    icon = icon("upload"),
    ## SIDEBAR -----------------------------------------------------------------
    sidebarLayout(
      sidebarPanel(
        width = 3,
        fileInput(
          inputId = ns("files"),
          label = "Import XSYG-file(s) containing your measurement data...",
          multiple = TRUE,
          accept = "*.xsyg"
        ),
        checkboxInput(
          inputId = ns("file_names_assignment"),
          label = "Use filenames for wheel assignment",
          value = FALSE
        ),
        fluidRow(
          tags$hr(),
          div(
            plotOutput(outputId = ns("carousel")),
            align = "center"
          )
        )
      ),
      ## MAIN ------------------------------------------------------------------
      mainPanel(
        uiOutput(outputId = ns("tabs")), # Create all the needed tabs
        rhandsontable::rHandsontableOutput(outputId = ns("sample_info"))
      )
    )
  )
}
