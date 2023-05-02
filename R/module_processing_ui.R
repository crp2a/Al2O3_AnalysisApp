#' Post-Processing UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [module_processing_server()]
#' @family modules
#' @keywords internal
#' @export
module_processing_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tabPanel(
    title = "3. Post-processing",
    value = "post_processing_run",
    icon = icon("random"),
    ## SIDEBAR -----------------------------------------------------------------
    sidebarLayout(
      sidebarPanel(
        width = 3,
        plotOutput(
          outputId = ns("post_processing_boxplot")
        )
      ),
      ## MAIN ------------------------------------------------------------------
      mainPanel(
        fluidRow(
          h3(textOutput(outputId = ns("post_processing_error"))),
        ),
        fluidRow(
          style = "margin-top: 25px;",
          textOutput(outputId = ns("travel_correction_info")),
          textOutput(outputId = ns("post_processing_info")),
          rhandsontable::rHandsontableOutput(
            outputId = ns("post_processing_combined"),
            width = "100%"
          )
        ),
        fluidRow(
          style = "margin-top: 25px;",
          rhandsontable::rHandsontableOutput(
            outputId = ns("post_processing_final"),
            width = "100%"
          )
        )
      )
    )
  )
}
