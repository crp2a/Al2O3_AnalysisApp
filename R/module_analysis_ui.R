#' Analysis UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [module_analysis_server()]
#' @family modules
#' @keywords internal
#' @export
module_analysis_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tabPanel(
    title = "2. Analyse",
    icon = icon("bolt"),
    ## SIDEBAR -----------------------------------------------------------------
    sidebarLayout(
      sidebarPanel(
        width = 3,
        shinyjs::useShinyjs(),
        selectInput(
          inputId = ns("select"),
          label = "Select a calibration dataset...",
          choices = basename(calibration_data)
        ),
        div(
          strong(textOutput(outputId = ns("source_DR_FINAL_today"))),
          textOutput(outputId = ns("source_DR_FINAL")),
          align = "center"
        ),
        tags$hr(),
        sliderInput(
          inputId = ns("signal_integral"),
          label = "Signal integral",
          min = 1,
          max = 99,
          value = c(1, 99)
        ),
        checkboxInput(
          inputId = ns("cross_talk_correction"),
          label = "Apply cross-talk correction",
          value = TRUE
        ),
        checkboxInput(
          inputId = ns("travel_dosimeters"),
          label = "Apply travel dosimeter correction",
          value = FALSE
        ),
        checkboxInput(
          inputId = ns("normalize_curve"),
          label = "Normalize curve",
          value = FALSE
        ),
        tags$hr(),
        actionButton(
          inputId = ns("run"),
          label = "(Re) Run analysis...",
          icon = icon("circle-play"),
          style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
        ),
        uiOutput(outputId = ns("export_analysis"))
      ),
      ## MAIN ------------------------------------------------------------------
      mainPanel(
        fluidRow(
          h3(textOutput(outputId = ns("analysis_error"))),
          plotOutput(outputId = ns("analysis_plot"))
        ),
        fluidRow(
          helpText(
            "Note: Dose values are here listed in seconds, not µGy!",
            "To reject data permanently, go back to the 'Import' panel."
          ),
          htmlOutput(outputId = ns("warnings"), style = "color: red;"),
          rhandsontable::rHandsontableOutput(
            outputId = ns("analysis_results"),
            width = "100%"
          )
        )
      )
    )
  )
}
