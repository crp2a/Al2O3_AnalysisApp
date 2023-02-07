#' Settings UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [module_settings_server()]
#' @family modules
#' @keywords internal
#' @export
module_settings_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tabPanel(
    "Settings",
    icon = icon("gear"),
    fluidPage(
      fluidRow(
        column(
          width = 3,
          h4("Bookmarking"),
          bookmarkButton(),
          tags$p(textOutput(outputId = ns("last_saved"))),
          tags$br(),
          h4("Available calibration datasets"),
          HTML(paste0("&nbsp; + ", basename(calibration_data), "<br>")),
          downloadButton(
            outputId = ns("download_calibration"),
            label = "Download calibration datasets"
          ),
          tags$hr(),
          h4("Allowed md5 data verification hash values"),
          HTML(paste0(verification_hash, collapse = "<br>")),
          tags$p(
            "The verification hash was generated from a reference data structure
            the time this app was created to ensure that only measurement data
            that can be analysed by this app are imported. The hash is hard
            coded and cannot be changed."
          ),
          tags$hr(),
          h4("Dangerous zone"),
          fileInput(
            inputId = ns("upload_calibrationdata"),
            label = "Upload own calibration dataset",
            accept = "*.Rdata"
          ),
          tags$p(
            "The '.Rdata' file must contain the objects:",
            tags$ul(
              tags$li("results_CT"),
              tags$li("results_ITC"),
              tags$li("sourceDR_FINAL")
            ),
            "Please note that any dataset uploaded here is only valid for the
            current shiny session!"
          ),
          actionButton(
            inputId = ns("clear_calibrationdata"),
            label = "Remove own calibration data",
            icon = icon("remove",lib = "glyphicon")
          )
        ),
        column(
          width = 9,
          h4("Session information"),
          verbatimTextOutput(outputId = ns("session"))
        )
      )
    )
  )
}
