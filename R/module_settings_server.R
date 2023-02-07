#' Settings Server
#'
#' @param input provided by \pkg{Shiny}.
#' @param output provided by \pkg{Shiny}.
#' @param session provided by \pkg{Shiny}.
#' @param user_settings a [shiny::reactiveValues()] list.
#' @seealso [module_settings_ui()]
#' @family modules
#' @keywords internal
#' @export
module_settings_server <- function(id, user_settings) {
  moduleServer(id, function(input, output, session) {

    ## Download handler for calibration data
    output$download_calibration <- downloadHandler(
      filename = "CalibrationDatasets.zip",
      content = function(file){
        ## Create ZIP-file
        utils::zip(zipfile = file, files = calibration_data, flags = "-j")
      },
      contentType = "application/zip"
    )

    ## Upload own calibration dataset
    observeEvent(input$upload_calibration, {
      ## Inspect content
      load(input$upload_calibration$datapath, safe <- new.env())

      if (length(ls(safe)) == 3 && all(c("results_CT", "results_ITC", "sourceDR_FINAL") %in% ls(safe))) {
        ## Reset data
        results_CT <<- NULL
        results_ITC <<- NULL
        sourceDR_FINAL <<- NULL

        ## Load data
        load(input$upload_calibration$datapath, envir = current_env)

        ## Correct input path
        updateSelectInput(
          session = session,
          inputId = "calibration_data",
          choices = "Own dataset loaded"
        )
      } else {
        showModal(modalDialog(
          title = "Error",
          "The uploaded calibration dataset is not supported, please only upload allowed data!",
          footer = modalButton("Ok")
        ))
      }
    })

    ## Clear own dataset
    observeEvent(input$clear_calibration, {
      ## Correct input path
      updateSelectInput(
        session = session,
        inputId = "calibration_data",
        choices = basename(calibration_data)
      )
    })

    ## Render
    output$last_saved <- renderText({
      req(user_settings$saved)
      paste("Last saved at", user_settings$saved)
    })

    output$session <- renderPrint({
      utils::sessionInfo()
    })

    ## Bookmark
    onBookmark(function(state) {
      user_settings$saved <- Sys.time()
      # state is a mutable reference object,
      # we can add arbitrary values to it.
      state$values$time <- user_settings$saved
    })

    onRestore(function(state) {
      user_settings$saved <- state$values$time
    })
  })
}
