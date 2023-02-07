#' Analysis Server
#'
#' @inheritParams module_import_server
#' @seealso [module_analysis_ui()]
#' @family modules
#' @keywords internal
#' @export
module_analysis_server <- function(id, user_data, user_settings) {
  moduleServer(id, function(input, output, session) {

    ## Remove all deselected values in the 'Import' tab
    included <- reactive({
      req(user_data$samples)
      user_data$samples$INCLUDE
    })
    included_data <- reactive({
      req(user_data$data)
      user_data$data[included()]
    })
    included_samples <- reactive({
      req(user_data$samples)
      user_data$samples[included(), -7, drop = FALSE] # Remove INCLUDE column
    })

    ## Identify travel dosimeters position
    travel <- reactive({
      which(included_samples()$TYPE == "travel")
    })

    ## Cross talk correction?
    cross_talk <- reactive({
      if (input$cross_talk_correction) results_CT else NULL
    })

    ## Calibration data
    observeEvent(input$select, {
      req(input$select)

      ## Load calibration data:
      ## * 'results_CT'
      ## * 'results_ITC'
      ## * 'sourceDR_FINAL' ('DR', 'DR_ERROR', 'UNIT', 'NUMBER_ALQ', 'CAL_DATE')
      selected <- grep(pattern = input$select, x = calibration_data, fixed = TRUE)
      load(calibration_data[[selected]], envir = current_env)

      ## Show applied dose rate
      if (!is.null(sourceDR_FINAL)) {
        DR_today <- Luminescence::calc_SourceDoseRate(
          measurement.date = Sys.Date(),
          calib.date = sourceDR_FINAL[["CAL_DATE"]],
          calib.dose.rate = sourceDR_FINAL[[1]],
          calib.error = sourceDR_FINAL[[2]]
        )
        DR_today <- DR_today$dose.rate

        output$source_DR_FINAL <- renderText({
          sprintf(
            "(%s: %f ± %f %s)",
            sourceDR_FINAL[["CAL_DATE"]],
            round(sourceDR_FINAL[[1]], 2),
            round(sourceDR_FINAL[[2]], 2),
            sourceDR_FINAL[[3]]
          )
        })
        output$source_DR_FINAL_today <- renderText({
          sprintf(
            "DR today: %f ± %f µGy/s",
            round(DR_today[[1]], 2),
            round(DR_today[[2]], 2)
          )
        })
      } else {
        output$source_DR_FINAL <- renderText({"(NA ± NA µGy/s)"})
        output$source_DR_FINAL_today <- renderText({"NA ± NA µGy/s"})
      }
    })

    ## Run analysis for the plots (with progress bar)
    plot_files <- eventReactive(input$run, {
      n <- sum(included())

      signal <- seq(min(input$signal_integral), max(input$signal_integral), 1)
      withProgress({
        temp_files <- file.path(tempdir(), paste0("ALQ_", seq_len(n), ".png"))
        for (i in seq_len(n)) {
          incProgress(i)

          grDevices::png(
            file = temp_files[[i]],
            bg = "transparent",
            width = 700,
            height = 400
          )
          Luminescence::analyse_Al2O3C_Measurement(
            object = included_data()[[i]],
            signal_integral = signal,
            irradiation_time_correction = results_ITC,
            cross_talk_correction = cross_talk(),
            plot = TRUE,
            verbose = FALSE,
            norm = input$normalize_curve # DO NOT normalize curves by default
          )
          grDevices::dev.off()
        }
      }, message = "Analysing data...", min = 0, max = n)

      temp_files
    })

    ## Run analysis again (otherwise the data are not treated correctly)
    results <- eventReactive(input$run, {
      signal <- seq(min(input$signal_integral), max(input$signal_integral), 1)

      ## Warning handling taken from https://github.com/daattali/advanced-shiny/blob/master/show-warnings-messages/app.R
      travel_position <- included_samples()$POSITION[travel()]
      if (length(travel()) == 0 || !input$travel_dosimeters) travel_position <- NULL

      results <- withCallingHandlers({
        shinyjs::html(id = "warnings", html = "")
        Luminescence::analyse_Al2O3C_Measurement(
          object = included_data(),
          travel_dosimeter = travel_position,
          signal_integral = signal,
          irradiation_time_correction = results_ITC,
          cross_talk_correction = cross_talk(),
          plot = FALSE,
          verbose = FALSE
        )
      },
      warning = function(m) {
        shinyjs::html(id = "warnings", html = paste(m$message, "\n"), add = TRUE)
      })

      results
    })

    observe({
      ## Get data
      results <- results()@data

      ## Create data.frame
      n <- sum(included())
      df <- cbind(
        ALQ = seq_len(n),
        included_samples(),
        REJECT = FALSE,
        results$data[, c(1, 2)] # Add 'DE' and 'DE_ERROR' columns
      )

      ## Correct for the travel dosimeter
      if (!is.null(results$data_TDcorrected)) {
        df[-travel(), c(9, 10)] <- round(results$data_TDcorrected[, c(1, 2)], 4)
      }

      ## Keep only thickness for retained chips
      thick <- included_samples()$THICKNESS
      ## Set correction factors for the source dose rate
      fthick <- rep(1, length(thick))
      fthick[thick == "thin"] <- 1.175
      ## Adjust the dose rate
      df$DE_CORR <- df$DE * fthick
      df$DE_CORR_ERROR <- df$DE_ERROR * fthick

      user_data$dose <- df
    })

    ## Update data
    observeEvent(input$analysis_results, {
      user_data$dose <- rhandsontable::hot_to_r(input$analysis_results)
    })

    ## Render table
    output$analysis_results <- rhandsontable::renderRHandsontable({
      req(user_data$dose)

      hot <- rhandsontable::rhandsontable(
        user_data$dose,
        readOnly = TRUE,
        selectCallback = TRUE
      )
      hot <- rhandsontable::hot_context_menu(
        hot,
        allowRowEdit = FALSE,
        allowColEdit = FALSE,
        customOpts = list(
          csv = list(
            name = "Download to CSV",
            callback = widget_export_csv("data_analysis.csv")
          )
        )
      )
      hot <- rhandsontable::hot_table(
        hot,
        highlightCol = TRUE,
        highlightRow = TRUE,
        allowRowEdit = FALSE
      )
      hot <- rhandsontable::hot_cols(hot, columnSorting = TRUE)
      hot <- rhandsontable::hot_col(hot, col = "REJECT", readOnly = FALSE)
      hot
    })

    ## Render download button
    output$export_analysis <- renderUI({
      req(user_data$dose)

      ns <- session$ns
      tagList(
        tags$hr(),
        helpText("Download combined results as returned by the R function analyse_Al2O3C_Measurements()."),
        downloadButton(
          outputId = ns("download_analysis"),
          label = "Download results"
        )
      )
    })

    ## Download handler for results
    output$download_analysis <- downloadHandler(
      filename = paste0(Sys.Date(), "_Al2O3_Analysis_Results.zip"),
      content = function(file) {
        tmpdir <- tempdir()

        ## CSV file
        fs <- vapply(
          X = names(results()),
          FUN = function(f) {
            fs <- file.path(tmpdir, paste0(f, ".csv"))
            utils::write.table(
              x = results()@data[[f]],
              file = fs,
              sep = ";",
              row.names = FALSE
            )
            return(fs)
          },
          FUN.VALUE = character(1)
        )

        ## HTML report
        if (requireNamespace("pander", quietly = TRUE)) {
          fs <- c(fs, file.path(tmpdir, "RLum_Report.html"))
          Luminescence::report_RLum(
            results(),
            title = "RLum-Results report Al2O3:C",
            file = file.path(tmpdir,"RLum_Report"),
            launch.browser = FALSE,
            timestamp = FALSE,
            show_report = FALSE,
            quiet = TRUE,
            clean = TRUE
          )
        }

        ## Pictures
        fs <- c(fs, unlist(plot_files()))

        ## Create ZIP-file
        utils::zip(zipfile = file, files = fs, flags = "-j")
      },
      contentType = "application/zip"
    )

    ## Show first graphic (otherwise it remains empty here, which is odd)
    output$analysis_plot <- renderImage({
      filename <- plot_files()[[1]]

      ## Return a list containing the filename and alt text
      list(
        src = filename,
        alt = paste("Image number", input$analysis_results$select$rAll[1])
      )
    }, deleteFile = FALSE)

    ## Provide graphical output environment for plot output
    observeEvent(input$analysis_results_select, {
      output$analysis_plot <- renderImage({
        ## Grep correct aliquot; based on the row number
        i <- input$analysis_results_select$select$rAll
        temp_aliquot <- paste0("ALQ_", user_data$dose$ALQ[i], ".png")

        ## Set filename
        filename <- plot_files()[[grep(pattern = temp_aliquot, x = plot_files(), fixed = TRUE)]]

        ## Return a list containing the filename and alt text
        list(src = filename, alt = paste("Image number", i, sep = " "))
      }, deleteFile = FALSE)
    })
  })
}
