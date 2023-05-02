#' Post-Processing Server
#'
#' @param input provided by \pkg{Shiny}.
#' @param output provided by \pkg{Shiny}.
#' @param session provided by \pkg{Shiny}.
#' @param user_data a [shiny::reactiveValues()] list with the
#'  following elements: "`data`", "`info`" and "`structure`".
#' @param user_settings a [shiny::reactiveValues()] list.
#' @seealso [module_analysis_ui()]
#' @family modules
#' @keywords internal
#' @export
module_processing_server <- function(id, user_data, user_settings) {
  moduleServer(id, function(input, output, session) {

    ## Remove all rejected values in the 'Analysis' tab
    rejected <- reactive({
      req(user_data$dose)
      user_data$dose$REJECT
    })
    keep_dose <- reactive({
      req(user_data$dose)
      user_data$dose[!rejected(), , drop = FALSE]
    })

    ## Add infotext
    output$post_processing_info <- renderText({
      validate(need(user_data$dose, "No data to aggregate!"))
      "Sample summary | Source dose rate re-calculated to measurement date."
    })

    output$travel_correction_info <- renderText({
      req(user_data$travel_correction)
      sprintf("Travel dosimeter correction: %g +/- %g s.",
              round(user_data$travel_correction[[1L]], 3),
              round(user_data$travel_correction[[2L]], 3))
    })

    ## Group statistics
    combined <- reactive({
      ## Split by sample ID
      splited <- split(keep_dose(), f = keep_dose()$SAMPLE_ID)

      ## Error weighted mean for each position
      combined <- lapply(
        X = splited,
        FUN = function(x) {
          x <- x[, c("DE_CORR", "DE_CORR_ERROR")]
          if (anyNA(x)) {
            c(NA_real_, NA_real_)
          } else {
            stat <- Luminescence::calc_Statistics(x, n.MCM = 1000)
            unlist(stat[["MCM"]][c("mean", "sd.abs")])
          }
        }
      )

      ## Reset column names
      combined <- do.call(rbind.data.frame, combined)
      colnames(combined) <- c("MEAN", "SD")

      ## Add sample ID
      combined <- data.frame(
        SAMPLE_ID = names(splited),
        N = vapply(X = splited, FUN = nrow, FUN.VALUE = integer(1)),
        combined,
        stringsAsFactors = FALSE
      )

      ## Calculate relative error
      combined$CV <- abs(combined$SD / combined$MEAN * 100)

      ## Translate to µGy
      start_date <- strtrim(user_data$info$startDate[1], 8)
      start_date <- as.Date(start_date, format = "%Y%m%d")
      if (!is.null(sourceDR_FINAL)) {

        source_dose_rate <- Luminescence::calc_SourceDoseRate(
          measurement.date = start_date,
          calib.date = as.Date(sourceDR_FINAL$CAL_DATE),
          calib.dose.rate = c(sourceDR_FINAL$DR),
          calib.error = c(sourceDR_FINAL$DR_ERROR)
        )
        src_dose <- source_dose_rate$dose.rate[[1]]
        src_dose_err <- source_dose_rate$dose.rate[[2]]
      } else {
        src_dose <- 1
        src_dose_err <- 0
      }

      ## Combine
      combined$MEASUREMENT_DATE <- start_date
      combined$SOURCE_DR <- src_dose
      combined$SOURCE_DR.ERROR <- src_dose_err
      combined$DOSE <- combined$MEAN * src_dose
      combined$DOSE.ERROR <- combined$SD * src_dose

      ## Make sure that the sample headers are OK
      colnames(combined) <- c(
        "SAMPLE_ID", "N", "SAMPLE MEAN \n [s]", "SAMPLE SD \n [s]", "CV \n [%]",
        "DATE \n MEASUREMENT", "SOURCE_DR \n [µGy/s]", "SOURCE_DR.ERROR \n [µGy/s]",
        "DOSE \n [µGy]", "DOSE.ERROR \n [µGy]"
      )

      ## Fix row names
      # rownames(combined) <- combined$SAMPLE_ID

      combined
    })

    observe({
      ## Add columns if they do not yet exist
      final <- data.frame(
        SAMPLE_ID = combined()$SAMPLE_ID,
        DATE_IN = Sys.Date(),
        DATE_OUT = Sys.Date(),
        DURATION = 0,
        COSMIC_DR = 0,
        COSMIC_DR.ERROR = 0,
        COSMIC_DOSE = 0,
        COSMIC_DOSE.ERROR = 0,
        ATTENUATION_CORR = 1.068,
        DOSE_CORR = 0,
        DOSE_CORR.ERROR = 0,
        DR = 0,
        DR.ERROR = 0,
        GAMMA = 0,
        GAMMA.ERROR = 0,
        GAMMA.ERROR_REL = 0
      )

      ## Fix colnames
      colnames(final) <- c(
        "SAMPLE_ID", "DATE_IN", "DATE_OUT",
        "DURATION \n [days]", "COSMIC_DR \n [µGy/a]", "COSMIC_DR.ERROR \n [µGy/a]",
        "COSMIC_DOSE \n [µGy]", "COSMIC_DOSE.ERROR \n [µGy]", "TUBE ATTENUATION \n CORRECTION FACTOR",
        "DOSE_CORR \n [µGy]", "DOSE_CORR.ERROR \n [µGy]", "FINAL DR \n [µGy/a]", "FINAL DR.ERROR \n [µGy/a]",
        "FINAL GAMMA_DR \n [µGy/a]", "FINAL GAMMA_DR.ERROR \n [µGy/a]", "FINAL GAMMA_DR.ERROR \n [%]"
      )

      user_data$final <- final
    })

    ## Render table of combined statistics
    output$post_processing_combined <- rhandsontable::renderRHandsontable({
      hot <- rhandsontable::rhandsontable(combined(), readOnly = TRUE)
      hot <- rhandsontable::hot_table(
        hot,
        allowRowEdit = FALSE,
        highlightCol = TRUE,
        highlightRow = TRUE
      )
      hot <- rhandsontable::hot_context_menu(
        hot,
        allowRowEdit = FALSE,
        allowColEdit = FALSE,
        customOpts = list(
          csv = list(
            name = "Download to CSV",
            callback = widget_export_csv("data_summary.csv")
          )
        )
      )
      hot <- rhandsontable::hot_cols(hot, columnSorting = FALSE)
    })

    ## Render table of final values
    output$post_processing_final <- rhandsontable::renderRHandsontable({
      req(user_data$final)

      hot <- rhandsontable::rhandsontable(
        user_data$final,
        readOnly = TRUE,
        selectCallback = TRUE
      )
      hot <- rhandsontable::hot_table(
        hot,
        allowRowEdit = FALSE,
        highlightCol = TRUE,
        highlightRow = TRUE
      )
      hot <- rhandsontable::hot_context_menu(
        hot,
        allowRowEdit = FALSE,
        allowColEdit = FALSE,
        customOpts = list(
          csv = list(
            name = "Download to CSV",
            callback = widget_export_csv("data_final.csv")
          )
        )
      )
      hot <- rhandsontable::hot_col(hot, col = "DATE_IN", readOnly = FALSE)
      hot <- rhandsontable::hot_col(hot, col = "DATE_OUT", readOnly = FALSE)
      hot <- rhandsontable::hot_col(hot, col = "COSMIC_DR \n [µGy/a]", readOnly = FALSE)
      hot <- rhandsontable::hot_col(hot, col = "COSMIC_DR.ERROR \n [µGy/a]", readOnly = FALSE)
      hot <- rhandsontable::hot_col(hot, col = "TUBE ATTENUATION \n CORRECTION FACTOR", readOnly = FALSE)
      hot <- rhandsontable::hot_cols(hot, columnSorting = FALSE)
    })

    ## Render boxplot
    output$post_processing_boxplot <- renderPlot(
      {
        n <- length(combined()[["SAMPLE_ID"]])
        x <- seq_len(n)
        graphics::plot(
          x = x,
          y = combined()[["DOSE \n [µGy]"]],
          xlab = "", ylab = "DOSE [µGy]",
          type = "p", pch = 16, col = "black", las = 1,
          main = "Totally Absorbed Dose"
        )
        graphics::segments(
          x0 = x, x1 = x,
          y0 = combined()[["DOSE \n [µGy]"]] - combined()[["DOSE.ERROR \n [µGy]"]],
          y1 = combined()[["DOSE \n [µGy]"]] + combined()[["DOSE.ERROR \n [µGy]"]],
          lty = 1, col = "black"
        )
      },
      bg = "transparent"
    )

    ## Update data
    observeEvent(input$post_processing_final, {
      tmp <- rhandsontable::hot_to_r(input$post_processing_final)

      ## Update DURATION
      tmp[["DURATION \n [days]"]] <-  as.integer(tmp$DATE_OUT - tmp$DATE_IN)

      ## Update COSMIC_DOSE
      tmp[["COSMIC_DOSE \n [µGy]"]] <- (tmp[["COSMIC_DR \n [µGy/a]"]] * tmp[["DURATION \n [days]"]]) / 365.25
      tmp[["COSMIC_DOSE.ERROR \n [µGy]"]] <- tmp[["COSMIC_DOSE \n [µGy]"]] * tmp[["COSMIC_DR.ERROR \n [µGy/a]"]] / tmp[["COSMIC_DR \n [µGy/a]"]]

      ## Prevent division by 0
      tmp[["COSMIC_DOSE.ERROR \n [µGy]"]][!is.finite(tmp[["COSMIC_DOSE.ERROR \n [µGy]"]])] <- 0

      ## Update DOSE based on the given cosmic dose and the attenuation factor
      tmp[["DOSE_CORR \n [µGy]"]] <- ((combined()[["DOSE \n [µGy]"]] - tmp[["COSMIC_DOSE \n [µGy]"]]) * tmp[["TUBE ATTENUATION \n CORRECTION FACTOR"]]) + tmp[["COSMIC_DOSE \n [µGy]"]]
      tmp[["DOSE_CORR.ERROR \n [µGy]"]] <- sqrt((tmp[["TUBE ATTENUATION \n CORRECTION FACTOR"]] * combined()[["DOSE.ERROR \n [µGy]"]])^2 + ((-tmp[["TUBE ATTENUATION \n CORRECTION FACTOR"]] + 1) * tmp[["COSMIC_DOSE.ERROR \n [µGy]"]])^2)

      ## Update DR and DR.ERROR
      tmp[["FINAL DR \n [µGy/a]"]] <- tmp[["DOSE_CORR \n [µGy]"]] * 365.25 / tmp[["DURATION \n [days]"]]
      tmp[["FINAL DR.ERROR \n [µGy/a]" ]] <- tmp[["FINAL DR \n [µGy/a]"]] * tmp[["DOSE_CORR.ERROR \n [µGy]"]] /  tmp[["DOSE_CORR \n [µGy]"]]

      ## Replace all Inf values with 0
      tmp[["FINAL DR \n [µGy/a]"]][!is.finite(tmp[["FINAL DR \n [µGy/a]"]])] <- 0
      tmp[["FINAL DR.ERROR \n [µGy/a]" ]][!is.finite(tmp[["FINAL DR.ERROR \n [µGy/a]"]])] <- 0

      ## Calculate final gamma dose rate
      tmp[["FINAL GAMMA_DR \n [µGy/a]"]] <- tmp[["FINAL DR \n [µGy/a]"]] - tmp[["COSMIC_DR \n [µGy/a]"]]
      tmp[["FINAL GAMMA_DR.ERROR \n [µGy/a]"]] <- sqrt(tmp[["FINAL DR.ERROR \n [µGy/a]"]]^2 + tmp[["COSMIC_DR.ERROR \n [µGy/a]"]]^2)
      tmp[["FINAL GAMMA_DR.ERROR \n [%]"]] <- abs(tmp[["FINAL GAMMA_DR.ERROR \n [µGy/a]"]] / tmp[["FINAL GAMMA_DR \n [µGy/a]"]] * 100)

      user_data$final <- tmp
    })
  })
}
