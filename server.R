## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Title:   Al2O3:C Analysis App
## Authors: Sebastian Kreutzer, IRAMAT-CRP2A, Université Bordeaux Montaigne (France)
## Contact: sebastian.kreutzer@u-bordeaux-montainge.fr
## Initial date: 2018-06-07
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
shinyServer(function(input, output, session) {

# PANEL IMPORT------------------------------------------------------------------------------------

  ##=============================##
  ##import data
  ##=============================##
   observeEvent(input$file_data, {
      .import_file(
        file = as.list(input$file_data$datapath),
        name = input$file_data$name,
        import_file_names_assignment = input$import_file_names_assignment
        )

   })

  ##=============================##
  ##import example data
  ##=============================##
  observeEvent(input$file_data_example, {
    .import_file(file = list.files("example_data/", full.names = TRUE), name = "Example data")

  })

  ##=============================##
  ##initial event of loading data
  ##=============================##
  observeEvent(c(
    input$file_data_example,
    input$file_data), {

    if(!is.null(file_data)){
    ##set tabs
    output$tabs <- renderUI({

      ##get number of needed wheels
      wheels <- length(unique(as.character(file_info[["wheels"]])))

      ##set number of needed tabs
      Tabs <- lapply(1:wheels, function(x){
        tabPanel(title = paste("Wheel",x), value = paste0("wheel",x))})

      ##create call for the frontend
      do.call(tabsetPanel, c(Tabs, id = "wheels"))

      })
    }

  })

  ##=============================##
  ##table - initial event
  ##=============================##
  observeEvent(c(
    input$file_data_example,
    input$file_data),{

    if(!is.null(file_data)){
      ##initialise sample
      sample_info_full <<- reactiveValues(
        data = data.frame(
          FILENAME = file_info$name,
          WHEEL = as.character(file_info[["wheels"]]),
          POSITION = as.integer(file_info$position),
          SAMPLE_ID = sprintf("Sample %02d", as.numeric(file_info$position)),
          TYPE = dosimeter_type[1],
          INCLUDE = verify,
          stringsAsFactors = FALSE
        ))

      ##create sample_info output
      output$sample_info <- renderRHandsontable({
        rhandsontable(data = sample_info_full$data[which(file_info[["wheels"]] == "wheel1"), ]) %>%
          hot_context_menu(
            allowRowEdit = FALSE,
            allowColEdit = FALSE) %>%
          hot_table(highlightCol = TRUE, highlightRow = TRUE, allowRowEdit = FALSE)  %>%
          hot_col("FILENAME", readOnly = TRUE) %>%
          hot_col("WHEEL", readOnly = TRUE) %>%
          hot_col("POSITION", readOnly = TRUE) %>%
          hot_col("TYPE", type = "dropdown", source = dosimeter_type)

      })

    }

  })

  ##=============================##
  ##table - select tabs
  ##=============================##
  observeEvent(input$wheels, {
  output$sample_info <- renderRHandsontable({
    rhandsontable(data = sample_info_full$data[which(file_info[["wheels"]] == input$wheels), ]) %>%
      hot_context_menu(
        allowRowEdit = FALSE,
        allowColEdit = FALSE) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE, allowRowEdit = FALSE)  %>%
      hot_col("FILENAME", readOnly = TRUE) %>%
      hot_col("WHEEL", readOnly = TRUE) %>%
      hot_col("POSITION", readOnly = TRUE) %>%
      hot_col("TYPE", type = "dropdown", source = dosimeter_type)

    })

  })


  ##=============================##
  ##table - updated event
  ##=============================##
  observe({
    if(!is.null(input$sample_info)){

      ##create hash from row names
      hashA <- sum(as.numeric(row.names(sample_info_full$data[which(file_info[["wheels"]] == input$wheels), ])))
      hashB <- sum(as.numeric(row.names(hot_to_r(input$sample_info))))

      ##update table values if rownumbers match, otherwise we overwrite
      if(hashA == hashB){
        sample_info_full$data[which(file_info[["wheels"]] == input$wheels), ] <<- hot_to_r(input$sample_info)

      ##update plot
      output$carousel <- renderPlot({
        .plot_carousel(positions = as.numeric(file_info$position),
          included = sample_info_full$data[["INCLUDE"]][which(file_info[["wheels"]] == input$wheels)],
          wheel = input$wheels
          )}, height = 320, width = 320, bg="transparent"
       )
      }

      if(!any(sample_info_full$data[["INCLUDE"]])){
        showModal(modalDialog(
          title = "Important message",
          "Smart move, nothing included, nothing can go wrong...",
          footer = modalButton("Ok, I'll try to include at least one aliquot.")
        ))

      }

    }## end hash

  })


  # PANEL Analyse ------------------------------------------------------------------------------

  ##=============================##
  ##Calibration dataset selection
  ##=============================##
  observeEvent(input$calibration_data,{
    ##load calibration dataset
    if(!"Own dataset loaded" %in% unlist(input$calibration_data) && input$calibration_data != "not available" ){
    load(calibration_data[[grep(pattern = input$calibration_data,
                                x = calibration_data,
                                fixed = TRUE)]], envir = current_env)
    }

    ##show applied dose rate
    if(!is.null(sourceDR_FINAL)){
      DR_today <- calc_SourceDoseRate(
        measurement.date = Sys.Date(),
        calib.date = sourceDR_FINAL[["CAL_DATE"]],
        calib.dose.rate = sourceDR_FINAL[[1]],
        calib.error = sourceDR_FINAL[[2]])$dose.rate

      ##render text
      output$sourceDR_FINAL <- renderText({
          paste0("(", sourceDR_FINAL[["CAL_DATE"]],": ",round(sourceDR_FINAL[[1]],2), " ± ", round(sourceDR_FINAL[[2]],2)," ",
                sourceDR_FINAL[[3]],")")
      })

      output$sourceDR_FINAL_today <- renderText({
        paste0("DR today: ", round(DR_today[[1]],2), " ± ",  round(DR_today[[2]],2), " µGy/s")

      })

    }else{
      output$sourceDR_FINAL <- renderText({"NA ± NA"})

    }

  })

  ##=============================##
  ##run analysis
  ##=============================##
  observeEvent(input$Analysis.run, {
      #preset error message
      output$analysis_error <- renderText(NULL)

      ##make sure that the app does not crash
      if(!is.null(file_data) && any(sample_info_full$data[["INCLUDE"]])){

        ##remove all values previously deselected
        file_data[!sample_info_full$data[["INCLUDE"]]] <- NULL

        ##identify travel dosimeters (with the position number; just in case)
        travel_dosimeters <-  which(
          sample_info_full$data[["TYPE"]][sample_info_full$data[["INCLUDE"]]] == 'travel')

        if(length(travel_dosimeters) == 0 || !input$settings_travel_dosimeter)
          travel_dosimeters <- NULL

        ##initialise values
        temp_dir <- tempdir()

        ##RUN ANALYSIS
        ##with progress bar
        withProgress(
          message = "Analysing data ...", min = 0, max = length(file_data), {

          ##run analysis for the plots
          for(i in 1:length(file_data)){
            incProgress(i)
            temp_files[[i]] <<- paste0(temp_dir,"/ALQ_",i,".png")
            png(file = temp_files[[i]], bg = "transparent", width = 700, height = 400)
              Luminescence::analyse_Al2O3C_Measurement(
                object = file_data[[i]],
                signal_integral = min(input$settings_signal_integral):max(input$settings_signal_integral),
                irradiation_time_correction = results_ITC,
                cross_talk_correction = if(input$settings_cross_talk_correction){
                  results_CT
                }else{
                  NULL
                },
                plot = TRUE,
                verbose = FALSE
                )
            dev.off()
          }

        ##run again (otherwise the data are not treated correctly)
        ##warning handling taken from https://github.com/daattali/advanced-shiny/blob/master/show-warnings-messages/app.R
        results <<- withCallingHandlers({
          shinyjs::html(id = "warnings", html = "")
          analyse_Al2O3C_Measurement(
            object = file_data,
            travel_dosimeter = if(!is.null(travel_dosimeters)){
              sample_info_full$data[["POSITION"]][sample_info_full$data[["INCLUDE"]]][travel_dosimeters]
              } else {NULL},
            signal_integral = min(input$settings_signal_integral):max(input$settings_signal_integral),
            irradiation_time_correction = results_ITC,
            cross_talk_correction = if(input$settings_cross_talk_correction){
              results_CT
            }else{
              NULL
            },
            plot = FALSE,
            verbose = FALSE)
        },
        warning = function(m) {
          shinyjs::html(id = "warnings", html = paste(m$message, "\n"), add = TRUE)
        })

      })#end progressbar

      ##create data.frame
      df <<- cbind(
          ALQ = 1:length(sample_info_full$data[sample_info_full$data[["INCLUDE"]],1]),
          sample_info_full$data[sample_info_full$data[["INCLUDE"]],-c(6,7)],
          REJECT = FALSE,
          results@data$data[,c(1,2)])


      ##correct for the travel dosimeter
      if(!is.null(results@data[["data_TDcorrected"]])){
        df[-travel_dosimeters,8:9] <<- round(results@data[["data_TDcorrected"]][,1:2],4)

      }

      ##make df reactive
      df_reactive <<- reactiveValues(data = df)

      ##render handsontable
      output$analysis_results <- renderRHandsontable({
          rhandsontable(data = df_reactive$data, readOnly = TRUE, selectCallback = TRUE) %>%
            hot_context_menu(
              allowRowEdit = FALSE,
              allowColEdit = FALSE,
              customOpts = list(
                csv = list(name = "Download to CSV",
                           callback = htmlwidgets::JS(
                             "function (key, options) {
                         var csv = csvString(this, sep=',', dec='.');

                         var link = document.createElement('a');
                         link.setAttribute('href', 'data:text/plain;charset=utf-8,' +
                           encodeURIComponent(csv));
                         link.setAttribute('download', 'data.csv');

                         document.body.appendChild(link);
                         link.click();
                         document.body.removeChild(link);
                       }")))) %>%
            hot_table(highlightCol = TRUE, highlightRow = TRUE, allowRowEdit = FALSE) %>%
            hot_cols(columnSorting = TRUE) %>%
            hot_col("REJECT", readOnly = FALSE)


        })

        ##add infotext
        output$analysis_table_info_text <- renderText(
          "Note: Dose values are here listed in seconds, not µGy! To reject data permanently,
          go back to the 'Import' panel")

        ##show first graphic (otherwise it remains empty here, which is odd)
        output$analysis_results.plot <- renderImage({
          filename <- temp_files[[1]]

          #Return a list containing the filename and alt text
          list(src = filename,
               alt = paste("Image number", input$analysis_results_select$select$rAll[1]))
        }, deleteFile = FALSE)

        ##add download button if results are available
        output$export_analysis_results <- renderUI({
          if(length(results) > 0)
            downloadButton(
              outputId = "download_analysis_results",
              label = "Download results")

        })

        ##download handler for results
        output$download_analysis_results <- downloadHandler(
           filename = paste0(Sys.Date(),"_Al2O3_Analysis_Results.zip"),
              content = function(file){
                  temp_results <- results
                  tmpdir <- tempdir()
                  fs <- vapply(names(temp_results), function(f){
                    fs <- paste0(tmpdir,"/",f,".csv")
                    write.table(
                      x = temp_results@data[[f]],
                      file = fs,
                      sep = ";",
                      row.names = FALSE
                    )
                    return(fs)

                  }, character(1))

                  ##add HTML report
                  fs <- c(fs, paste0(tmpdir,"/RLum_Report.html"), unlist(temp_files))
                  report_RLum(
                    results,
                    title = "RLum-Results report Al2O3:C",
                    file = paste0(tmpdir,"/RLum_Report"),
                    launch.browser = FALSE,
                    timestamp = FALSE,
                    show_report = FALSE,
                    quiet = TRUE,
                    clean = TRUE
                    )

                  ##create ZIP-file
                  zip(zipfile = file, files = fs, flags = "-j")
                },
                contentType = "application/zip"
            )

      }else{
        output$analysis_error <- renderText("Error: No file imported!")

      }

   })#run analysis tab

   #observe selection change in table
   observe({
    if(!is.null(input$analysis_results)){
      df_reactive$data <<- hot_to_r(input$analysis_results)

     }
    })

   ##provide graphical output environment for plot output
   observeEvent(input$analysis_results_select, {
     output$analysis_results.plot <- renderImage({

       ##grep correct aliquot; based on the row number
       temp_aliquot <- paste0(
         "ALQ_",df[["ALQ"]][input$analysis_results_select$select$rAll[1]],".png")

       ##set filename
       filename <- temp_files[[grep(pattern = temp_aliquot, x = temp_files,fixed = TRUE)]]

        #Return a list containing the filename and alt text
        list(src = filename,
             alt = paste("Image number", temp_aliquot))

    }, deleteFile = FALSE)

   })


  # PANEL Post-processing -------------------------------------------------------------------------
  observeEvent(input$navbar,{

    if(input$navbar == "post_processing_run"){
     ##reset error message
     output$post_processing_error <- renderText(NULL)

     if(!is.null(df_reactive$data)){
       ##add infotext
       output$post_processing_table_info_text <- renderText(
         "Sample summary | Source dose rate re-calculated to measurement date.")

       ##group by sample ID
       df_grouped <- dlply(
         df_reactive$data[!df_reactive$data[["REJECT"]],], .variables = "SAMPLE_ID", .fun = identity)

       ##error weighted mean for each position
       df_combined <- t(vapply(1:length(df_grouped), function(x){
         if(any(is.na(df_grouped[[x]][,c("DE","DE_ERROR")]))){
           c(NA_real_,NA_real_)

         }else{
         unlist(calc_Statistics(
           df_grouped[[x]][,c("DE","DE_ERROR")], n.MCM = 1000)[["MCM"]][c("mean", "sd.abs")])
        }
       }, FUN.VALUE = numeric(length = 2)))

       ##reset column names
       colnames(df_combined) <- c("MEAN", "SD")

       ##add sample ID
       df_grouped <-
         data.frame(SAMPLE_ID = attributes(df_grouped)$names,
                    N = vapply(df_grouped, nrow, integer(1)),
                    df_combined,
                    stringsAsFactors = FALSE)

       ##calculate relative error
       df_grouped <- cbind(df_grouped, CV = abs(df_grouped[["SD"]]/df_grouped[["MEAN"]] * 100))

       # ##translate to µGy
       if(!is.null(sourceDR_FINAL)){
         source_dose_rate <- calc_SourceDoseRate(
           measurement.date = as.Date(strtrim(file_info$startDate[1],8), format = "%Y%m%d"),
           calib.date = as.Date(sourceDR_FINAL$CAL_DATE),
           calib.dose.rate = c(sourceDR_FINAL$DR),
           calib.error = c(sourceDR_FINAL$DR_ERROR)
         )$dose.rate

       }else{
         source_dose_rate <- data.frame(x = 1, y = 0)

       }

      ##combine
      results_final <<- reactiveValues(data = cbind(
           df_grouped,
           MEASUREMENT_DATE = as.Date(strtrim(file_info$startDate[1],8), format = "%Y%m%d"),
           SOURCE_DR = source_dose_rate[[1]],
           SOURCE_DR.ERROR = source_dose_rate[[2]],
           DOSE = df_grouped[["MEAN"]] * source_dose_rate[,1],
           DOSE.ERROR = df_grouped[["SD"]] * source_dose_rate[,1]
         ))

         ##add columns if they do not yet exist
         if(!("DURATION" %in% colnames(results_final$data))){
           results_final$data <- cbind(
             results_final$data,
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

         }

        ##make sure that the sample headers are ok
        colnames(results_final$data) <- c(
          "SAMPLE_ID", "N", "SAMPLE MEAN \n [s]", "SAMPLE SD \n [s]", "CV \n [%]", "DATE \n MEASUREMENT",
          "SOURCE_DR \n [µGy/s]", "SOURCE_DR.ERROR \n [µGy/s]", "DOSE \n [µGy]", "DOSE.ERROR \n [µGy]",
          "DATE_IN", "DATE_OUT", "DURATION \n [days]", "COSMIC_DR \n [µGy/a]", "COSMIC_DR.ERROR \n [µGy/a]",
          "COSMIC_DOSE \n [µGy]", "COSMIC_DOSE.ERROR \n [µGy]", "TUBE ATTENUATION \n CORRECTION FACTOR",
          "DOSE_CORR \n [µGy]", "DOSE_CORR.ERROR \n [µGy]", "FINAL DR \n [µGy/a]", "FINAL DR.ERROR \n [µGy/a]",
          "FINAL GAMMA_DR \n [µGy/a]", "FINAL GAMMA_DR.ERROR \n [µGy/a]", "FINAL GAMMA_DR.ERROR \n [%]"
          )

       ##create output plot
       ##boxplot
       output$postprocessing_boxplot <- renderPlot({
       ggplot(data = results_final$data,
              aes(x = SAMPLE_ID, y = `DOSE \n [µGy]`, col = SAMPLE_ID)) +
         geom_point() +
         geom_segment(
           aes(
             x = SAMPLE_ID, xend = SAMPLE_ID,
             y = `DOSE \n [µGy]` - `DOSE.ERROR \n [µGy]`,
             yend = `DOSE \n [µGy]` + `DOSE.ERROR \n [µGy]`,
             col = SAMPLE_ID)) +
         ggtitle("Totally Absorbed Dose") +
         theme_gray(base_size = 14) +
         theme(axis.text.x = element_text(angle = 45, hjust = 1))
       }, width = 800)


       ##create table output
       output$postprocessing_results <- renderRHandsontable({
         rownames(results_final$data) <- results_final$data$SAMPLE_ID
         rhandsontable(data = results_final$data, readOnly = TRUE, selectCallback = TRUE, rowHeaderWidth = 150) %>%
           hot_context_menu(
             allowRowEdit = FALSE,
             allowColEdit = FALSE,
              customOpts = list(
                 csv = list(name = "Download to CSV",
                            callback = htmlwidgets::JS(
                              "function (key, options) {
                         var csv = csvString(this, sep=',', dec='.');
                         var link = document.createElement('a');
                         link.setAttribute('href', 'data:text/plain;charset=utf-8,' +
                           encodeURIComponent(csv));
                         link.setAttribute('download', 'Data_summary.csv');

                         document.body.appendChild(link);
                         link.click();
                         document.body.removeChild(link);
                       }")))) %>%
           hot_col("DATE_IN", readOnly = FALSE) %>%
           hot_col("DATE_OUT", readOnly = FALSE) %>%
           hot_col(col = 14, readOnly = FALSE) %>%
           hot_col(col = 15, readOnly = FALSE) %>%
           hot_col(col = 17, readOnly = FALSE) %>%
           hot_table(allowRowEdit = FALSE, highlightCol = TRUE, highlightRow = TRUE) %>%
           hot_cols(columnSorting = FALSE)

       })

     }else{
       output$post_processing_error <- renderText("Error: No data to aggregate!")

     }
    }#end if navbar selection

   })#observeEvent Post-processing

   ##monitor post-processing table
   observe({
     if(!is.null(input$postprocessing_results)){
       results_final$data <- hot_to_r(input$postprocessing_results)

     }

   })

   ##update post-processing table
   observeEvent(input$post_processing_update, {

     ##update DURATION
     results_final$data[["DURATION \n [days]"]] <-  as.integer(
       results_final$data[["DATE_OUT"]] - results_final$data[["DATE_IN"]])

     #update COSMIC_DOSE
     results_final$data[["COSMIC_DOSE \n [µGy]"]] <- (results_final$data[["COSMIC_DR \n [µGy/a]"]] * results_final$data[["DURATION \n [days]"]])/365.25
     results_final$data[["COSMIC_DOSE.ERROR \n [µGy]"]] <- (results_final$data[["COSMIC_DR \n [µGy/a]"]] * results_final$data[["DURATION \n [days]"]])/365.25 *
       results_final$data[["COSMIC_DR.ERROR \n [µGy/a]"]] / results_final$data[["COSMIC_DR \n [µGy/a]"]]

     ##update DOSE based on the given cosmic dose and the attenuation factor
     results_final$data[["DOSE_CORR \n [µGy]"]] <- ((results_final$data[["DOSE \n [µGy]"]] - results_final$data[["COSMIC_DOSE \n [µGy]"]]) *
                                                      results_final$data[["TUBE ATTENUATION \n CORRECTION FACTOR"]]) + results_final$data[["COSMIC_DOSE \n [µGy]"]]


     results_final$data[["DOSE_CORR.ERROR \n [µGy]"]] <- sqrt((results_final$data[["TUBE ATTENUATION \n CORRECTION FACTOR"]] * results_final$data[["DOSE.ERROR \n [µGy]"]])^2 +
       ((-results_final$data[["TUBE ATTENUATION \n CORRECTION FACTOR"]] + 1) * results_final$data[["COSMIC_DOSE.ERROR \n [µGy]"]])^2)


     ##update DR and DR.ERROR
     results_final$data[["FINAL DR \n [µGy/a]"]] <- results_final$data[["DOSE_CORR \n [µGy]"]] * 365.25 / results_final$data[["DURATION \n [days]"]]
     results_final$data[["FINAL DR.ERROR \n [µGy/a]" ]] <- ((results_final$data[["DOSE_CORR \n [µGy]"]] * 365.25) /
                                                              as.numeric(results_final$data[["DURATION \n [days]"]])) *
        results_final$data[["DOSE_CORR.ERROR \n [µGy]"]] /  results_final$data[["DOSE_CORR \n [µGy]"]]


     ##Replace all Inf values with 0
     results_final$data[["FINAL DR \n [µGy/a]"]][is.infinite(results_final$data[["FINAL DR \n [µGy/a]"]])] <- 0
     results_final$data[["FINAL DR.ERROR \n [µGy/a]" ]][is.infinite(  results_final$data[["FINAL DR.ERROR \n [µGy/a]"]])] <- 0

     ##calculate final gamma dose rate
     results_final$data[["FINAL GAMMA_DR \n [µGy/a]"]] <- results_final$data[["FINAL DR \n [µGy/a]"]] - results_final$data[["COSMIC_DR \n [µGy/a]"]]
     results_final$data[["FINAL GAMMA_DR.ERROR \n [µGy/a]"]] <- sqrt(results_final$data[["FINAL DR.ERROR \n [µGy/a]"]]^2 + results_final$data[["COSMIC_DR.ERROR \n [µGy/a]"]]^2)
     results_final$data[["FINAL GAMMA_DR.ERROR \n [%]"]] <- abs(results_final$data[["FINAL GAMMA_DR.ERROR \n [µGy/a]"]] /
       results_final$data[["FINAL GAMMA_DR \n [µGy/a]"]] * 100)


     ##create table output
     output$postprocessing_results <- renderRHandsontable({
       rhandsontable(data = results_final$data, readOnly = TRUE, selectCallback = TRUE) %>%
         hot_context_menu(
           allowRowEdit = FALSE,
           allowColEdit = FALSE,
           customOpts = list(
             csv = list(name = "Download to CSV",
                        callback = htmlwidgets::JS(
                          "function (key, options) {
                         var csv = csvString(this, sep=',', dec='.');

                         var link = document.createElement('a');
                         link.setAttribute('href', 'data:text/plain;charset=utf-8,' +
                           encodeURIComponent(csv));
                         link.setAttribute('download', 'Data_summary.csv');

                         document.body.appendChild(link);
                         link.click();
                         document.body.removeChild(link);
                       }")))) %>%
         hot_col("DATE_IN", readOnly = FALSE) %>%
         hot_col("DATE_OUT", readOnly = FALSE) %>%
         hot_col(col = 14, readOnly = FALSE) %>%
         hot_col(col = 15, readOnly = FALSE) %>%
         hot_col(col = 17, readOnly = FALSE) %>%
         hot_table(allowRowEdit = FALSE, highlightCol = TRUE, highlightRow = TRUE) %>%
         hot_cols(columnSorting = FALSE)

    })

   })

   ##update ggplot with whatever we have to
   observeEvent(input$postprocessing_results_select, {
     df_hot <- as.data.frame(hot_to_r(input$postprocessing_results), stringsAsFactors = FALSE)
     ##set selection
     if(length(input$postprocessing_results_select$select$cAll) == 1){
       x_sel <- 1
       y_sel <- min(input$postprocessing_results_select$select$cAll)

     }else{
       x_sel <- min(input$postprocessing_results_select$select$cAll)
       y_sel <- max(input$postprocessing_results_select$select$cAll)
     }

     output$postprocessing_boxplot <- renderPlot({
       ggplot(data = df_hot,
              aes(
                x = df_hot[[x_sel]],
                y = df_hot[[y_sel]],
                col = SAMPLE_ID)) +
         geom_point() +
         xlab(colnames(df_hot)[x_sel]) +
         ylab(colnames(df_hot)[y_sel]) +
         ggtitle("Alternative plot") +
         theme_gray(base_size = 14) +
         theme(axis.text.x = element_text(angle = 45, hjust = 1))
     }, width = 800)



   })

  # PANEL Settings----- -------------------------------------------------------------------------
  ##download handler for calibration data
   output$download_CalibrationData <- downloadHandler(
     filename = "CalibrationDatasets.zip",
     content = function(file){

       ##create ZIP-file
       zip(zipfile = file, files = calibration_data, flags = "-j")
     },
     contentType = "application/zip"
   )

  ##upload own calibration dataset
  observeEvent(input$upload_calibrationdata, {

       ##inspect content
       load(input$upload_calibrationdata$datapath, safe <- new.env())

       if(length(ls(safe)) == 3 && all(c("results_CT", "results_ITC", "sourceDR_FINAL") %in% ls(safe))){
        #reset data
        results_CT <<- NULL
        results_ITC <<- NULL
        sourceDR_FINAL <<- NULL


       ##load data
       load(input$upload_calibrationdata$datapath, envir = current_env)

       ##correct input path
       updateSelectInput(
         session, "calibration_data",
         choices = "Own dataset loaded")

       }else{
         showModal(modalDialog(
           title = "Error",
           "The uploaded calibration dataset is not supported, please only upload allowed data!",
           footer = modalButton("Ok")
         ))

       }

  })

  ##clear own dataset
  observeEvent(input$clear_calibrationdata, {

    ##correct input path
    updateSelectInput(
      session, "calibration_data",
      choices = basename(sort(list.files(path = "calibration_data/", full.names = TRUE), decreasing = TRUE)))


  })

  # Static pages --------------------------------------------------------------------------------
  output$about <- renderUI({
     HTML(markdown::markdownToHTML(knit('static/about.Rmd', quiet = TRUE, output = tempfile()), fragment.only = TRUE))
   })

   output$news <- renderUI({
     HTML(markdown::markdownToHTML(knit('static/news.Rmd', quiet = TRUE, output = tempfile()), fragment.only = TRUE))
   })

})#last brackets

