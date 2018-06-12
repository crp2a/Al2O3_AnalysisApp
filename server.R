## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Title:   Al2O3:C Analysis App
## Authors: Sebastian Kreutzer, IRAMAT-CRP2A, Université Bordeaux Montaigne (France)
## Contact: sebastian.kreutzer@u-bordeaux-montainge.fr
## Initial: 2018-06-10
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
shinyServer(function(input, output, session) {


    ##import data
    observeEvent(input$file_data, {

          ##data import
          file_data <<- read_XSYG2R(
            file = as.list(input$file_data$datapath),
            fastForward = TRUE,
            verbose = FALSE
          )

          ##import info
          file_info <<- read_XSYG2R(
            file = as.list(input$file_data$datapath),
            fastForward = TRUE,
            verbose = FALSE,
            import = FALSE
          )

          ##replace names by real file names
          file_info[["name"]] <<- rep(input$file_data$name, each = nrow(file_info)/length(input$file_data$name))


          ##create file structure object
          file_structure <<- structure_RLum(file_data)

          ##TODO
          ##add server logic for verifying the import status

          ##deconstruct to wheels
          ##extract needed columns
          wheels <- file_info[["position"]]

          for(n in 1:max(table(wheels))){
            wheels[!duplicated(wheels) &
                     !grepl(pattern = "wheel", x = wheels)] <- paste0("wheel", n)

          }

          ##return wheels
          file_info <<- cbind(file_info, wheels = as.character(wheels))

    })



# PANEL IMPORT------------------------------------------------------------------------------------

  ##=============================##
  ##initial event of loading data
  ##=============================##
  observeEvent(input$file_data, {

    ##set tabs
    output$tabs <- renderUI({

      ##get number of needed wheels
      wheels <- max(table(file_info[["position"]]))

      ##set number of needed tabs
      Tabs <- lapply(1:wheels, function(x){
        tabPanel(title = paste("Wheel",x), value = paste0("wheel",x))})

      ##create call for the frontend
      do.call(tabsetPanel, c(Tabs, id = "wheels"))

    })

  })

  ##=============================##
  ##table - initial event
  ##=============================##
  observeEvent(input$file_data, {
    ##initialise sample
    sample_info_full <<- reactiveValues(
      data = data.frame(
        FILENAME = file_info$name,
        WHEEL = as.character(file_info[["wheels"]]),
        POSITION = as.integer(file_info$position),
        SAMPLE_ID = paste("Sample ", as.character(file_info$position)),
        TYPE = dosimeter_type[1],
        INCLUDE = TRUE,
        IMPORT_STATUS = "OK",
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
        hot_col("TYPE", type = "dropdown", source = dosimeter_type) %>%
        hot_col("IMPORT_STATUS", readOnly = TRUE)

    })

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
      hot_col("TYPE", type = "dropdown", source = dosimeter_type) %>%
      hot_col("IMPORT_STATUS", readOnly = TRUE)

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
        plot_carousel(positions = as.numeric(file_info$position),
          included =  sample_info_full$data[["INCLUDE"]][which(file_info[["wheels"]] == input$wheels)],
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


  # PANEL Analysis ------------------------------------------------------------------------------

  ##=============================##
  ##Calibration dataset selection
  ##=============================##
  observeEvent(input$calibration_data,{
    ##load calibration dataset
    load(calibration_data[[grep(pattern = input$calibration_data,
                                x = calibration_data,
                                fixed = TRUE)]], envir = .GlobalEnv)

    ##show applied dose rate
    if(!is.null(sourceDR_FINAL)){
      output$sourceDR_FINAL <- renderText({
        paste(round(sourceDR_FINAL[[1]],2), " ± ", round(sourceDR_FINAL[[2]],2),
              sourceDR_FINAL[[3]]
        )
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

        ##identify travel dosimeters
        travel_dosimeters <- which(
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
                signal_integral = input$settings_signal_integral,
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
            travel_dosimeter = travel_dosimeters,
            signal_integral = input$settings_signal_integral,
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
            hot_heatmap(cols = 8) %>%
            hot_cols(columnSorting = FALSE) %>%
            hot_col("REJECT", readOnly = FALSE)


        })

        ##add infotext
        output$analysis_table_info_text <- renderText("Note: Dose values are here listed in seconds, not µGy! To reject data permanently, go back to the 'Import' panel")

        ##show first graphic (otherwise it remains empty here, which is odd)
        output$analysis_results.plot <- renderImage({
          filename <- temp_files[[1]]

          #Return a list containing the filename and alt text
          list(src = filename,
               alt = paste("Image number", input$analysis_results_select$select$r))
        }, deleteFile = FALSE)

        ##add download button if results are available
        output$export_analysis_results <- renderUI({
          if(length(results)>0)
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

       ##grep correct aliquot
       temp_aliquot <- paste0("ALQ_",df[["ALQ"]][input$analysis_results_select$select$r],".png")

       ##set filename
       filename <- temp_files[[grep(pattern = temp_aliquot, x = temp_files,fixed = TRUE)]]

        #Return a list containing the filename and alt text
        list(src = filename,
             alt = paste("Image number", temp_aliquot))

    }, deleteFile = FALSE)

   })


  # PANEL Post-processing -------------------------------------------------------------------------
   observeEvent(input$`Post-processing.run`,{

     #preset error message
     outputpost_processing_error <- renderText(NULL)

     if(!is.null(df_reactive$data)){

       ##add infotext
       output$post_processing_table_info_text <- renderText("Sample summary")

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
       df_grouped <- cbind(df_grouped, CV = df_grouped[[3]]/df_grouped[[2]])

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
           SOURCE_DR = source_dose_rate[[1]],
           SOURCE_DR.ERROR = source_dose_rate[[2]],
           DOSE = df_grouped[["MEAN"]] * source_dose_rate[,1],
           DOSE.ERROR = df_grouped[["SD"]] * source_dose_rate[,1]
         ))

         ##add columns of they do not yet exist
         if(!("DURATION" %in% colnames(results_final$data))){
           results_final$data <- cbind(
             results_final$data,
             DATE_IN = Sys.Date(),
             DATE_OUT = Sys.Date(),
             DURATION = NA_integer_,
             DR = NA_real_,
             DR.ERROR = NA_real_
             )

         }

        ##make sure that the sample headers are ok
        colnames(results_final$data) <- c(
          "SAMPLE_ID", "N", "SAMPLE MEAN \n [s]", "SAMPLE SD \n [s]", "CV \n [%]", "SOURCE_DR \n [µGy/s]",
          "SOURCE_DR.ERROR \n [µGy/s]", "DOSE \n [µGy]", "DOSE.ERROR \n [µGy]", "DATE_IN", "DATE_OUT",
          "DURATION \n [days]", "FINAL DR \n [µGy/a]", "FINAL DR.ERROR \n [µGy/a]"
          )

        ##add new ui to add a new 'update' button
        output$post_processing_update <- renderUI({
          actionButton(
            inputId = "post_processing_update",
            label = "Update table",
            icon("refresh", lib = "glyphicon")
            )

        })

       ##create output plot
       ##boxplot
       output$postprocessing_boxplot <- renderPlot({
       ggplot(data = df_reactive$data[!df_reactive$data[["REJECT"]],],
              aes(x = as.factor(SAMPLE_ID), y = DE * source_dose_rate[,1], col = SAMPLE_ID)) +
         geom_boxplot() +
         xlab("SAMPLE ID") +
         ylab(expression(paste(D[e], " [µGy]"))) +
         ggtitle("Totally Absorbed Dose") +
         theme_gray(base_size = 14)
       }, width = 800)


       ##create table output
       output$postprocessing_results <- renderRHandsontable({
         rownames(results_final$data) <- 1:nrow(results_final$data)
         rhandsontable(data = results_final$data, readOnly = TRUE, selectCallback = TRUE,
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
           hot_col("DATE_IN", readOnly = FALSE) %>%
           hot_col("DATE_OUT", readOnly = FALSE) %>%
           hot_context_menu(
             allowRowEdit = FALSE,
             allowColEdit = FALSE) %>%
           hot_table(allowRowEdit = FALSE) %>%
           hot_heatmap(cols = 13) %>%
           hot_cols(columnSorting = TRUE)

       })

     }else{
       output$post_processing_error <- renderText("Error: No data to aggregate!")

     }

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

     ##update DR and DR.ERROR
     results_final$data[["FINAL DR \n [µGy/a]"]] <- results_final$data[["DOSE \n [µGy]"]] * 365.25 / results_final$data[["DURATION \n [days]"]]
     results_final$data[["FINAL DR.ERROR \n [µGy/a]" ]] <- ((results_final$data[["DOSE \n [µGy]"]] * 365.25) /
                                                              as.numeric(results_final$data[["DURATION \n [days]"]])) *
        results_final$data[["DOSE.ERROR \n [µGy]"]] /  results_final$data[["DOSE \n [µGy]"]]

     ##create table output
     output$postprocessing_results <- renderRHandsontable({
       rhandsontable(data = results_final$data, readOnly = TRUE, selectCallback = TRUE,
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
         hot_col("DATE_IN", readOnly = FALSE) %>%
         hot_col("DATE_OUT", readOnly = FALSE) %>%
         hot_context_menu(
           allowRowEdit = FALSE,
           allowColEdit = FALSE) %>%
         hot_table(allowRowEdit = FALSE) %>%
         hot_heatmap(cols = 13) %>%
         hot_cols(columnSorting = TRUE)

    })

   })


  # Static pages --------------------------------------------------------------------------------
  output$about <- renderUI({
     HTML(markdown::markdownToHTML(knit('static/about.Rmd', quiet = TRUE, output = tempfile()), fragment.only = TRUE))
   })

   output$news <- renderUI({
     HTML(markdown::markdownToHTML(knit('static/news.Rmd', quiet = TRUE, output = tempfile()), fragment.only = TRUE))
   })

})#last brackets

