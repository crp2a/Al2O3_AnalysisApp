## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Title:   Al2O3:C Analysis App
## Authors: Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
## Contact: sebastian.kreutzer@u-bordeaux-montainge.fr
## Date:    2017-11-11
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

          ##create file structure object
          file_structure <<- structure_RLum(file_data)

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



# TAB 1 ---------------------------------------------------------------------------------------

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
        POSITION = as.integer(file_info$position),
        SAMPLE_ID = "unkown",
        INCLUDE = TRUE,
        stringsAsFactors = FALSE
      ))

    ##create sample_info output
    output$sample_info <- renderRHandsontable({
      rhandsontable(data = sample_info_full$data[which(file_info[["wheels"]] == "wheel1"), ]) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)  %>%
        hot_col("FILENAME", readOnly = TRUE) %>%
        hot_col("POSITION", readOnly = TRUE)

    })

  })

  ##=============================##
  ##table - select tabs
  ##=============================##
  observeEvent(input$wheels, {
  output$sample_info <- renderRHandsontable({
    rhandsontable(data = sample_info_full$data[which(file_info[["wheels"]] == input$wheels), ]) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)  %>%
      hot_col("FILENAME", readOnly = TRUE) %>%
      hot_col("POSITION", readOnly = TRUE)

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
          )}, height = 500, width = 500
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
  ##run analysis
  ##=============================##
  observeEvent(input$Analysis.run, {

      #preset error message
      output$analysis_error <- renderText(NULL)

      ##make sure that the app does not crash
      if(!is.null(file_data) && any(sample_info_full$data[["INCLUDE"]])){

        ##remove all values previously deselected
        file_data[!sample_info_full$data[["INCLUDE"]]] <- NULL
        results[!sample_info_full$data[["INCLUDE"]]] <<- NULL

        ##initialise values
        temp_dir <- tempdir()

        ##RUN ANALYSIS
        ##with progress bar
        withProgress(
          message = "Analysing data ...", min = 0, max = length(file_data), {

          ##run analysis
          for(i in 1:length(file_data)){
            incProgress(i)
            temp_files[[i]] <<- paste0(temp_dir,"/ALQ_",i,".png")
            png(file = temp_files[[i]], bg = "transparent", width = 700, height = 400)
              results[[i]] <<- Luminescence::analyse_Al2O3C_Measurement(
                object = file_data[[i]],
                signal_integral = input$settings_signal_integral,
                irradiation_time_correction = results_ITC,
                cross_talk_correction = if(input$settings_cross_talk_correction){
                  results_CT
                }else{
                  NULL
                },
                plot = TRUE,
                verbose = FALSE)
            dev.off()

          }
        })

      ##create data.frame
      df <<- cbind(
        sample_info_full$data[sample_info_full$data[["INCLUDE"]],-c(4)],
        merge_RLum(results)$data[,c(1,2)])

      ##render handsontable
      output$analysis_results <- renderRHandsontable({
          rhandsontable(data = df, readOnly = TRUE, selectCallback = TRUE) %>%
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
            hot_heatmap(cols = 4)


        })

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
           filename = "Analysis_Results.zip",
              content = function(file){
                  temp_results <- merge_RLum(results)
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
                  zip(zipfile = file, files = fs, flags = "-j")
                },
                contentType = "application/zip"
            )

      }else{
        output$analysis_error <- renderText("Error: No file imported!")

      }

   })#run analysis tab

   ##provide graphical output enviroment
   observeEvent(input$analysis_results_select, {
     output$analysis_results.plot <- renderImage({
       filename <- temp_files[[input$analysis_results_select$select$r]]

        #Return a list containing the filename and alt text
        list(src = filename,
             alt = paste("Image number", input$analysis_results_select$select$r))
    }, deleteFile = FALSE)

   })


  # Post-processing -----------------------------------------------------------------------------
   observeEvent(input$`Post-processing.run`,{

     ##group by sample ID
     df_grouped <- dlply(df, .variables = "SAMPLE_ID", .fun = identity)

     ##error weighted mean for each position
     df_combined <- t(vapply(1:length(df_grouped), function(x){
       unlist(calc_Statistics(
         df_grouped[[x]][,c("DE","DE_ERROR")], n.MCM = 1000)[["MCM"]][c("mean", "sd.abs")])
     }, FUN.VALUE = numeric(length = 2)))

     ##add sample ID
     df_grouped <-
       data.frame(ID = attributes(df_grouped)$names,
                  df_combined,
                  stringsAsFactors = FALSE)

     ##calculate relative error
     df_grouped <- cbind(df_grouped, sd.rel = df_grouped[[3]]/df_grouped[[2]])

     # ##translate to µGy
     source_dose_rate <- calc_SourceDoseRate(
       measurement.date = as.Date(strtrim(file_info$startDate[1],8), format = "%Y%m%d"),
       calib.date = as.Date(sourceDR_FINAL$CAL_DATE),
       calib.dose.rate = c(sourceDR_FINAL$DR),
       calib.error = c(sourceDR_FINAL$DR_ERROR)
     )$dose.rate

     ##combine
     results_final <- cbind(
       df_grouped,
       DOSE = df_grouped[["mean"]] * source_dose_rate[,1],
       DOSE.ERROR = df_grouped[["sd.abs"]] * source_dose_rate[,1]
     )

     ##create output plot
     ##boxplot
     output$postprocessing_boxplot <- renderPlot({
     ggplot(data = df, aes(x = as.factor(SAMPLE_ID), y = DE * source_dose_rate[,1], col = SAMPLE_ID)) +
       geom_boxplot() +
       xlab("Dosimeter ID") +
       ylab(expression(paste(D[e], " [µGy]"))) +
       ggtitle("Totally Absorbed Dose")
     })


     ##create table output
     output$postprocessing_results <- renderRHandsontable({
       colnames(results_final) <- toupper(colnames( results_final))
       rhandsontable(data = results_final, readOnly = TRUE, selectCallback = TRUE)

     })


   })#observeEvent Post-processing


 }
)

