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

    })


# TAB 1 ---------------------------------------------------------------------------------------

  ##=============================##
  ##carousel plot - default
  ##=============================##
  output$carousel <- renderPlot({
    plot_carousel()
  })

  ##=============================##
  ##carousel plot - event
  ##=============================##
  observeEvent(input$file_data, {
      output$carousel <- renderPlot({
        plot_carousel(positions = as.numeric(file_info$position))

      })
   })

  ##=============================##
  ##table - initial event
  ##=============================##
  observeEvent(input$file_data, {
    ##initialise sample
    sample_info <<- reactiveValues(
      data = data.frame(
        FILENAME = file_info$name,
        POSITION = as.integer(file_info$position),
        SAMPLE_ID = "unkown",
        INCLUDE = TRUE,
        stringsAsFactors = FALSE
      ))

    output$sample_info <- renderRHandsontable({
      rhandsontable(data = sample_info$data)

    })

  })

  ##=============================##
  ##table - updated event
  ##=============================##
  observe({
    if(!is.null(input$sample_info))
      sample_info$data <- hot_to_r(input$sample_info)

  })





 }
)

