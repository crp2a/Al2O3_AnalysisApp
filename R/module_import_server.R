#' Import Server
#'
#' @param input provided by \pkg{Shiny}.
#' @param output provided by \pkg{Shiny}.
#' @param session provided by \pkg{Shiny}.
#' @param user_data a [shiny::reactiveValues()] list with the
#'  following elements: "`data`", "`info`" and "`samples`".
#' @param user_settings a [shiny::reactiveValues()] list.
#' @seealso [module_import_ui()]
#' @family modules
#' @keywords internal
#' @export
module_import_server <- function(id, user_data, user_settings) {
  moduleServer(id, function(input, output, session) {
    ## Import data
    observeEvent(input$files, {
      req(input$files)

      new <- import_XSYG(
        file = as.list(input$files$datapath),
        name = input$files$name,
        file_names_assignment = input$file_names_assignment
      )

      ## Set reactive values
      user_data$data <- new$data
      user_data$info <- new$info

      user_data$samples <- data.frame(
        FILENAME = new$info$name,
        WHEEL = as.character(new$info$wheels),
        POSITION = as.integer(new$info$position),
        SAMPLE_ID = sprintf("Sample %02d", as.numeric(new$info$position)),
        TYPE = dosimeter_type[1],
        THICKNESS = dosimeter_thickness[1],
        INCLUDE = new$verify,
        stringsAsFactors = FALSE
      )
    })

    ## Create tabs
    observeEvent(user_data$info, {
      ## Get wheels
      wheels <- unique(as.character(user_data$info$wheels))

      ## Build tabs
      ns <- session$ns
      tabs <- lapply(
        X = wheels,
        FUN = function(x) tabPanel(title = x, value = ns(x))
      )

      ## Create call for the frontend
      output$tabs <- renderUI({
        do.call(tabsetPanel, c(tabs, id = ns("wheels")))
      })
    })

    ## Update data
    observeEvent(input$sample_info, {
      req(user_data$info)
      req(user_data$samples)

      ## Update table values if row numbers match, otherwise we overwrite
      updated <- rhandsontable::hot_to_r(input$sample_info)
      current <- match(rownames(updated), rownames(user_data$samples))
      user_data$samples[current, ] <- updated

      if (!any(user_data$samples$INCLUDE)) {
        showModal(
          modalDialog(
            "Smart move, nothing included, nothing can go wrong...",
            title = "Important message",
            footer = modalButton("Ok, I'll try to include at least one aliquot.")
          )
        )
      }
    })

    ## Get data
    current_wheel <- reactive({
      req(user_data$info)
      req(user_data$samples)
      req(input$wheels)

      ns <- session$ns
      current_wheel <- ns(user_data$info$wheels) == input$wheels
      user_data$samples[current_wheel, ]
    })

    ## Fill tabs
    output$sample_info <- rhandsontable::renderRHandsontable({
      hot <- rhandsontable::rhandsontable(current_wheel(), readOnly = TRUE)
      hot <- rhandsontable::hot_context_menu(
        hot,
        allowRowEdit = FALSE,
        allowColEdit = FALSE
      )
      hot <- rhandsontable::hot_table(
        hot,
        highlightCol = TRUE,
        highlightRow = TRUE,
        allowRowEdit = FALSE
      )
      hot <- rhandsontable::hot_col(
        hot, col = "TYPE", type = "dropdown",
        source = dosimeter_type, readOnly = FALSE
      )
      hot <- rhandsontable::hot_col(
        hot, col = "THICKNESS", type = "dropdown",
        source = dosimeter_thickness, readOnly = FALSE
      )
      hot <- rhandsontable::hot_col(hot, col = "INCLUDE", readOnly = FALSE)
    })

    ## Plot carousel
    output$carousel <- renderPlot(
      {
        plot_carousel(
          positions = as.numeric(current_wheel()$POSITION),
          included = current_wheel()$INCLUDE,
          wheel = unique(current_wheel()$WHEEL)
        )
      },
      bg = "transparent"
    )
  })
}
