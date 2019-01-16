## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Title:   Al2O3:C Analysis App
## Authors: Sebastian Kreutzer, IRAMAT-CRP2A, Université Bordeaux Montaigne (France)
## Contact: sebastian.kreutzer@u-bordeaux-montainge.fr
## Initial data:    2018-06-07
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
shinyUI(
 navbarPage(
   title = HTML(paste0("Al<sub>2</sub>O<sub>3</sub>:C Analysis App")),
   windowTitle = "Al2O3:C Analysis App",
   footer = HTML("<hr><div align = 'center'><small>This software comes WITHOUT ANY WARRANTY.</small></div>"),

  # PANEL - Import ------------------------------------------------------------------------------
  tabPanel(title = "Import",
   sidebarLayout(
    sidebarPanel(
      fileInput("file_data", accept = "*.xsyg",
               label = "Select XSYG-files containing your measurement data...", multiple = TRUE),
      div(
       checkboxInput("import_file_names_assignment",
          label = "Use filenames for wheel assignment",
          value = FALSE),
       br(),
       actionButton("file_data_example", label = "Load example data")
      ,align = "center"),
        fluidRow(
          br(),
          div(
            plotOutput("carousel"),
            align = "center"
          )
        )

    , width = 3),
      mainPanel(
        ##create all the needed tabs
        uiOutput("tabs"),
        rHandsontableOutput("sample_info")

     )##mainPanel
    ),##sidebarLayout
    icon = icon("import", lib = "glyphicon")
  ),##tabPanel - Data Import

  # PANEL - Analysis ------------------------------------------------------------------------------
  tabPanel("Analyse",
     ##=========================================SIDEBAR============================================##
     sidebarLayout(
      sidebarPanel(
        h5("Used calibration dataset"),
        wellPanel(
          selectInput(
            inputId = "calibration_data",
            label = "Available calibration datasets",
            choices = basename(calibration_data)),
          wellPanel(
            div(strong(textOutput("sourceDR_FINAL_today")), textOutput("sourceDR_FINAL"),
              align = "center"
            )
          )
        ),
        h5("Settings"),
        wellPanel(
          ##signal integral
          sliderInput(
            "settings_signal_integral",
            label = "Signal integral",
            value = settings_signal_integral,
            min = 1,
            max = 99
          ),
          checkboxInput(
            "settings_cross_talk_correction",
            label = "Apply cross-talk correction",
            value = TRUE),
          checkboxInput(
            "settings_travel_dosimeter",
            label = "Apply travel dosimeter correction",
            value = FALSE)
        ),
        div(
          actionButton("Analysis.run", icon("play-circle"),
                       "(Re) Run analysis ...", icon = NULL, width = NULL,
                       style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
          align = "center"
        ),
        br(),
        tags$div(title = "Download combined results as returned by the R function analyse_Al2O3C_Measurements().",
          uiOutput("export_analysis_results"),
          align = "center"
        ),
        br(),
        shinyjs::useShinyjs(),
        tags$head(
          tags$style("#warnings{color: red;
                     font-size: 11px;
                     font-style: monospace;
                     }"
         )
        ),
        div(
          verbatimTextOutput("warnings"),
          align = "center"
        )

      ,width = 3),
      ##=========================================MAIN==============================================##
      mainPanel(
        fluidRow(
          column(10, offset = 1,
            h3(textOutput("analysis_error"), align = "center"),
            div(align = "center",
              plotOutput(outputId = "analysis_results.plot")
            )
          )
        ),
        fluidRow(
          column(10, offset = 1,
              br(),
              br(),
              textOutput("analysis_table_info_text"),
              rHandsontableOutput("analysis_results", height = 600, width = 1200)
          )
         )
      )#mainPanel
    )
  , icon = icon("cog", lib = "glyphicon")
  ), ##tablPanel - Analysis


  # PANEL - Post-processing------------------------------------------------------------------------
  tabPanel("Post-processing",
           ##=========================================SIDEBAR=====================================##
           sidebarLayout(
             sidebarPanel(
               fluidRow(
                 column(12,
                   div(
                    actionButton("Post-processing.run", icon("filter"),
                               "Aggregate data ...", icon = NULL, width = NULL,
                               style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),

                 align = "center"),
                 br(),
                 div(
                  uiOutput("post_processing_update"),
                  align = "center"
                 )
                )
               )
             , width = 3),
           ##=========================================MAIN========================================##
           mainPanel(
             fluidRow(
               column(10, offset = 1,
                 h3(textOutput("post_processing_error"), align = "center"),
                 div(align = "center",
                   plotOutput(outputId = "postprocessing_boxplot", width = 1000, height = 400)
                 )
               )
             ),
             fluidRow(
               column(10, offset = 1,
                    br(),
                    br(),
                   textOutput("post_processing_table_info_text"),
                   rHandsontableOutput("postprocessing_results", height = 600, width = 1000)
              )
             )
           )
          )
  , icon = icon("random", lib = "glyphicon")
  ),
  # PANEL Settings ------------------------------------------------------------------------------
  tabPanel("Settings",
    bookmarkButton("Bookmark this session (beta)"),
    downloadButton("download_CalibrationData",label = "Download calibration data", icon = "download"),
    br(),
    h4("Session information"),
    HTML(paste0("<b>Loaded R packages</b><br>",
         "&nbsp; + 'Luminescence' (v", packageDescription('Luminescence')$Version,")<br>",
         "&nbsp; + 'shiny' (v", packageDescription('shiny')$Version,")<br>",
         "&nbsp; + 'shinyjs' (v", packageDescription('shinyjs')$Version,")<br>",
         "&nbsp; + 'shape' (v", packageDescription('shape')$Version,")<br>",
         "&nbsp; + 'rhandsontable' (v", packageDescription('rhandsontable')$Version,")<br>",
         "&nbsp; + 'plyr' (v", packageDescription('plyr')$Version,")<br>",
         "&nbsp; + 'ggplot2' (v", packageDescription('ggplot2')$Version,")<br>",
         "&nbsp; + 'knitr' (v", packageDescription('knitr')$Version,")<br>",
         "&nbsp; + 'digest' (v", packageDescription('digest')$Version,")<br>"

    )),
    HTML(paste("<br><b>Initially available R objects</b><br><ul>",
               "<li>results_CT&nbsp;",
               if (!is.null("results_CT")) {
                 "<i class='fa fa-check-circle'></i>"
               } else{
                 "<i class='fa fa-times-circle'></i>"
               },
               "<li>results_ITC&nbsp;",
               if (!is.null("results_ITC")) {
                 "<i class='fa fa-check-circle'></i>"
               } else{
                 "<i class='fa fa-times-circle'></i>"
               },
               "<li>sourceDR_FINAL&nbsp;",
               if (!is.null("sourceDR_FINAL")) {
                 "<i class='fa fa-check-circle'></i>"
               } else{
                 "<i class='fa fa-times-circle'></i>"
               },
               "</ul>")),
    HTML(paste("<b>Allowed md5 data verification hash values</b><br> ", paste(verification_hash, collapse = "<br>"))),
    helpText(HTML("The verification hash was generated from a reference data structure the time this app was created<br>
                  to ensure that only measurement data that can be analysed by this app are imported.<br>
                  The hash is hard coded and cannot be changed.
                  ")),
    hr(),
    h4("Dangerous zone"),
    fileInput("upload_calibrationdata", accept = "*.Rdata", label = "Upload own calibration dataset"),
    helpText(HTML("
      The '.Rdata' file must contain the objects <br>
      - 'results_CT', <br>
      - 'results_ITC',<br>
      - 'sourceDR_FINAL'.<br>
      Please note that any dataset uploaded here is only valid for the current shiny session!")),
    actionButton("clear_calibrationdata", label = "Remove own calibration data", icon = icon("remove",lib = "glyphicon"))

  ,icon = icon("wrench", lib = "glyphicon")),
  # PANEL - News ------------------------------------------------------------------------------
  tabPanel("News",
           fluidRow(
             column(10, offset = 1,
                    uiOutput('news')
             )
           ),icon = icon("list-alt", lib = "glyphicon")
  ),#news
  # PANEL - About ------------------------------------------------------------------------------
  tabPanel("About",
           fluidRow(
             column(10, offset = 1,
                    uiOutput('about')
             )
           ),icon = icon("info-sign", lib = "glyphicon")
  )#About
 )##navbarPage
)##ShinyUI


