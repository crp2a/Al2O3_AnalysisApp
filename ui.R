## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Title:   Al2O3:C Analysis App
## Authors: Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
## Contact: sebastian.kreutzer@u-bordeaux-montainge.fr
## Date:    2017-11-11
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
shinyUI(
 navbarPage("Al2O3:C Analysis",

  # PANEL - Import ------------------------------------------------------------------------------
  tabPanel(title = "Import",
   sidebarLayout(
    sidebarPanel(
      fileInput("file_data", accept = "*.xsyg",
               label = "Select XSYG-files containing your measurement data...", multiple = TRUE),
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
          p(basename(calibration_data), align = "center"),
          wellPanel(
            div(textOutput("sourceDR_FINAL"),
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
                       "Run analysis ...", icon = NULL, width = NULL,
                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
          align = "center"
        ),
        br(),
        tags$div(title="Download combined results as returned by the R function analyse_Al2O3C_Measurements().",
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
              rHandsontableOutput("analysis_results", height = 600, width = 1000)
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
  # PANEL - About ------------------------------------------------------------------------------
  tabPanel("About",
    fluidRow(
      column(8, offset = 1,
        h4("About this app"),
        p("This software was developed for the IRAMAT-CRP2A, Université Bordeaux Montaigne (France)
          to analyse Al2O3:C chip measurements. For all calculations functions from the R package 'Luminescence' are used."),
        p("App version: 0.1.0 [2018-06-09]"),
        p("Author: sebastian.kreutzer@u-bordeaux-montaigne.fr"),
        br(),
        h4("References to cite"),

        p(paste0("Kreutzer, S., Burow, C., Dietze, M., Fuchs, Margret C., Schmidt, C., Fischer, M., Friedrich, J. ", strtrim(Sys.Date(), 4), ".
        Luminescence: Comprehensive Luminescence Dating Data Analysis. R package version",packageDescription("Luminescence")$Version,".
        https://CRAN.R-project.org/package=Luminescence")),

        p("Kreutzer S., Martin L., Guérin G., Tribolo C., Selva P., Mercier N., 2018.
        Environmental Dose Rate Determination Using a Passive Dosimeter: Techniques and Workflow for alpha-Al2O3:C Chips.
        Geochronometria 45, 56-67. doi: 10.1515/geochr-2015-0086. doi: 10.1515/geochr-2015-0086."),

        p("Kreutzer, S., Schmidt, C., Fuchs, M.C., Dietze, M., Fischer, M., Fuchs, M., 2012.
          Introducing an R package for luminescence dating analysis. Ancient TL 30 (1), 1–8.")
      )
    ),icon = icon("info-sign", lib = "glyphicon")
  )#About
 )##navbarPage
)##ShinyUI


