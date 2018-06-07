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
       hr(),
       ##create all the needed tabs
       uiOutput("tabs"),
       rHandsontableOutput("sample_info")

    ),
      mainPanel(
        fluidRow(
         column(10,plotOutput("carousel"))

        )

     )##mainPanel
    ),##sidebarLayout
    icon = icon("import", lib = "glyphicon")
  ),##tabPanel - Data Import
  # PANEL - Analysis ------------------------------------------------------------------------------
  tabPanel("Analysis",
     ##=========================================SIDEBAR============================================##
     sidebarLayout(
      sidebarPanel(
        h5("Used calibration dataset"),
        wellPanel(
          p(basename(calibration_data), align = "center")
        ),
        h5("Settings"),
        wellPanel(
          ##signal integral
          sliderInput(
            "analysis.signal_integral",
            label = "Signal integral",
            value = c(0,100),
            min = 0,
            max = 100
          )

        ),
        div(
          actionButton("Analysis.run", "Run analysis ...", icon = NULL, width = NULL),
          align = "center"
        )
      ),
      ##=========================================MAIN==============================================##
      mainPanel(
        fluidRow(
          column(10, offset = 1,
            h3(textOutput("analysis_error"), align = "center"),
            plotOutput(outputId = "analysis_results.plot")
          )
        ),
        fluidRow(
          column(10, offset = 1,
            rHandsontableOutput("analysis_results", height = 600, width = 800)
          )
        )
      )#mainPanel
    )
  , icon = icon("cog", lib = "glyphicon")
  ) ##tablPanel - Analysis
 )##navbarPage
)##ShinyUI


