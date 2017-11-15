## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Title:   Al2O3:C Analysis App
## Authors: Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
## Contact: sebastian.kreutzer@u-bordeaux-montainge.fr
## Date:    2017-11-11
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
shinyUI(
 navbarPage("Al2O3:C Analysis",
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
  tabPanel("Analysis"


  , icon = icon("cog", lib = "glyphicon")
  ) ##tablPanel - Analysis
 )##navbarPage
)##ShinyUI


