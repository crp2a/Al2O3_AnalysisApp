## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Title:   Al2O3:C Analysis App
## Authors: Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
## Contact: sebastian.kreutzer@u-bordeaux-montainge.fr
## Date:    2017-11-11
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
shinyUI(
 navbarPage("Al2O3:C Analysis",
  tabPanel("Data import",
   sidebarLayout(
    sidebarPanel(
     fileInput("file_data", accept = "*.xsyg",
               label = "Select XSYG-files containing your measurement data...", multiple = TRUE),
       hr(),
       rHandsontableOutput("sample_info")

    ),
    mainPanel(
       plotOutput("carousel")

     )
    )
   )##tablPanel --
 )##navbarPage
)##ShinyUI


