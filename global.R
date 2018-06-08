## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Title:   Al2O3:C Analysis App
## Authors: Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France)
## Contact: sebastian.kreutzer@u-bordeaux-montainge.fr
## Date:    2018-06-07
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

##load needed packages
library(shiny)
library(Luminescence)
library(shape)
library(rhandsontable)
library(plyr)
library(ggplot2)

##Shiny settings
options(shiny.maxRequestSize=30*1024^2)
enableBookmarking(store = "server")

##load calibration data
calibration_data <- list.files(path = "calibration_data/", full.names = TRUE)
load(file = calibration_data)

##initialise data
file_data <- NULL
file_info <- NULL
file_structure <- NULL
settings_signal_integral <- c(1,99)
results <- list()
temp_files <<- list()
df <- NULL
dosimeter_type <- c("field", "travel")

# Carousel plot -------------------------------------------------------------------------------
plot_carousel <<- function(positions = NULL, wheel = NULL, included = NULL){

  ##pre-calculation
  arc.step <- (2 * pi) / 40
  step <- 0

  ##set colour
  col_positions <- rep("grey", 40)

  if(!is.null(included)){
    col_positions[which(included)] <- rgb(0, 0.8, 0.2, 1)
    col_positions[which(!included)] <- rgb(1, 0, 0, 1)


  }

  ##set layout matrix
  layout(mat = matrix(
    c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 3, 1, 1, 1, 1, 3),
    5,
    5,
    byrow = TRUE
  ))

  ##create empty plot
  par(
    mar = c(1, 1, 1, 1),
    omi = c(1, 1, 1, 1),
    oma = c(0.2, 0.2, 0.2, 0.2),
    cex = 1.1
  )
  shape::emptyplot(c(-1.15, 1.15), frame.plot = FALSE)

  ##add outher circle
  shape::plotcircle(r = 1.1, col = rgb(0.9, 0.9, 0.9, 1))

  ##add inner octagon
  shape::filledcircle(
    r1 = 0.6,
    mid = c(0, 0),
    lwd = 1,
    lcol = "black",
    col = "white"
  )

  ##add wheel number
  if(!is.null(wheel)){
    text(x = 0, y = 0, labels = wheel, cex = 2, col = "black")

  }

  ##add circles
  for (i in 1:40) {
    shape::plotcircle(
      r = 0.05,
      mid = c(cos(step), sin(step)),
      cex = 6,
      pch = 20,
      col = col_positions[i]
    )


    ##outer position text
    text(x = cos(step) * 0.85,
         y = sin(step) * .85,
         labels = i)
    step <- step + arc.step

  }

}



