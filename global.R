## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Title:   Al2O3:C Analysis App
## Authors: Sebastian Kreutzer, IRAMAT-CRP2A, Université Bordeaux Montaigne (France)
## Contact: sebastian.kreutzer@u-bordeaux-montainge.fr
## Initial date:    2018-06-07
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

##load needed packages
rm(list = ls())
require(Luminescence)
require(shiny)
require(shinyjs)
require(shape)
require(rhandsontable)
require(plyr)
require(ggplot2)
require(knitr)
require(digest)

##Shiny settings
options(shiny.maxRequestSize=30*1024^2)
enableBookmarking(store = "server")
current_env <- environment()

# Load calibration data -----------------------------------------------------------------------
calibration_data <- sort(list.files(path = "calibration_data/", full.names = TRUE), decreasing = TRUE)

  ##load initial set
  ##calibration data
  ##make sure nothing breaks here if no file is available
  if(length(calibration_data) == 0){
    results_CT <- NULL
    results_ITC <- NULL
    sourceDR_FINAL <- NULL
    calibration_data <- "not available"

  }else{
    ##make sure that the example data is only loaded if needed
    if(length(calibration_data) > 1){
      calibration_data <- calibration_data[
        !grepl(pattern = "Example_Calibration.Rdata", x = calibration_data, fixed = TRUE)]

    }

  }



# Initialise variables ------------------------------------------------------------------------
file_data <- NULL
file_info <- NULL
file_structure <- NULL
settings_signal_integral <- c(1,99)
results <- list()
temp_files <<- list()
df <- NULL
df_reactive <- NULL
verify <- TRUE
dosimeter_type <- c("field", "travel")
verification_hash <- "c2bba3c97909e573a3f7b25dad61380d"



# Helper functions ----------------------------------------------------------------------------

##+++++++++++++++++++
##IMPORT FILE
##++++++++++++++++++
.import_file <- function(file, name, import_file_names_assignment = NULL){
  ##import file
  ##data import
  file_data <<- read_XSYG2R(
    file = file,
    fastForward = TRUE,
    verbose = FALSE
  )

  ##import info
  file_info <<- read_XSYG2R(
    file = file,
    fastForward = TRUE,
    verbose = FALSE,
    import = FALSE
  )

    ##verify file data and kick out the rest
    verify <<- vapply(file_data, function(v) {
      digest::digest(names(v), algo = "md5") == "c2bba3c97909e573a3f7b25dad61380d"
    }, logical(1))

    ##kick out what do not belong into the dataset
    if(!all(verify)){
     if(!any(verify)){
       showModal(modalDialog(
         title = "Verification hash mismatch",
         "All files were excluded, try another file!",
         footer = modalButton("Ok, I'll select another file")
       ))

       file_data <<- NULL
       file_info <<- NULL
       return()

     }else{
       showModal(modalDialog(
         title = "Verification hash mismatch",
         "Some of your data do not appear to be Al2O3:C measurement data and were automatically excluded!",
         footer = modalButton("Well, that may happen.")
       ))


    }

   }

  ##replace names by real file names
  for(i in 1:length(unique(file_info[["parentID"]]))){
    file_info[["name"]][grepl(pattern = unique(file_info[["parentID"]])[i], x = file_info[["parentID"]], fixed = TRUE)] <<- name[i]

  }

  ##create file structure object
  file_structure <<- structure_RLum(file_data)

  ##deconstruct to wheels
  ##extract needed columns
  if (!is.null(import_file_names_assignment) && import_file_names_assignment == TRUE) {
    wheels <- file_info[["name"]]
    unique_names <- unique(wheels)

    for(n in 1:length(unique_names)){
      wheels[grepl(pattern = unique_names[n], x = wheels)] <- paste0("wheel", n)

    }


  }else{
    wheels <- file_info[["position"]]
    for(n in 1:max(table(wheels))){
      wheels[!duplicated(wheels) &
               !grepl(pattern = "wheel", x = wheels)] <- paste0("wheel", n)

    }
  }

  ##return wheels
  file_info <<- cbind(file_info, wheels = as.character(wheels))
}

##+++++++++++++++++++
##PLOT CAROUSEL
##++++++++++++++++++
.plot_carousel <<- function(positions = NULL, wheel = NULL, included = NULL){


  ##pre-calculation
  arc.step <- (2 * pi) / 40
  step <- 0

  ##set initial colour
  col_positions <- rep("grey", 40)

  if(!is.null(included)){
    col_positions[positions[which(included)]] <- rgb(0, 0.8, 0.2, 1)
    col_positions[positions[which(!included)]] <- rgb(1, 0, 0, 1)


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



