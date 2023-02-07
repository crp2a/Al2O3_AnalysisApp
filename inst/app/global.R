## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Title:   Al2O3:C Analysis App
## Authors: Sebastian Kreutzer, Heidelberg University (Germany)
##          Nicolas Frerebeau, Université Bordeaux Montaigne (France)
## Contact: services-archeosciences@u-bordeaux-montaigne.fr
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Clean current environment ===================================================
rm(list = ls())

## Load packages ===============================================================
library(Al2O3AnalysisApp)

## Set Shiny settings ==========================================================
options(shiny.maxRequestSize = 30 * 1024^2)
enableBookmarking(store = "server")

# Initialise variables =========================================================
dosimeter_type <- c("field", "travel")
dosimeter_thickness <- c("normal", "thin")

# Initialise environment =======================================================
current_env <- environment()

# Load calibration data ========================================================
calibration_dir <- system.file("extdata", "calibration", package = "Al2O3AnalysisApp")
calibration_data <- list.files(path = calibration_dir, full.names = TRUE)
calibration_data <- sort(calibration_data, decreasing = TRUE)
