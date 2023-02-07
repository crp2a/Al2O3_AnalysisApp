#' About UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @family modules
#' @keywords internal
#' @export
module_about_ui <- function(id) {
  tabPanel(
    "About",
    icon = icon("circle-info"),
    fluidRow(
      column(
        width = 8,
        align = "center",
        offset = 2,
        wellPanel(
          h4(
            paste("Luminescence", utils::packageVersion("Luminescence"), sep = " "),
            tags$br(),
            paste("Al2O3AnalysisApp", utils::packageVersion("Al2O3AnalysisApp"), sep = " ")
          ),
          tags$br(),
          tags$p(
            icon("github"), "Source code:", tags$br(),
            tags$a(href = "https://github.com/R-Lum/Luminescence",
                   "https://github.com/R-Lum/Luminescence"), tags$br(),
            tags$a(href = "https://github.com/crp2a/Al2O3AnalysisApp",
                   "https://github.com/crp2a/Al2O3AnalysisApp")
          ),
          tags$p(
            "This program is free software: you can redistribute it and/or
              modify it under the terms of the GNU General Public License as
              published by the Free Software Foundation, either version 3 of
              the License, or (at your option) any later version."
          ),
          tags$p(
            "This program is distributed in the hope that it will be useful,
              but WITHOUT ANY WARRANTY; without even the implied warranty of
              MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
              GNU General Public License for more details."
          ),
          tags$br(),
          tags$p(format(utils::citation("Luminescence"), bibtex = FALSE)[[1]]),
          tags$p(format(utils::citation("Luminescence"), bibtex = FALSE)[[2]]),
          tags$br(),
          tags$p(
            "This work received a state financial support
              managed by the Agence Nationale de la Recherche (France)
              throught the program Investissements d'avenir (ref. ",
            tags$a(href = "https://lascarbx.labex.u-bordeaux.fr/",
                   "10-LABX-0052", .noWS = c("before", "after")),
            ")."
          )
        )
      )
    )
  )
}
