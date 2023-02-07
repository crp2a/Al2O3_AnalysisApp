# HELPERS

#' Plot a Carousel
#'
#' @param positions A [`logical`].
#' @param included A [`logical`] vector.
#' @param wheel A [`character`] string.
#' @return
#'  `plot_carousel()` is called it for its side-effects: it results in a graphic
#'  being displayed.
#' @examples
#' \dontrun{
#' plot_carousel()
#' }
#' @keywords internal
#' @author S. Kreutzer
#' @noRd
plot_carousel <- function(positions = NULL, included = NULL, wheel = NULL) {
  ## Pre-calculation
  arc.step <- (2 * pi) / 40
  step <- 0

  ## Set initial colour
  col_positions <- rep("grey", 40)

  if(!is.null(included)){
    col_positions[positions[included]] <- grDevices::rgb(0, 0.8, 0.2, 1)
    col_positions[positions[!included]] <- grDevices::rgb(1, 0, 0, 1)
  }

  ## Save and restore graphical parameters
  old_par <- graphics::par(
    mar = c(1, 1, 1, 1),
    omi = c(1, 1, 1, 1),
    oma = c(0.2, 0.2, 0.2, 0.2),
    cex = 1.1,
    no.readonly = TRUE
  )
  on.exit(graphics::par(old_par), add = TRUE)

  ## Open new window
  grDevices::dev.hold()
  on.exit(grDevices::dev.flush(), add = TRUE)
  graphics::plot.new()

  ## Set plotting coordinates
  graphics::plot.window(xlim = c(-1.15, 1.15), ylim = c(-1.15, 1.15), asp = 1)

  ## Add outher circle
  plot_circle(0, 0, radius = 1.1, lwd = 2, border = "black",
              col = grDevices::rgb(0.9, 0.9, 0.9, 1))

  ## Add inner octagon
  plot_circle(0, 0, radius = 0.6, border = "black", col = "white")

  ## Add wheel number
  if (!is.null(wheel)) {
    graphics::text(x = 0, y = 0, labels = wheel, cex = 2, col = "black")
  }

  ## Add circles
  for (i in 1:40) {
    plot_circle(
      x = cos(step), y = sin(step), radius = 0.05, lwd = 2,
      border = "black", col = col_positions[i]
    )

    ## Outer position text
    graphics::text(x = cos(step) * 0.85, y = sin(step) * .85, labels = i)
    step <- step + arc.step
  }
}

#' Draw a Circle
#'
#' @param x,y A length-one [`numeric`] vector giving the coordinates of the
#'  center of the circle.
#' @param radius A length-one [`numeric`] vector giving the radius of the
#'  circle.
#' @param n A length-on [`integer`] vector specifying the number of vertices to
#'  draw the circle.
#' @param ... Further parameters to be passed to [graphics::polygon()].
#' @examples
#' \dontrun{
#' plot(NA, xlim = c(-1, 1), ylim = c(-1, 1),
#'      axes = FALSE, ann = FALSE, asp = 1)
#' plot_circle(0, 0, 0.5)
#' }
#' @keywords internal
#' @author N. Frerebeau
#' @noRd
plot_circle <- function(x, y, radius, n = 100, ...) {
  angle.inc <- 2 * pi / n
  angles <- seq(0, 2 * pi - angle.inc, by = angle.inc)

  xv <- cos(angles) * radius + x
  yv <- sin(angles) * radius + y
  graphics::polygon(xv, yv, ...)
}

#' Import XSYG Files
#'
#' @param file A [`character`] vector or a [`list`] giving the name of the files
#'  which the data are to be read from.
#' @param name A [`character`] vector giving the dataset names.
#' @param file_names_assignment A [`logical`] scalar: should filenames be
#'  used for wheel assignment?
#' @keywords internal
#' @author S. Kreutzer
#' @noRd
import_XSYG <- function(file, name, file_names_assignment = NULL){
  ## Import data
  file_data <- Luminescence::read_XSYG2R(
    file = file,
    fastForward = TRUE,
    verbose = FALSE
  )

  ## Import info
  file_info <- Luminescence::read_XSYG2R(
    file = file,
    fastForward = TRUE,
    verbose = FALSE,
    import = FALSE
  )

  ## Verify file data and kick out the rest
  verify <- vapply(
    X = file_data,
    FUN = function(v) {
      any(digest::digest(names(v), algo = "md5") %in% verification_hash)
    },
    FUN.VALUE = logical(1)
  )

  ## Kick out what do not belong into the dataset
  if (!all(verify)) {
    if (!any(verify)) {
      showModal(
        modalDialog(
          "All files were excluded, try another file!",
          title = "Verification hash mismatch",
          footer = modalButton("Ok, I'll select another file")
        )
      )
      return(list(data = NULL, info = NULL, verify = verify))
    } else {
      showModal(
        modalDialog(
          "Some of your data do not appear to be Al2O3:C measurement data and were automatically excluded!",
          title = "Verification hash mismatch",
          footer = modalButton("Well, that may happen.")
        )
      )
    }
  }

  ## Replace names by real file names
  for (i in 1:length(unique(file_info$parentID))) {
    file_info$name[grepl(pattern = unique(file_info$parentID)[i], x = file_info$parentID, fixed = TRUE)] <- name[i]
  }

  ## Create file structure object
  # file_structure <- Luminescence::structure_RLum(file_data)

  ## Deconstruct to wheels
  ## Extract needed columns
  if (!is.null(file_names_assignment) && file_names_assignment) {
    wheels <- file_info$name
    unique_names <- unique(wheels)

    for(n in 1:length(unique_names)) {
      wheels[grepl(pattern = unique_names[n], x = wheels)] <- paste0("wheel", n)
    }
  } else {
    wheels <- file_info$position
    for(n in 1:max(table(wheels))){
      wheels[!duplicated(wheels) & !grepl(pattern = "wheel", x = wheels)] <- paste0("wheel", n)
    }
  }

  ## Return wheels
  file_info <- cbind(file_info, wheels = as.character(wheels))

  list(
    data = file_data,
    info = file_info,
    # structure = file_structure,
    verify = verify
  )
}

widget_export_csv <- function(file) {
  js <- "function (key, options) {
           var csv = csvString(this, sep=',', dec='.');
           var link = document.createElement('a');
           link.setAttribute('href', 'data:text/plain;charset=utf-8,' + encodeURIComponent(csv));
           link.setAttribute('download', '%s');
           document.body.appendChild(link);
           link.click();
           document.body.removeChild(link);
         }"
  htmlwidgets::JS(sprintf(js, file))
}
