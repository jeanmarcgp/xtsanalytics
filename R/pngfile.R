###################################################################################
#
#  FUNCTIONS IN THIS FILE:
#     pngfile.R
#
###################################################################################


#----------------------------------------------------------------------------------
#  FUNCTION pngfile
#
#' Function to ease the creation of a png graphic file
#'
#' This function is used to open the png file or to close the png file.
#' One opened, plots normally sent to the console will be sent to the
#' png file instead.
#'
#'
#'
#' @param onoff     A character string to denote whether the file in opened
#'                  "on" or closed "off". This must be specified.
#'
#' @param outflag   Specifies whether the graphical output is sent to the
#'                  console or the file.  This is useful in a script to
#'                  easily control where the output is sent when several
#'                  charts are created (send them all to file or the console).
#'
#' @param fname     The file name for the output file.
#'
#' @param fpath     The path of the output file.
#'
#' @param width     The width (in pixels) of the chart.
#'
#' @param height    The height (in pixel) of the chart.
#'
#'
#' @return Nothing is returned per se.  This function opens/close the
#'         output channel.
#'
#' @export
#----------------------------------------------------------------------------------
pngfile <- function(onoff, outflag, fname = NA, fpath = NA,
                    width = 1600, height = 1000) {
  # outflag = 1:  screen console. outflag = 2: png file
  if(outflag == 2) {
    if(onoff == "on") {
      png(paste0(fpath, fnamestamp(paste0(fname, ".png"))),
          width = width, height = height)
    } else {
      dev.off()
    }
  }
}
