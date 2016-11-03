#####################################################################
#
#  FILE plot_parameters.R
#
#
##############################################################################
#
# Function plot_parameters
#
#' Plot the parameters for a simulation run
#'
#' Plots the parameters for a simulation in its own plotting window.
#' This is normally used as a header page to describe the key
#' parameters for a simulation.  The function provides
#'
#' @param data       Either a list of parameters, or a dataframe containing
#'                   the run parameters to plot on the screen.  If a list,
#'                   each item should be named as the names correspond to the
#'                   parameter name.  If a dataframe, the row names should
#'                   be the parameter names.
#'
#' @param symbols    A character vector of the asset ticker symbols.  This
#'                   set of assets will be printed near the bottom of the
#'                   screen in a matrix format of up to 8 columns and as
#'                   many rows as needed. If omitted (NA), then nothing is
#'                   printed.
#'
#' @param nc         The maximum number of columns to display the set of assets.
#'                   Default is 8.
#'
#' @param main          The main title at the top panel of the screen.  If no
#'                      title is desired, then set to an empty string "".
#'
#'
#' @param symbol_title  The title for the bottom panel where the asset
#'                      symbols are shown.
#'
#' @param exclude_rows  A vector of row numbers or row names corresponding
#'                      to the rows to exclude from showing in the top panel.
#'
#'
#' @export
#---------------------------------------------------------------------------
plot_parameters <- function(data, symbols = NA, nc = 8, main = "Run Parameters",
                            symbol_title = "List of Asset Symbols",
                            exclude_rows = NA ) {


  # # #############################################################
  # #########  Code used for quick testing during development ######
  # library(xtsanalytics)
  # data    = list(job_name     = "my job",
  #                featurevec   = "mom126, sdnaup126, mom189, mom252, something, else, more stuff and more stuff, and then some more, and so on",
  #                type         = "h2o.ensemble",
  #                par1         = "asda asd ggh h sd",
  #                featurelist  = list("mom1234", "sdnaup33", "some item", "somethign else", "yet more stuff"),
  #                # par2         = "asda asd ggh h sd",
  #                # par3         = "asda asd ggh h sd",
  #                # par4         = "asda asd ggh h sd",
  #                # par5         = "asda asd ggh h sd",
  #                # par6         = "asda asd ggh h sd",
  #                # par7         = "asda asd ggh h sd",
  #                # par8         = "asda asd ggh h sd",
  #                # par9         = "asda asd ggh h sd",
  #                # par10         = "asda asd ggh h sd",
  #                # par11        = "asda asd ggh h sd",
  #                par12        = "asda asd ggh h sd"       )
  # symbols = rep(c("SPY", "IEF", "VUSTX", "GOLD", "TLT", "VEIEX"), 3)
  # symbols = "GSPC"
  # symbols = c("SPY", "TLT", "SHY", "GLD")
  # nc      = 8
  # main    = "Plot Title goes here"
  # symbol_title = "List of assets"
  # exclude_rows = "par1"
  # #exclude_rows = c("par12", "type")
  # #exclude_rows = NA

  ####################################################

  #------------------------------------------------------------
  # Prepare the data and asset symbols for plotting
  # Process as a list, then convert to data frame.
  #------------------------------------------------------------
  if(class(data) == "data.frame") data <- as.list(data)
  data <- data[!(names(data) %in% exclude_rows)]

    # unlist all elements in the list
  dlist <- lapply(data, FUN = unlist)

  # concatenate all elements, separated by commas
  dlist2 <- lapply(dlist, FUN = str_c, collapse = ", ")

  # then wrap long strings
  dlist3 <- lapply(dlist2, FUN = str_wrap, width = 40)

  # convert to a dataframe
  df     <- data.frame(as.character(dlist3), row.names = names(dlist3))

  N           <- length(symbols)
  nr          <- ceiling(N / nc)
  width       <- min(N, nc)
  sympad      <- rep(" ", nr * width)
  sympad[1:N] <- symbols
  sym_mat     <- matrix(data = sympad, nrow = nr, ncol = width, byrow = TRUE)

  #-------------------------------------------------------------
  # Set plot region layout as 2 panels, then plot the text!
  #-------------------------------------------------------------
  op <- par(no.readonly = TRUE)
  layout(matrix(c(1, 2)), heights = c(2.5, 1), widths = 1)

  # Plot the parameter table and the main title
  textplot(df, mar = c(0, 0.5, 3.3, 0.5), show.colnames = FALSE,
           cmar = 4, wrap.rownames = 20)
  title(main, cex.main = 1.7)

  # Plot the asset symbols as a matrix
  textplot(sym_mat, show.rownames = FALSE, show.colnames = FALSE,
           mar = c(0, 0.5, 3.0, 0.5))
  title(symbol_title)

  par(op)


}  #######  END FUNCTION plot_parameters  #######

