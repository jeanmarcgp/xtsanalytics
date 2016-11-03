#
#
#  LIST OF FUNCTIONS:
#  ------------------
#   .plot_df   Need to document properly
#

#----------------------------------------------------------------------------------
#  FUNCTION plot_df
#  ----------------
#
#' Plot a data frame or xts matrix to plotting device
#'
#' This function plots a dataframe or anything that can be coerce to a dataframe
#' to the plotting device.  This includes xts matrices.
#' It uses the gridExtra package to plot a dataframe or a matrix, including an
#' xts matrix, to the plot region.
#' The row names are plotted by default.  Only the first 30 rows are plotted.
#'
#' @param df          The dataframe or matrix to plot
#' @param main        Title to put above the table
#' @param footnote    An optional footnote plotted just below the table
#' @param title_size  This is a scaling factor for the title.  Default is 2.0
#' @param scaling     This is a global scaling factor (similar to cex for graphics).
#'                    It scales the size of the entire table.  Default fontsize is 12,
#'                    and scaling by a factor other than 1.0 will scale the fontsize
#'                    accordingly.
#' @param foot_size   A scaling factor for the size of the footnote. Default is 1.0
#' @param Nmax        This is the maximum number of rows to plot.  Normally set to 20.
#'
#' @examples
#' x <- xts_data
#' plot_df(x, main="First 20 rows in xts_data", footnote = "some footnote text here")
#' @export
#  More info on tableGrob, esp. the major changes since ver. 2.0.0
#  http://stackoverflow.com/questions/31776557/how-to-adjust-the-font-size-of-tablegrob
#  vignette(package = 'gridExtra')  3 vignettes available!
#  vignette('tableGrob')
#----------------------------------------------------------------------------------
plot_df <- function(df, main = " ", footnote = " ", title_size = 2.0, foot_size = 1.0,
                    scaling = 1.0, Nmax = 20) {
  N <- nrow(df)
  if(N > Nmax) {
    N <- Nmax
    footnote <- paste("First ", Nmax, " rows shown - ", footnote)
  }
  df2 <- as.data.frame(df[1:N, , drop=F])


  mytheme <- gridExtra::ttheme_default(
    core = list(fg_params = list(cex = 1.0 * scaling)),
    colhead = list(fg_params=list(cex = 1.1 * scaling)),
    rowhead = list(fg_params=list(cex = 1.0 * scaling)))

  mtable <- gridExtra::tableGrob(df2, theme = mytheme)

  mtitle    <- grid::textGrob(main, gp=grid::gpar(fontsize = 12 * scaling * title_size,
                                                  fontface = "bold"))
  mfootnote <- grid::textGrob(footnote, x=0, hjust=0,
                              gp=grid::gpar(fontsize = 10 * scaling * foot_size,
                                            fontface = "italic"))

  padding_t <- grid::unit(1.2, "line")
  padding_f <- grid::unit(0.5, "line")

  mtable  <- gtable::gtable_add_rows(mtable,
                                     heights = grid::grobHeight(mtitle) + padding_t,
                                     pos = 0)
  mtable  <- gtable::gtable_add_rows(mtable,
                                     heights = grid::grobHeight(mfootnote) + padding_f)

  mtable  <- gtable::gtable_add_grob(mtable, list(mtitle, mfootnote),
                                     t=c(1, nrow(mtable)), l=c(1,2),
                                     r=ncol(mtable))

  grid::grid.newpage()
  grid::grid.draw(mtable)

}   ######  END plot_df  ######




