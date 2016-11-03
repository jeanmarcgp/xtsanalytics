#
####################################################################################
# FILE plot_rsquare.R
#
#
####################################################################################
#-----------------------------------------------------------------------------------
# FUNCTION plot_rsquare
#
#'  Scatterplot of a regressed predictor vs. actual to visualize ML model performance
#'
#'  This function does a scatter plot of a predicted value (ypred) vs. its
#'  actual value (y). The points are color coded such that if the signs of y and
#'  ypred are the same, the points are green, otherwise they are red.
#'
#'  In addition, a linear regression is performed and the regression line is shown
#'  in red, along with the R-square value in the subtitle.  A line is also plotted in
#'  dotted blue to illustrate the split of data at x = y.
#'
#'  NOTE:  This function assumes that column 1 is y, and column 2 is ypred.
#'
#' @param  xy   A 2 column xts matrix containing y and ypred.
#'
#' @param  main If provided, overrides the default plot title
#'
#' @return The linear regression model is returned
#'
#' @export
#------------------------------------------------------------------------------------
plot_rsquare <- function(xy, main = NA) {
  if(ncol(xy) != 2) stop("plot_rsquare:  argument must have exactly to columns.")

  xy      <- as.data.frame(xy)
  xy      <- xy[complete.cases(xy), ]
  xy$same_sign <- ifelse(sign(xy[, 1]) == sign(xy[, 2]), 1, 0)

  timeframe  <- paste0(index(as.xts(xy[1, ])), " / ", index(as.xts(xy[nrow(xy),])))

  cnames  <- colnames(xy)
  if(is.na(main)) lmtitle <- paste0("Regression of: ", cnames[2], " vs. ", cnames[1]) else
    lmtitle <- main

  # linear regression of xy
  lmformula  <- paste0(colnames(xy)[1], " ~ .")
  xylm       <- lm(lmformula, data=xy[, 1:2])

  adjrsq  <- round(summary(xylm)$adj.r.squared, 3)

  parsave <- par()

  # Add margin to the right for text
  par(mar = c(5, 4, 4, 9) + 0.1)

  plot(xy[which(as.logical(xy$same_sign)), 1:2], col = "darkgreen", pch=20, main = lmtitle,
       sub = paste0("Timeframe: ", timeframe) )

  points(xy[which(!as.logical(xy$same_sign)), 1:2], col = "darkred", pch=20)
  abline(xylm, col = "red", lwd = 2)
  abline(h = 0, lty = "dotted", col = "black")
  abline(v = 0, lty = "dotted", col = "black")

  lines(c(-0.5, 0.5), c(-0.5, 0.5), col = "blue", lty = "dotted")

  legend("topleft", legend = c("Same Sign", "Different Sign", "Regression", "Y = X Line"),
         col = c("darkgreen", "darkred", "red", "blue"), lty = c(0,0,1,3), lwd = c(1,1,3,2),
         pch = c(19, 19, NA, NA), bg = "grey97")

  #---------------------------------------------------------------
  # Compute the Qscore and annotate the right margin with
  # Qscore and adjusted R-square
  #---------------------------------------------------------------
  # ret_score <- Qscore(as.xts(xy[, 1:2]), on = on)
  # martext0  <- "Q-Score:"
  # martext1  <- round(ret_score[5,2], 1)
  martext2  <- "Adjusted"
  martext3  <- "R-square:"
  martext4  <- adjrsq

  # mtext(martext0, side = 4, las = 2, adj = 0.5, line = 4, padj = -4)
  # mtext(martext1, side = 4, las = 2, adj = 0.5, line = 4, padj = -2.5)
  mtext(martext2, side = 4, las = 2, adj = 0.5, line = 4, padj = -1.5)
  mtext(martext3, side = 4, las = 2, adj = 0.5, line = 4, padj =  0.0)
  mtext(martext4, side = 4, las = 2, adj = 0.5, line = 4, padj =  1.5)

  print(summary(xylm))

  par(mar = parsave$mar)

  return(xylm)

}


