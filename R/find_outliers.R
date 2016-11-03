#########################################
#
# find_outliers.R
#
#########################################
#
#' Find outliers in a distribution and optionally removes them
#'
#' Although removing outliers is normally a bad idea, in some
#' cases this can be useful when the interest lies in the shape
#' of the main distribution.  This function finds these outliers
#' and removes them if specified so.
#'
#' This function needs to be properly documented...
#'
#'
#'
#' @export
find_outliers <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  sprint("Outliers identified:        %g", na2 - na1)
  sprint("Proportion (%%) of outliers: %g", round((na2 - na1) / sum(!is.na(var_name))*100, 1))
  sprint("Mean of the outliers:       %g", round(mo, 3))
  m2 <- mean(var_name, na.rm = T)
  sprint("Mean without removing outliers: %g", round(m1, 5))
  sprint("Mean if we remove outliers:     %g", round(m2, 5))
  #response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  response <- "y"
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    sprint("Outliers successfully removed")
    return(invisible(dt))
  } else{
    sprint("Nothing changed")
    return(invisible(var_name))
  }
}
