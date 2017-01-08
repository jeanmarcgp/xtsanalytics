#
# update_xtsanalytics.R
#
############################################################
#
#' Function to update xtsanalytics from github
#'
#' A simple function to update package xtsanalytics from
#' github if a new version is available.
#'
#' @export
#----------------------------------------------------------
update_xtsanalytics <- function() {

  library(drat)
  addRepo("jeanmarcgp")

  update.packages(oldPkgs = "xtsanalytics")

}
