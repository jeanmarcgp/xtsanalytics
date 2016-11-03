####################################################################################
# FILE wfo_grow_jungle.R
#
#
####################################################################################
# FUNCTION wfo_grow_jungle
#
#' Parallel version of wfo_grow_forest
#'
#' TODO write the documentation!!!
#'
#' @export
#-----------------------------------------------------------------------------------
wfo_grow_jungle <- function(featuremat, modelwindow = 252, wfo_span = "months",
                            ylag = 21, Nblocks = 7, SPwindow = 63, verbose = FALSE,
                            mtry = 2, ntree = 1000, importance = TRUE,
                            na.action = na.omit, ...) {

  #------------------------------------------------
  # Remove leading NAs in featuremat
  #------------------------------------------------
  featuremat <- zoo::na.trim(featuremat, sides = "left", is.na = "any")

  wfo_data   <- wfo_timeframe(featuremat = featuremat, modelwindow = modelwindow,
                              wfo_span = wfo_span, ylag = ylag, earliest = earliest,
                              verbose = verbose)
  wfo_points <- wfo_data$wfo_points
  Nlast      <- wfo_data$Nlast
  Ndates     <- length(wfo_points)


  #---------------------------------------------------------
  # Break wfo_points into date_blocks, a list of
  # vectors of the wfo_span dates.
  #---------------------------------------------------------
  block_size  <- Ndates %/% Nblocks
  if((block_size * Nblocks) < Ndates) block_size <- block_size + 1

  jobnames    <- NULL
  date_blocks <- list()
  for(i in 1:Nblocks) {
    # Create date blocks list
    block_beg         <- (i - 1) * block_size
    block_end         <- i * block_size
    dates_i           <- wfo_points[block_beg:block_end]
    dates_i           <- dates_i[!is.na(dates_i)]
    date_blocks[[i]]  <- index(featuremat[dates_i, ])

    # Create job names vector
    jobnames[i]       <- paste0("Job_", i)
  }

  #---------------------------------------------------------------
  # Start/stop parallel cluster and foreach function call
  #---------------------------------------------------------------
  clust <- start_parallel(Ncores = -1)
  mp <- foreach(i = 1:Nblocks, .combine = 'c', .inorder = FALSE,
                .packages = c("xtsanalytics", "randomForest", "xts")) %dopar% {
    wfo_grow_forest(featuremat = featuremat, modelwindow = modelwindow,
                    wfo_span = date_blocks[[i]], ylag = ylag, SPwindow = SPwindow,
                    mtry = mtry, jobname = jobnames[i], ntree = ntree,
                    importance = importance, na.action = na.action, verbose = verbose)

  }  #### END foreach ####

  stop_parallel(clust)

  #---------------------------------------------------------------
  # Combine results into a single list
  #---------------------------------------------------------------
  pred      <- mp[[1]][["pred"]]
  wfo_dates <- mp[[1]][["wfo_dates"]]
  if(Nblocks > 1) {
    for(i in 2:Nblocks) {
      pred      <- rbind.xts(pred, mp[[i]][["pred"]])
      wfo_dates <- c(wfo_dates, mp[[i]][["wfo_dates"]])
    }
    wfo_dates   <- wfo_dates[order(wfo_dates, decreasing = FALSE)]
  }

  #---------------------------------------------------------------
  # Return results
  #---------------------------------------------------------------
  results  <- list(pred = pred, wfo_dates = wfo_dates)

  return(results)

}
