#
#
# xtsparallel.R
#
# Starts and stops the parallel backend to enable using
# foreach
#----------------------------------------------------------------------------------
#' Starts the parallel backend.
#'
#' Starts the parallel backend cluster for use by the foreach function.
#'
#' @param Ncores   A positive number specifies the number of cores to use by
#'                 the parallel backend.  0 means all cores.  A negative number
#'                 means all cores minus that number, so that the computer doesn't
#'                 "freeze up" while processing. Default is -1, so that one CPU core
#'                 remains unused.
#' @return The parallel CPU cluster is returned and should be assigned to a variable
#' @seealso stop_parallel
#' @export
start_parallel <- function(Ncores = -1) {

  if(Ncores <= 0) Clcores <- parallel::detectCores() + Ncores else
    Clcores <- Ncores

  cl     <- parallel::makeCluster(Clcores)
  doParallel::registerDoParallel(cl, cores = Ncores)

  sprint("Initiating Parallel Backend.")
  sprint("  => Number of cores in cluster: %s", getDoParWorkers())
  sprint("  => Name of parallel backend: %s", getDoParName())

  return(cl)
}

#----------------------------------------------------------------------------------
#' Stops the parallel backend
#'
#' Stops a CPU cluster previously started using start_parallel.  The cluster
#' must be provided for this function to work.
#'
#' @param cluster  The CPU cluster to stop
#'
#' @return Nothing is returned.
#' @seealso start_parallel
#' @export
stop_parallel <- function(cluster) {
  sprint("Stopping Parallel Backend.")
  parallel::stopCluster(cluster)
}
