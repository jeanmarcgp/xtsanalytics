#
#  roll_bootstrap.R
#
#
#


#'
#' Generates a rolling window bootstrap index vector to synthesize a random series
#'
#' This function generates an index vector that can be used to
#' subset an xts matrix in a rolling window random fashion.
#' For speed, the underlying code uses Rcpp to generate the random
#' vectors.
#'
#' The function is used to create synthetic time series by reordering
#' the returns randomly in a rolling window fashion.
#'
#' The implication is any autocorrelation between consecutive or near
#' consecutive return samples will be randomly shuffled.  However, the
#' heteroscedastic properties of the series over longer time frames is
#' mostly maintained.
#'
#'
#'
#' @param N    The length of the vector we wish to generate
#' @param k    The rolling window width
#' @param nc   The number of columns to generate (number of vectors)
#'
#' @return Returns a numeric matrix of N rows by nc columns. Consider using
#'         apply to then apply these index to generate an equivalent xts matrix.
#'         See examples to understand how to do this.
#'
#'
#' @export
roll_bootstrap <- function(N, k = 3, nc = 1) {

  Rcpp::cppFunction('NumericVector bootvec_cpp(NumericVector vec,
                                               NumericVector rvec,
                                               int N, int k)
                   {
                     int i = 0;

                     for(i = k; i<N; i++)
                     {
                       rvec[i] = rand() % k;
                       while((rvec[i] - 1) == rvec[i-1])
                       {
                         // printf("old rvec[%i] = %1.0f ", i, rvec[i]);
                         rvec[i] = rand() % k;
                         // printf("new rvec[%i] = %1.0f :: ", i, rvec[i]);
                       }
                      // printf("::rvec[%i] = %1.0f ::", i, rvec[i]);
                       vec[i]  = i - rvec[i];
                      // printf("::vec[%i] = %1.0f ::", i, vec[i]);
                     }
                     return(vec);
                   }')

  # Use the Rcpp function to generate a vector of rolling indices
  rvec <- rep(0, N)
  vec_cpp <- 0:(N-1)
  vec_mat <- matrix(rep(vec_cpp, nc), nrow = N, ncol = nc)

  vec_mat <- apply(vec_mat, 2, bootvec_cpp, rvec, N, k)
  #vec_mat <- bootvec_cpp(vec_cpp, rvec, N, k)
  vec_mat <- vec_mat + 1

  return(vec_mat)


}

# print(rvec_cpp)
#  vec  <- 1:N
#   for(i in (k+1):N) {
#
#     rvec[i] = sample(0:(k-1), 1)
#     while(rvec[i] - 1 == rvec[i-1]) {
#       rvec[i] = sample(0:(k-1),1)
#       #if(i == (k+1)) sprint("new rvec[%i] == %i", i, rvec[i])
#     }
#
#     vec[i]  <- i - rvec[i]
#
#   }

