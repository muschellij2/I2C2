#' @title Compute the Convidence Interval
#' @description Computing the confidence interval of I2C2 via multicore computing.
#'
#' @param y Dataset (Y11, Y12, ... , YnJn)' => (IJ)-by-p matrix for balanced case, EX) (Y11, Y12, Y21, Y22, ... , YI1 , YI2)
#' @param ... arguments passed to \code{\link{I2C2.rsample}}
#' @param rseed Seed number
#' @param R The bootstrap repetition size
#' @param mc.cores Number of Cores
#' @param ci 100*ci\% The level of the Confidence Interval
#'
#' @export
#' @return List of the lambdas and then the confidence interval
#'
I2C2.mcCI <- function(y, ...,
                      rseed = 1234, R = 100,
                      mc.cores = 1, ci = 0.95){

  args = list(...)
  id = args$id
  visit = args$visit
  I = args$I
  J = args$J
  ############
  if (  (is.null(id) | is.null(visit) ) && ( is.null(I) | is.null(J) )  ){
    stop("Not enough information! Please provide (id, visit) or (I,J) !")
  }

  ##	set up the number of multicores
  ##	Set a random seed

  set.seed(rseed)

  lambda <- parallel::mclapply(
    1:R,
    function(s){
      l = I2C2.rsample(s + rseed, y = y, ...)
      return(l)
    }, mc.cores = mc.cores)

  lambda = as.vector(unlist(lambda))
  result <- list(lambda = lambda)
  result$CI <- quantile( result$lambda, c( (1 - ci)/2,ci + (1 - ci)/2) )
  return(result)
}
