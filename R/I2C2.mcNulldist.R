#' @title Compute the Null Distribution
#' @description Computing the null distribution of I2C2
#' via permutation and multicore computing.
#'
#' @param ... arguments passed to \code{\link{I2C2.rpermute}}
#' @param rseed Seed number
#' @param R Number of permutations
#' @param mc.cores Number of Cores
#'
#' @export
#' @return List of the lambdas
I2C2.mcNulldist <- function(...,
                      rseed = 1234, R = 500,
                      mc.cores = 1){

  ##	set up the number of multicores
  ##	Set a random seed

  set.seed(rseed)

  lambda <- parallel::mclapply(
    1:R,
    function(s){
      l = I2C2.rpermute(s + rseed, ...)
      return(l)
    }, mc.cores = mc.cores)

  lambda = as.vector(unlist(lambda))
  result <- list(lambda = lambda)
  # result$CI <- quantile( result$lambda, c( (1 - ci)/2,ci + (1 - ci)/2) )
  return(result)
}
