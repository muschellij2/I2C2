#' @title Compute I2C2 for Permuted Data
#' @description  Computing the I2C2 of the permuted data.
#' @param s Random seed, Default = 1
#' @param ... arguments to pass to \code{\link{I2C2}}
#' @return Set of lambda values
#' @export
#' @return Lambda value from \code{\link{I2C2}}
I2C2.rpermute <- function(s = 1, y, id, visit, ...) {

  L = check_id_visit(y = y, id = id, visit = visit)
  y = L$y
  id = L$id
  visit = L$visit

  set.seed(s)
  newI = sample(1:nrow(y), replace = FALSE)
  y = y[newI, ]

  lambda = I2C2(y, id = id, visit = visit, ...)$lambda
  return(lambda)
}
