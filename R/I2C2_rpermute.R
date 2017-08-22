#' @title Compute I2C2 for Permuted Data
#' @description  Computing the I2C2 of the permuted data.
#' @param s Random seed, Default = 1
#' @param y An n by p data matrix containing n vectorized image data with p voxels.
#' Each row contains one observed image data at a particular visit for one subject.
#' Each column contains image values for all subjects and visits at a particular voxel.
#'
#' The rows are organized by subjects and then visits, EX)
#' (Y11, Y12, Y21, Y22, ... , YI1 , YI2)
#' @param id Vector of IDs, EX) c(1, 1, 2, 2, 3, 3, 4, 4, ... , I, I)
#' @param visit Vector of visits, EX) (1, 2, 1, 2, 1, 2, ... , 1, 2)
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
