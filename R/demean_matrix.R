
#' @title Worker function for demeaning a matrix
#' @description De-means a matrix based on the column means or the
#' visit-specific means.
#'
#' @param y n by p matrix of data
#' @param visit Vector of visits, EX) (1, 2, 1, 2, 1, 2, ... , 1, 2)
#' @param twoway a logical argument indicating whether a oneway or twoway
#' mean subtraction is more appropriate for the problem. "twoway = TRUE"
#' will remove both subject specific and visit specific means
#' @param tol tolerance for matching visits.
#'
#' @return Matrix of the same size as y
#' @export
#'
#' @examples
#' id = c(1:10, 1:10)
#' visit = rep(1:2, each = 10)
#' n = length(id)
#' p = 100
#' y = matrix(rnorm(n * p), nrow = n, ncol = p)
#' demean_matrix(y, visit = visit)
demean_matrix = function(y, visit, twoway = TRUE, tol = 0) {

  p = ncol(y)
  mu <- colMeans(y)
  resd <- t(y) # not defined yet

  if (twoway) {
    # get unique visits
    uvisit = sort(unique(visit))
    eta <- matrix(0, length(uvisit), p)

    for (ivisit in seq_along(uvisit)) {
      j = uvisit[ivisit]

      ###########################
      # Maybe include a tolerance measure
      ###########################
      if (tol == 0) {
        ind = visit == j
      } else {
        ind = (visit - j) <= tol
      }
      # grab these visits
      mat = y[ ind, , drop = FALSE]
      visit_mean = colMeans(mat)
      eta[ ivisit, ] = visit_mean - mu


      ### Calculate residuals by subtracting visit-specific mean from
      ### original functions for'twoway == TRUE', or subtracting
      ### overall mean function for 'twoway == FALSE'.
      resd[, ind] = resd[, ind] - visit_mean
    }
    resd = t(resd)
  } else {
    resd <- t(resd - mu) #twoway == FALSE
  }
  return(resd)
}
