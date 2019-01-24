
#' @title Worker function for demeaning a matrix
#' @description De-means a matrix based on the column means or the
#' visit-specific means.
#'
#' @param y n by p matrix of data
#' @param visit Vector of visits, EX) (1, 2, 1, 2, 1, 2, ... , 1, 2)
#' @param twoway a logical argument indicating whether a oneway or
#' twoway
#' mean subtraction is more appropriate for the problem. If FALSE,
#' only the overall sample
#' mean will be removed only; if TRUE, it will also remove visit
#' specific means to
#' avoid scanner or batch effects
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
#' result = demean_matrix(y, visit = visit)
#' result.oneway = demean_matrix(y, visit = visit, twoway = FALSE)
#'
#' testthat::expect_warning({
#' result.old = demean_matrix.Deprecated(y, visit = visit)
#' })
#' testthat::expect_warning({
#' result.old_oneway = demean_matrix.Deprecated(y,
#' visit = visit,
#' twoway = FALSE)
#' })
#' stopifnot(
#' isTRUE(all.equal(
#' result.oneway, result.old_oneway)
#' ))
#'
#' stopifnot(
#' isTRUE(all.equal(result.old, result))
#' )
demean_matrix = function(y, visit, twoway = TRUE, tol = 0) {

  # p = ncol(y)
  # mu <- colMeans(y)
  y = scale(y, center = TRUE, scale = FALSE)
  attr(y, "scaled:center") = NULL

  if (twoway) {
    resd <- y # not defined yet
    # get unique visits
    uvisit = sort(unique(visit))
    # eta <- matrix(0, length(uvisit), p)

    ##########
    # ETA ISN'T USED HERE
    ########3
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
      # mat = y[ ind, , drop = FALSE]
      # visit_mean = colMeans(y[ ind, , drop = FALSE])
      # eta[ ivisit, ] = visit_mean - mu


      ### Calculate residuals by subtracting visit-specific mean from
      ### original functions for'twoway == TRUE', or subtracting
      ### overall mean function for 'twoway == FALSE'.
      # mat = y[ ind, , drop = FALSE]
      mat = resd[ ind, , drop = FALSE]
      mat = scale(mat, scale = FALSE, center = TRUE)
      resd[ind, ] = mat
      # resd[, ind] = resd[, ind] - visit_mean
    }
    # y = resd
    return(resd)
    # resd = t(resd)
  }
  # else {
  #   y = scale(y, center = mu, scale = FALSE)
  #   attr(y, "scaled:center") = NULL
  #   # resd <- t(resd - mu) #twoway == FALSE
  # }
  return(y)
}


#' @export
#' @rdname demean_matrix
demean_matrix.Deprecated = function(y, visit,
                                    twoway = TRUE, tol = 0) {

  .Deprecated("demean_matrix", package = "I2C2")
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