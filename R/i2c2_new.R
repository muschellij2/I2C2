#' @rdname i2c2_new
#' @title Image Intraclass Correlation Coefficient
#' @description Calculate image intraclass correlation
#'  coefficient (I2C2) of balanced/unbalanced data using the trace method
#'
#' @param y An n by p data matrix containing functional responses.
#' Each row contains measurements from a function for one observation at a
#' set of grid points, and each column contains measurements of all
#' functions at a particular grid point.
#'
#' The rows are organized by subjects and then visits, EX)
#' (Y11, Y12, Y21, Y22, ... , YI1 , YI2)
#' @param id Vector of IDs, EX) c(1, 1, 2, 2, 3, 3, 4, 4, ... , I, I)
#' @param visit Vector of visits, EX) (1, 2, 1, 2, 1, 2, ... , 1, 2)
#' @param twoway a logical argument indicating whether a oneway or a twoway
#' functional ANOVA (analysis of variance) decomposition is more
#' appropriate for the problem. "twoway = TRUE" will carry out twoway
#' ANOVA and remove the visit specific mean
#' @param demean if TRUE, include the demean step and
#' output the demeaned dataset
#' @param symmetric if FALSE then the function uses the
#' method of moments estimator formula;
#' if TRUE, pairwise symmetric sum formula, default is FALSE
#' @param truncate if TRUE, set negative I2C2 to zero
#'
#' @return The output of the function is a list that contains the
#' following elements.
#' lambda:       estimated I2C2
#' Kx:           the trace of between-cluster variance operator
#' Ku:           the trace of within-cluster variance operator
#' demean_y:     if demean == TRUE, output the demeaned dataset
#'
#' @author Haochang Shou, Ani Eloyan, Seonjoo Lee, Vadim Zipunnikov,
#' Mary Beth Nebel, Brian Caffo, Martin Lindquist, Ciprian M. Crainiceanu
#' @references
#' Haochang Shou, Ani Eloyan, Seonjoo Lee, Vadim Zipunnikov, Mary Beth Nebel,
#'   Brian Caffo, Martin Lindquist, Ciprian M. Crainiceanu  (2012) The image intra
#' class correlation coefficient for replication studies.
#'
#' @export
#' @examples
#' id = c(1:10, 10:1)
#' visit = rep(1:2, each = 10)
#' visit = as.character(visit)
#' n = length(id)
#' p = 100
#' y = matrix(rnorm(n * p), nrow = n, ncol = p)
#' i2c2(y, id = id, y = y)
i2c2 <- function(
  y,
  id,
  visit,
  symmetric = FALSE,
  truncate = FALSE,
  twoway = TRUE,
  demean = TRUE){


  L = check_id_visit(y = y, id = id, visit = visit)
  n = L$n
  y = L$y
  id = L$id
  visit = L$visit
  p = L$p

  n_I0 = as.numeric(table(id))  # visit number for each id cluster
  k2 = sum(n_I0 ^ 2) # wtf is this?

  ### If demean == TRUE, we calculate the overall mean function and subtract
  ### the mean function from the data
  ### If twoway functional ANOVA is needed ("twoway==TRUE"),  the visit
  ### specific mean function
  ###     and the deviation from the overall mean to visit specific mean
  ### functions are also
  ###     computed.
  demean = as.logical(demean)
  tol = 0
  W <- y

  if (demean) {
    resd = demean_matrix(y = y, visit = visit, twoway = twoway, tol = tol)
    W = resd
  }

  # population average for the demeaned dataset W
  Wdd = colMeans(W)

  # subject-specific sum for the demeaned dataset W
  Ni = !is.na(W)
  class(Ni) = "numeric"
  Ni = rowsum(Ni, group = id)

  Si = rowsum(W, group = id)
  Si = Si / Ni

  # repeat the rows in order for the ids
  Wi = Si[id,]
  ### If symmetric is FALSE, use the method of moments estimator
  ### formula from the manuscript; otherwise, use pairwise symmetric sum estimator

  if (!symmetric) {
    trKu <- sum((W - Wi) ^ 2) / (n - I)
    trKw <- sum((t(W) - Wdd) ^ 2) / (n - 1)
    trKx <- (trKw - trKu) / (1 + (1 - k2 / n) / (n - 1))
  } else {
    trKu <- (sum(W ^ 2 * n_I0[id]) - sum(Si ^ 2)) / (k2 - n)
    trKw <-
      (sum(W ^ 2) * n - sum((n * Wdd) ^ 2) - trKu * (k2 - n)) / (n ^ 2 - k2)
    trKx <-  trKw - trKu
  }

  ## estimated I2C2 values
  lambda <-  trKx / (trKx + trKu)
  if (truncate) {
    lambda[ lambda <= 0] = 0
    ## If trun==TRUE, truncate negative lambdas to 0
  }

  ###  Return the results from I2C2 calculation as a list, with 'lambda' as I2C2 value,
  ###  Kx and Ku being the trace of between and within cluster variance operators;
  ###  If demean == TRUE, also return demeaned data
  L = list(
    lambda = lambda,
    Kx = trKx,
    Ku = trKu
  )
  if (demean) {
    L$demean_y = resd
  }

  return(L)

}

