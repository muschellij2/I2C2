#' @title Compute I2C2 for Permuted Data
#' @description  Computing the I2C2 of the permuted data.
#' @param s Random seed, Default = 1
#' @param y An n by p data matrix containing functional responses.
#' Each row contains measurements from a function for one observation at a
#' set of grid points, and each column contains measurements of all
#' functions at a particular grid point.
#'
#' The rows are organized by subjects and then visits, EX)
#' (Y11, Y12, Y21, Y22, ... , YI1 , YI2)
#' @param id Vector of IDs, EX) c(1, 1, 2, 2, 3, 3, 4, 4, ... , I, I)
#' @param visit Vector of visits, EX) (1, 2, 1, 2, 1, 2, ... , 1, 2)
#' @param ... arguments to pass to \code{\link{I2C2}}
#' @return Set of lambda values
#' @export
#' @examples
#' id = c(1:10, 10:1)
#' visit = rep(1:2, each = 10)
#' visit = as.character(visit)
#' n = length(id)
#' p = 100
#' y = matrix(rnorm(n * p), nrow = n, ncol = p)
#' L = resample_id(s = 10, y = y, id = id, visit = visit)
I2C2.rsample <- function(s = 1, y, id, visit, ...) {

  L = resample_id(s = s, y = y, id = id, visit = visit)
  id = L$id
  y = L$y
  visit = L$visit

  lambda = I2C2(y, id = id, visit = visit, ...)$lambda
  return(lambda)
}

#' @rdname I2C2.rsample
#' @export
resample_id = function(s = 1, y, id, visit) {

  L = check_id_visit(y = y, id = id, visit = visit)
  y = L$y
  id = L$id
  visit = L$visit

  uid = unique(id)
  nid = length(uid)

  set.seed(s)
  df = data.frame(id = id, visit = visit, ind = seq(id))

  ###################################
  # Sample the new ids
  ###################################
  newI = sort(sample(uid, nid, replace = TRUE))
  newI = as.data.frame(table(id = newI))
  newI = split(newI, newI$id)
  newI = lapply(newI, function(x) {
    x = x[ rep(1, x$Freq), ]
    x$index = seq(nrow(x))
    x$Freq = NULL
    x
  })
  newI = do.call("rbind", newI)
  newI$newid = seq(nrow(newI))

  df = merge(newI, df, all.x = TRUE, sort = FALSE)

  id = df$newid
  visit = df$visit
  y = y[ df$ind,]

  L = list(y = y,
           id = id,
           visit = visit,
           resample = df,
           seed = s
  )
  return(L)
}