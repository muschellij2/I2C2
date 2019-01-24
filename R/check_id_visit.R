
#' @title Check ID and visit specification for I2C2
#' @description Checks the input for I2C2
#' @param y An n by p data matrix containing n vectorized image data with p voxels.
#' Each row contains one observed image data at a particular visit for one subject.
#' Each column contains image values for all subjects and visits at a particular voxel.
#'
#' The rows are organized by subjects and then visits, EX)
#' (Y11, Y12, Y21, Y22, ... , YI1 , YI2)
#' @param id Vector of IDs, EX) c(1, 1, 2, 2, 3, 3, 4, 4, ... , I, I)
#' @param visit Vector of visits, EX) (1, 2, 1, 2, 1, 2, ... , 1, 2)
#' @export
#'
#' @return List of elements: reordered data, id, visit, I (number of ids),
#' number of rows (n), and number of columns (p)
#' @examples
#' id = c(1:10, 10:1)
#' visit = rep(1:2, each = 10)
#' visit = as.character(visit)
#' n = length(id)
#' p = 100
#' y = matrix(rnorm(n * p), nrow = n, ncol = p)
#' check_id_visit(y =y, id = id, visit = visit)
check_id_visit = function(y, id, visit) {
  p = ncol(y)
  n = nrow(y)
  if (n == 1) {
    stop("only one observation!")
  }
  uid = unique(id)
  I = length(uid)

  if (length(id) != n) {
    stop("Number of ids not equal to number of rows of y")
  }
  if (length(visit) != n) {
    stop("Number of visits not equal to number of rows of y")
  }

  if (is.data.frame(y)) {
    y = as.matrix(y)
  }
  if (!is.matrix(y)) {
    stop("y is not a matrix!")
  }
  if (!typeof(y) %in% c("integer", "double", "logical", "numeric")) {
    stop("y is not a numeric/integer/logical type!")
  }

  # reset the id number to be arithmetic sequence starting from 1
  id <- as.numeric(factor(id))
  visit <- as.numeric(factor(visit))

  # reorder the rows in order of the ids
  ord = order(id, visit)

  # only reorder if needed
  # saving for memory
  if (!all(ord == seq_along(ord))) {
    # order visit
    visit = visit[ord]
    id = id[ord]
    y = y[ord,]
  }

  L = list(y = y, id = id, visit = visit, n = n, p = p,
           I = I)
  return(L)
}