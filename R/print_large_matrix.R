#' Print method for `demeaned_matrix`
#'
#' @return NULL
#' @param x an object used to select a method.
#' @param ... further arguments passed to or from other methods
#' @param reveal Should the `demeaned_matrix` be revealed
#' @export
#'
#' @examples
#' x = I2C2::mask10
#' class(x) = "demeaned_matrix"
#' print(x)
#' print(x, reveal = TRUE)
#' @method print demeaned_matrix
print.demeaned_matrix = function(x, reveal = FALSE, ...) {
  if (reveal) {
    class(x) = setdiff(class(x), "demeaned_matrix")
    print(x, ...)
  } else {
    d = dim(x)
    d = paste(d, collapse = "x")
    cat(paste0("<a large matrix of dim:", d, ">"))
  }
}
