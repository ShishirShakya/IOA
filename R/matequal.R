#' Check If Two Matrix Are Same or Not.
#'
#' Considers two matrix x and y and checks if the matrix are identical in term of dimension and elements or not.
#'
#' @param x is a matrix
#' @param y is another matrix to be tested if it's same with x or not.
#' @return TRUE if matrix are identical and FALSE otherwise.
#' @export
matequal <- function(x, y)
  is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)
