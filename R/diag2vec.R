#' Extract Diagonal of a Matrix
#'
#' Given an input matrix, diag2vec returns a column vector of the elements along the diagonal.
#'
#' @param x an input matrix
#' @return Extracts the diagonal of a matrix.
#' @export
diag2vec = function (x)
{
  return(as.matrix(diag(as.matrix(x))))
}
