#' Create Diagonal Matrix From Vector
#'
#' Given an input row or column vector, vec2diag returns a diagonal matrix with the input argument along the diagonal.
#'
#' @param x a row or a column vector
#' @return Elemnts of x are placed along the diagonal.
#' @export
vec2diag = function (x)
{
  x <- as.matrix(x)
  if (nrow(x) != 1 && ncol(x) != 1) {
    stop("argument must be a row or column vector")
  }
  if (nrow(x) * ncol(x) == 1) {
    return(x)
  }
  else {
    return(as.matrix(diag(as.numeric(x))))
  }
}
