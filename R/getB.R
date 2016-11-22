#' Commodity by Industry Direct Requirement
#'
#' Generates commodity by industry direct requirement matrix
#' @param make Load the make matrix
#' @param use Load the use matrix
#'
#' @return Generates commodity by industry direct requirement matrix. Use Latex formula later.
#' @export
getB = function(make, use){
  print("commodity by industry direct requirement")
  B = use%*%solve(vec2diag(as.matrix(rowSums(make))))
  return(B)
}
