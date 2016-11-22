#' Industries Output Proportions Matrix
#'
#' Generates industries output proportions matrix also known as product mix or commodity mix matrix.
#' @param make Load the make matrix
#'
#' @return Generates industries output proportions matrix also known as product mix or commodity mix matrix. Use Latex formula later. The column sum of this matrix thus should be unity. It has commodities-by-industries dimension.
#' @export
getC = function(make){
  C = t(make)%*%solve(vec2diag(as.matrix(rowSums(make))))
  return(C)
}
