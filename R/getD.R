#' Commodities Output Proportions Matrix
#'
#' Generates commodities output proportions matrix also known as market shares matrix.
#' @param make Load the make matrix
#'
#' @return Generates industries output proportions matrix also known as product mix or commodity mix matrix. Use Latex formula later.It has industries-by-commodities dimension.
#' @export
getD = function(make){
  D = make%*%solve(vec2diag(as.matrix(colSums(make))))
  return(D)
}
