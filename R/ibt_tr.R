#' Total Requirement Matrices - Industry Based Tecnhology
#'
#' Based on industry based technology assumptions,this function generates various total requirement matrices on commodities-by-commodities, commodities-by-industries, industries-by-commodities and industries-by-industries.
#' @param make Load the make matrix
#' @param use Load the use matrix
#'
#' @return Generates lists of various industry based technology total requirement matrices.
#' @export
ibt_tr = function(make, use){
  B = use%*%solve(vec2diag(as.matrix(rowSums(make))))
  C = t(make)%*%solve(vec2diag(as.matrix(rowSums(make))))
  D = make%*%solve(vec2diag(as.matrix(colSums(make))))

  ibt_cbc = solve(diag(nrow(B))-B%*%D)
  ibt_cbi = ibt_cbc%*%solve(D)
  ibt_ibc = D%*%ibt_cbc
  ibt_ibi = solve(diag(nrow(B))-D%*%B)

  ibt_mat = list(ibt_cbc, ibt_cbi, ibt_ibc, ibt_ibi)
  names(ibt_mat) = c('ibt_cbc', 'ibt_cbi' , 'ibt_ibc', 'ibt_ibi')
  return(ibt_mat)

}
