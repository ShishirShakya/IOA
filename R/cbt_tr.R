#' Total Requirement Matrices - Commodity Based Tecnhology
#'
#' Based on commodity based technology assumptions,this function generates various total requirement matrices on commodities-by-commodities, commodities-by-industries, industries-by-commodities and industries-by-industries.
#' @param make Load the make matrix
#' @param use Load the use matrix
#'
#' @return Generates lists of various commodity based technology total requirement matrices.
#' @export
cbt_tr = function(make, use){
  B = use%*%solve(vec2diag(as.matrix(rowSums(make))))
  C = t(make)%*%solve(vec2diag(as.matrix(rowSums(make))))
  D = make%*%solve(vec2diag(as.matrix(colSums(make))))

  cbt_cbc = solve(diag(nrow(B))-B%*%solve(C))
  cbt_cbi = cbt_cbc%*%C
  cbt_ibc = cbt_cbc%*%solve(C)
  cbt_ibi = solve(diag(nrow(B))-solve(C)%*%B)

  cbt_mat = list(cbt_cbc, cbt_cbi, cbt_ibc, cbt_ibi)
  names(cbt_mat) = c('cbt_cbc', 'cbt_cbi' , 'cbt_ibc', 'cbt_ibi')
  return(cbt_mat)

}
