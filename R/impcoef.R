#' Generates Importance Coefficient
#'
#' It generates the importance coefficient bases on the total requirement matrix.
#' @param IminusAInv Load total requirement matrix.
#' @param y Load the output vector
#' @param supress An arbitary cutoff
#' @return The function returns the importance coefficient bases on the total requirement matrix. The algorithm follows Aroche-Reyes, 1996; Schintke & Staglin, 1988. Thus tracks the importance between i-th sector and j-sector.
#' @export
impcoef = function(IminusAInv, y, supress = 0){
  if(!nrow(IminusAInv) == nrow(y)){
    stop("Rows of technical coefficient mismatched to industry output")
  }
  A = -1*(solve(IminusAInv)-diag(nrow(IminusAInv)))
  dy = replicate(nrow(y), as.vector(y))
  dx = t(y)
  tibytj = sweep(dy, 2, dx, FUN="/")
  tibytj[tibytj<0] = 0
  return(tibytj)

  IC = solve(A*(IminusAInv+IminusAInv*tibytj))
  IC = ifelse(IC < supress, 1, 0)
  #IC

}
