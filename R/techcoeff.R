#' Generates Technical Coefficient
#'
#' It generates the technical coefficient bases on the total requirement matrix.
#' @param IminusAInv Load total requirement matrix.
#' @return The function returns the technical coefficient bases on the total requirement matrix. Note: Make sure to use correct inverse of (I-A).
#' @export
techcoeff = function(IminusAInv){
  if(!nrow(IminusAInv)==ncol(IminusAInv)){
    stop("The requirement matrix has to be square")
  }
  A = -1*(solve(IminusAInv)-diag(nrow(IminusAInv)))
}
