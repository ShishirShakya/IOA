#' Generates Adjacency Matrix
#'
#' It generates the adjancency matrix based on the layer matrix and binarization on some arbitarily set up cut-off value.
#' The layer matrix having value more than such arbitary cut-off is set to 1 and 0 otherwise.
#' @param A Load the technical-coefficient matrix
#' @param y Load the finaldemand
#' @param n Numbers of rounds
#' @param mfc Minimal flow condition or the cut-off arbitarily choosen to binarize the layer matrix
#'
#' @return The function provides the adjacency matrix in which various graph-theories designs can be implemented.
#'
#' @export
adj = function(A, y, n, mfc){
  power = function(n)
  {
    if (n==1)  return (A)
    if (n==2)  return (A%*%A)
    if (n>2) return ( A%*%power(n-1))
  }
  if(!nrow(A)==nrow(y)){
    stop("Rows of output vector and Techinical Requirement mis-matched")
  }
  if(!ncol(y) == 1){
    stop("The ouput should be a matrix with one column")
  }
  if(n < 0 ){
    stop('Power cannot be negative')
  }

  if(n == 0){
    L0 = A%*%(vec2diag(y))
    diag(L0) = 0
    #return(L0)
    adj = ifelse(L0>mfc, 1, 0)
    return(adj)
  }
  else{
  L0 = A%*%(vec2diag(power(n)%*%(y)))
  diag(L0) = 0
  #return(L0)
  adj = ifelse(L0>mfc, 1, 0)
  return(adj)
  }
}
