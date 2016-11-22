#' Generates Layer Matrix
#'
#' The layer matrix reflects the intermediary flow triggered by the final demand at some N-th round of the process.
#' @param A Load the technical-coefficient matrix
#' @param y Load the finaldemand
#' @param n Numbers of rounds
#' @param mfc Minimal Flow Condition
#' @return The layer matrix reflects the intermediary flow triggered by the final demand in some N-th round of the process. Therefore
#' they are in absolute volume amonunts and interpretable as delivery flows and comparable to each other.
#'
#' @export
layer = function(A ,y, n, mfc){
  power = function(n)
  {
    if (n==1)  return (A)
    if (n==2)  return (A%*%A)
    if (n>2) return ( A%*%power(n-1))
  }
  if (!nrow(A)==nrow(y)){
    stop("Rows of output vector and Techinical Requirement mis-matched")
  }
  if (!ncol(y) == 1){
    stop("The ouput should be a matrix with one column")
  }
  if(n < 0 ){
    stop('Power cannot be negative')
  }
  if(n == 0){
    L0 = A%*%(vec2diag(y))
    diag(L0) = 0
    adj = ifelse(L0>mfc, 1, 0)
    lay = list(L0, adj)
    names(lay) = c('layer0', 'adj0')
    return(lay)
  }
  else{
    L0 = A%*%(vec2diag(power(n)%*%(y)))
    diag(L0) = 0
    adj = ifelse(L0>mfc, 1, 0)
    lay = list(L0, adj)
    names(lay) = c(paste0("layer", n), paste0("adj", n))
    return(lay)
  }
}
