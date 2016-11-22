#' Generates Layer Matrice Recursively
#'
#' The layer matrix reflects the intermediary flow triggered by the final demand at some N-th round of the process.
#' @param A Load the technical-coefficient matrix
#' @param y Load the finaldemand
#' @param k Numbers of rounds to iterate
#' @param mfc Minimla Flow Control
#' @return This function returns all the rounds of layer matrices and adjacency matrices based on the given minimal flow control parameter. The layer matrix reflects the intermediary flow triggered by the final demand in some N-th round of the process. Therefore
#' they are in absolute volume amonunts and interpretable as delivery flows and comparable to each other.
#'
#' @export
rlayer = function(A, y, k, mfc){
  #x1 <- vector("list", k)
  #x1 = list()
  for(i in 0:k){
    Ps = layer(A, y, i, mfc)
    #x1[[i]] <- Ps
    #x1 = Ps
    print(as.list(Ps))

  }
}
