#' Check for balances of Input Matrix
#'
#' Check the balances of total commodities inputs and outputs and the balances of industry inputs and outputs.
#' @param make Load the make matrix
#' @param use Load the use matrix
#' @param finaldemand Load the finaldemand matrix
#' @param valueadded Load the valueadded matrix
#'
#' @return Checks if the sums of "row sums of use matrix and row sums of final demand" is same with "column sum of make matrix".
#' @return Checks if the sums of "column sums of use matrix and column sums of valueadded matrix" is same with "row sums of make matrix".
#' @export
#'
IOchecksum = function(make, use, finaldemand, valueadded){

  X = as.matrix(rowSums(make),nrow=nrow(make))
  XT = as.matrix(colSums(use)) + as.matrix(colSums(valueadded))

if(matequal(X,XT) == FALSE ){
    warning("row sums of make matrix is not same as column sum of use and valueadded matrix")
  }else{
    print("row sums of make matrix balances the column sum of use and valueadded matrix")
  }

  QT = as.matrix(rowSums(use)) + as.matrix(rowSums(finaldemand))
  Q =as.matrix(colSums(make))

if(matequal(Q,QT) == FALSE ){
    warning("column sums of make matrix is not same as row sum of use and final demand matrix")
  }else{
    print("column sums of make matrix balances as row sum of use and final demand matrix")
  }
}
