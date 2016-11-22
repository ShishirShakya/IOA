#' Check the feasibilities of Input Matricies
#'
#' Check the consistency of the dimension, existence on numerical values and sum balances of Make, Use, Final Demand and Valueadded Matrix.
#' @param make Load the make matrix
#' @param use Load the use matrix
#' @param finaldemand Load the finaldemand matrix
#' @param valueadded Load the valueadded matrix
#' @export
IOfullcheck = function(make, use, finaldemand, valueadded){
  IOcheckdim(make, use, finaldemand, valueadded)
  IOchecknum(make, use, finaldemand, valueadded)
  IOchecksum(make, use, finaldemand, valueadded)
}
