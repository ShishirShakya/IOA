#' Check the Consistency of Dimensions of Input Matrix
#'
#' Check the consistency of the dimension of Make, Use, Final Demand and Valueadded Matrix.
#' @param make Load the make matrix
#' @param use Load the use matrix
#' @param finaldemand Load the finaldemand matrix
#' @param valueadded Load the valueadded matrix
#'
#' @return Checks if the numbers of rows of use and finaldemand matrix are same or not.
#' @return Checks if the numbers of row of use  and numbers of columns of make make matrix are same or not.
#' @return Checks if the numbers of columns of use and numbers of columns of valueadded matrix are same or not.
#' @export
IOcheckdim = function(make, use, finaldemand, valueadded){
  if(!nrow(use) == nrow(finaldemand)){
    warning("number of row of use and finaldemand matrix are not same")
  }else{
    print("number of rows of use and final demand matrix are same")
  }

  if(!nrow(use) == ncol(make)){
    warning("number of rows of use and number of columns of make matrix are not same")
  }else{
    print("number of rows of use and columns of matrix are same")

  }
  if(!ncol(use) == ncol(valueadded)){
    warning("number of columns of use and valueadded matrix are not same")
  }else{
    print("number of columns of use and valueadded matrix are same")

  }
}
