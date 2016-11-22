#' Check for existence of Non-numerics is Input Matrix
#'
#' Check the existence of Non-numerics in Make, Use, Final Demand and Valueadded Matrix.
#' @param make Load the make matrix
#' @param use Load the use matrix
#' @param finaldemand Load the finaldemand matrix
#' @param valueadded Load the valueadded matrix
#'
#' @return Check the existence of Non-numerics in Make, Use, Final Demand and Valueadded Matrix.
#' @export
IOchecknum = function(make, use, finaldemand, valueadded){
if(!is.numeric(use) == TRUE){
  warning("there are non-numericals in use matrix")
}else{
  print("use matrix has all numericals")
}
if(!is.numeric(make) == TRUE){
    warning("there are non-numericals in make matrix")
  }else{
    print("make matrix has all numericals")
  }

if(!is.numeric(finaldemand) == TRUE){
  warning("there are non-numericals in finaldemand matrix")
}else{
  print("finaldemand matrix has all numericals")
}

if(!is.numeric(valueadded) == TRUE){
  warning("there are non-numericals in valueadded matrix")
}else{
  print("valueadded matrix has all numericals")
}

}
