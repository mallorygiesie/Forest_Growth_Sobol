#'  Forest growth model
#' @param time time since start
#' @param C forest carbon
#' @param parms - as list with three values, r, K, C
#' @param r intrinsic exponential growth rate 
#' @param K carrying capacity (kgC)
#' @param g linear growth rate once above threshold
#' @param threshold canopy closer (kg C)
#' @return derivative of population with time 

dforestgrowth <- function(Time, C, params){
  if (C <= params$threshold){
    dC = params$r * C
  }
  else {
    dC = params$g * (1 - C/params$K)
  }
  
  return(list(dC))
}
