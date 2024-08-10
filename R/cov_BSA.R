#' Calculate BSA with different methods
#'
#' @param weight weight in kg
#' @param height height in cm
#' @param method method to calculate BSA: mosteller, dubois, haycock
#' @keywords BSA, mosteller, dubois, haycock
#' @export
#' @examples  None
#'
#'

cov_BSA <- function(weight,height,method){
  if(tolower(method) == 'mosteller'){
    BSA <- sqrt((height*weight)/3600)
  }
  else if(tolower(method) == 'dubois'){
    BSA <- 0.007184 * weight^0.425 * height^0.725
  }
  else if(tolower(method) == 'haycock'){
    BSA <- 0.024265 * weight^0.5378 * height^0.3964
  }
  else{
    BSA <- "unknown method"
  }
  return(BSA)
}
