#' Calculate BSA with different methods
#'
#' @param weight weight in kg
#' @param height height in m
#' @param method method to calculate BSA: mosteller, dubois, haycock
#' @keywords BSA, mosteller, dubois, haycock
#' @export
#' @examples
#' cov_BSA(70, 1.70, method="mosteller")
#'

cov_BSA <- function(weight, height, method){

  if(sum(height>3)>0){stop("Column \"height\" has to be supplied in meters!")}

  height <- height*100 #from m to cm

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
