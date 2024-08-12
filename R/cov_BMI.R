#' calculate body mass index
#'
#' @param weight in kg
#' @param len in meters
#'
#' @return  None
#' @export
#'
#' @examples
#'

cov_BMI <- function(weight, len){

  if(sum(len>3)>0){stop("Column \"length\" has to be supplied in meters!")}

  val.BMI <- as.numeric(weight) / (as.numeric(len)/100)**2

  return(val.BMI)

}
