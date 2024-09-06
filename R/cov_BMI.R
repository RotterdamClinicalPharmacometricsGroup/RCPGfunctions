#' calculate body mass index
#'
#' @param weight in kg
#' @param height in meters
#'
#' @return  None
#' @export
#'
#' @examples
#' cov_BMI(79, 1.77)

cov_BMI <- function(weight, height){

  if(sum(height>3)>0){stop("Column \"height\" has to be supplied in meters!")}

  val.BMI <- as.numeric(weight) / (as.numeric(height))**2

  return(val.BMI)

}
