#' Calculate the adjusted body weight
#'
#' @param val.IBW ideal body weight
#' @param sex 1 for male and 0 for female, or "M/m" for male and "F/f" for female
#' @param height specified in meters
#' @param weight in kg
#'
#' @return  None
#' @export
#'
#' @examples
#' cov_ABW(val.IBW=66.2, weight=58.6)
#'

cov_ABW <- function(val.IBW=NULL, sex=NULL, height=NULL, weight=NULL){

  if(is.null(val.IBW)){

    if(is.null(sex)){stop("Or \"sex\" and \"height\" have to be supplied, or \"IBW\" has to be supplied!")}
    if(is.null(height)){stop("Or \"sex\" and \"height\" have to be supplied, or \"IBW\" has to be supplied!")}

    val.IBW <- cov_IBW(sex, height)
  }

  if(is.null(weight)){stop("Column \"weight\" has to be supplied!")}

  vals <- val.IBW + 0.4 * (as.numeric(weight) - val.IBW)

  return(vals)

}
