#' Calculate the adjusted body weight
#'
#' @param val.IBW ideal body weight
#' @param sex 1 for male and 0 for female, or "M/m" for male and "F/f" for female
#' @param len - in meters
#' @param Weight in kg
#'
#' @return  None
#' @export
#'
#' @examples None
#'

ABW <- function(val.IBW=NULL, sex=NULL, len=NULL, Weight=NULL){

  if(is.null(val.IBW)){

    if(is.null(sex)){stop("Or \"sex\" and \"len\" have to be supplied, or \"IBW\" has to be supplied!")}
    if(is.null(len)){stop("Or \"sex\" and \"len\" have to be supplied, or \"IBW\" has to be supplied!")}

    val.IBW <- IBW(sex, len)
  }

  if(is.null(Weight)){stop("Column \"weight\" has to be supplied!")}

  vals <- val.IBW + 0.4 * (as.numeric(Weight) - val.IBW)

  return(vals)

}
