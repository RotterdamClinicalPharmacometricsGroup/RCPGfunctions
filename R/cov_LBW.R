#' Calculate lean body weight
#'
#' @param sex 1 for male and 0 for female, or "M/m" for male and "F/f" for female
#' @param weight - specified in kg
#' @param val.BMI - specified in kg/m2
#' @param height - specified in meters
#'
#' @return None
#' @export
#' @examples
#'   cov_LBW("m", 120, val.BMI=34)
#'

cov_LBW <- function(sex, weight, val.BMI=NULL, height=NULL){

  if(is(sex, "character")){
    if(sum(!grepl("^[m|f].*$", sex))>0){stop("Column \"Sex\" contains strange values!")
    } else {
      sex <- ifelse(grepl("^m.*$", sex), 1, 0) # 1 is male, 0 is female
    }
  }


  if(is.null(height)){

    if(is.null(val.BMI)){stop("Or column \"BMI\" or column \"heightgth\" has to be supplied.")
    } else {

      #Source: https://clincalc.com/Kinetics/IdealBW.aspx
      val.LBW <- ifelse(as.numeric(sex)==0,
                        (9.27 * 10^3 * as.numeric(weight)) / (6.68 * 10^3 + (216 * val.BMI)),
                        (9.27 * 10^3 * as.numeric(weight)) / (8.78 * 10^3 + (244 * val.BMI)))

    }
  } else {

    #Source: https://clincalc.com/Kinetics/IdealBW.aspx
    val.LBW <- ifelse(as.numeric(sex)==0,
                      (9.27 * 10^3 * as.numeric(weight)) / (6.68 * 10^3 + (216 * cov_BMI(weight, height))),
                      (9.27 * 10^3 * as.numeric(weight)) / (8.78 * 10^3 + (244 * cov_BMI(weight, height))))
  }

  return(val.LBW)
}

