#' Calculate ideal body weight
#'
#' @param sex 1 for male and 0 for female, or "M/m" for male and "F/f" for female
#' @param height numeric meters
#'
#' @return None
#' @export
#'
#' @examples
#'   cov_IBW("m", 1.77)
#'


cov_IBW <- function(sex, len){

  if(class(sex)=="character"){
    if(sum(!grepl("^[m|f].*$", sex))>0){stop("Column \"Sex\" contains strange values!")
    } else {
      sex <- ifelse(grepl("^m.*$", sex), 1, 0) # 1 is male, 0 is female
    }
  }

  if(class(sex)=="numeric"){
    vals <- ifelse(sex==0, 45.5, 50) + 2.3 * (len * 39.4 - 60)
    return(vals)
  }
}
