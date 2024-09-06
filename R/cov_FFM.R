#' Caculate the fat free mass (FFM)
#'
#' @param weight numeric
#' @param height numeric
#' @param gender 1 for male and 0 for female, or "M/m" for male and "F/f" for female
#' @param age numeric
#'
#' @return None
#' @export
#'
#' @examples
#'   cov_FFM(70, 1.77, gender="m")
#'
#' @details
#' Functions from https://academic.oup.com/jac/article/73/8/2104/4995076
#'

cov_FFM <- function(weight, height, gender, age=NULL){

  # Convert height and weight to numeric values
  height <- as.numeric(height)
  weight <- as.numeric(weight)


  # recalculate
  height <- height*100

  ## Functions from https://academic.oup.com/jac/article/73/8/2104/4995076

  # Calculate FFM based on gender and age
  if(is.null(age)){

    # Set WHS_max and WHS_50 based on gender
    WHS_max <-  ifelse(gender %in% 1 | tolower(gender) %in% "m", 42.92, 37.99) # 1 =male
    WHS_50  <-  ifelse(gender %in% 1 | tolower(gender) %in% "m", 30.93, 35.98) # 1 =male

    # Calculate FFM using the formula: WHS_max * height^2 * (weight / (WHS_50 * height^2) + weight)
    val.ffm <-  WHS_max * height**2 * (weight / (WHS_50 * height**2) + weight)

  } else {

    # Convert age to numeric value
    age <- as.numeric(age)

    # Calculate FFM based on gender, age, height, and weight
    val.ffm <- ifelse(gender %in% 1 | tolower(gender) %in% "m",  # 1 =male
                      0.88 + ((1 - 0.88)/(1 + (age/13.4)**(-12.7))) + (9270 * weight)/(6680 + (216 * (weight/height**2))),
                      1.11 + ((1 - 1.11)/(1 + (age/7.1 )**(-1.1 ))) + (9270 * weight)/(8780 + (244 * (weight/height**2)))
    )
  }

  # Return the calculated FFM value
  return(val.ffm)

}
