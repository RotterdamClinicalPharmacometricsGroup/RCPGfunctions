#' Providing a last observation carried forward (LOCF) and first observation carried backward (FOCB) on vector
#'
#' @param x vector which includes NAs
#' @param na.value value that contains the values that are overwritten by LOCF or FOCB
#' @return vector overwritten using LOCF and FOCB
#' @export
#'
#' @examples
#' focb_locf(c(NA,NA,1,2,3,5,NA,NA,NA))
#'
#'

focb_locf <- function(x, na.value=NA){
  
  if(sum(is.na(x))==length(x)){
    v <- rep(NA, length(x))
    return(v)
  }
  
  if(is.na(x[1])){
    v <- x[!is.na(x)]
    v <- v[1]
    wh.true <- 1:which(!is.na(x))[1]
    x[wh.true] <- v[1]
  }
  v <- !is.na(x)
  v <- c(NA, x[v])[cumsum(v)+1]
  
  return(v)
}
