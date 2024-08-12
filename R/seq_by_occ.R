#' Create counter of rows for each occasion
#' @description
#' Create counter of rows for each occasion
#'
#' @param id_col Values from the (patient) ID column
#' @param occ_col Values from the occasion (OCC) column
#' @export
#' @keywords seq_by_occ
#'

seq_by_occ <- function(id_col, occ_col){
  ave(1:length(id_col), id_col, occ_col, FUN=function(x){1:length(x)})
}
