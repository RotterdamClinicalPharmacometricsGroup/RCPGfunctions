#' Create counter of rows for each ID
#' @description
#' Create counter of rows for each ID
#'
#' @param id_col Values from the (patient) ID column
#' @export
#' @keywords seq_by_id


seq_by_id <- function(id_col){
  ave(1:length(id_col), id_col, FUN=function(x){1:length(x)})
}
