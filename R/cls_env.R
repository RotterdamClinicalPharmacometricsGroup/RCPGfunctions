#' Clearing the global environment
#'
#' @param keep.vals Vector of values that should be kept in the global environment
#' @param patterns Vector of patterns that should be kept in the global environment
#' @param rm.self If not TRUE than function returns the values that would have been removed
#' @param rm.show If not TRUE than function returns the values that would have been removed
#'
#' @return  None
#' @export
#'
#' @examples  None
#'

cls_env <- function (keep.vals=NULL, patterns=NULL, rm.self=FALSE, rm.show=FALSE) {
  if(is.null(keep.vals) & is.null(patterns)){stop("Must specify \"keep.vals\" or \"patterns\" or both!")}

  if(length(intersect(keep.vals, patterns)) != 0){stop( c("Objects \"keep.vals\" and \"patterns\" both contain the following values:\n",
                                                          paste(intersect(keep.vals, patterns), collapse = ", ")))}

  rm.vals <- c()

  if(!rm.self){
    if(!is.null(keep.vals)){keep.vals <- c(keep.vals, "clsenv", "keep.vals")}
    if(!is.null(patterns)) {patterns  <- c(patterns , "clsenv", "keep.vals")}
  }

  if(!is.null(keep.vals)){
    rm.vals <- append(rm.vals, ls(envir = globalenv())[!(ls(envir = globalenv()) %in% keep.vals)])
  }

  if(!is.null(patterns)){
    rm.vals <- append(rm.vals, ls(envir = globalenv())[!grepl(paste0(paste0(c("^"), patterns, collapse = "|")), ls(envir = globalenv()))])
  }

  if (rm.show == TRUE) {
    return(rm.vals)
  } else {
    rm(list = rm.vals, envir = globalenv())
  }
}
