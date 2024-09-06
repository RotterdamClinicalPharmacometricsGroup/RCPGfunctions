
#' Function for obtaining BOOTSTRAP data
#'
#' @param fpath path to "bootstrap_results.csv"
#'
#' @return None
#' @export
#'
#'


bootstrap_data <- function(fpath){

  txt <- readLines(file.path(fpath, "bootstrap_results.csv"))
  txt <- gsub('(")+','',txt)

  wh.begdat1 <- which(grepl("^diagnostic.means", txt))
  wh.begdat2 <- which(grepl("^means", txt))
  wh.begdat3 <- which(grepl("^bias", txt))
  wh.begdat4 <- which(grepl("^standard.error.confidence.intervals", txt))
  wh.begdat5 <- which(grepl("^standard.errors", txt))
  wh.begdat6 <- which(grepl("^medians", txt))
  wh.begdat7 <- which(grepl("^percentile.confidence.intervals", txt))

  wh.enddat1 <- wh.begdat1 + 2
  wh.enddat2 <- wh.begdat2 + 2
  wh.enddat3 <- wh.begdat3 + 2
  wh.enddat4 <- wh.begdat4 + 9
  wh.enddat5 <- wh.begdat5 + 2
  wh.enddat6 <- wh.begdat6 + 2
  wh.enddat7 <- wh.begdat7 + 9

  txt.diag <- txt[(wh.begdat1+2):wh.enddat1]
  txt.mean <- txt[(wh.begdat2+2):wh.enddat2]
  txt.bias <- txt[(wh.begdat3+2):wh.enddat3]
  txt.stci <- txt[(wh.begdat4+2):wh.enddat4]
  txt.ster <- txt[(wh.begdat5+2):wh.enddat5]
  txt.medi <- txt[(wh.begdat6+2):wh.enddat6]
  txt.pcci <- txt[(wh.begdat7+2):wh.enddat7]

  txt.head_diag <- txt[(wh.begdat1+1)]
  txt.head_othr <- txt[(wh.begdat2+1)]

  # Correlations are delimited with comma, change to lower_dash before splitting
  txt.head_othr <- gsub('(OMEGA)\\((\\d{1,}),(\\d{1,}))',"\\1(\\2_\\3)", txt.head_othr)
  txt.head_othr <- gsub('(SIGMA)\\((\\d{1,}),(\\d{1,}))',"\\1(\\2_\\3)", txt.head_othr)

  txt.head_diag <- unlist(lapply(strsplit(txt.head_diag, split=','), FUN=function(w){trimws(w)}))
  txt.head_othr <- unlist(lapply(strsplit(txt.head_othr, split=','), FUN=function(w){trimws(w)}))

  txt.head_diag <- gsub("\\\\s", ".", txt.head_diag)
  txt.head_othr <- gsub("\\\\s", ".", txt.head_othr)

  # Delete begining with se. or shrinkage or EI
  txt.head_othr <- txt.head_othr[!grepl("^se|^shrinkage|^EI", txt.head_othr)]

  txt.diag <- suppressWarnings(as.numeric(unlist(lapply(strsplit(txt.diag, split=','), FUN=function(w){trimws(w)})))[-1])
  txt.mean <- as.numeric(unlist(lapply(strsplit(txt.mean, split=','), FUN=function(w){trimws(w)})))[-1]
  txt.bias <- as.numeric(unlist(lapply(strsplit(txt.bias, split=','), FUN=function(w){trimws(w)})))[-1]
  txt.ster <- as.numeric(unlist(lapply(strsplit(txt.ster, split=','), FUN=function(w){trimws(w)})))[-1]
  txt.medi <- as.numeric(unlist(lapply(strsplit(txt.medi, split=','), FUN=function(w){trimws(w)})))[-1]

  txt.stci <- lapply(strsplit(txt.stci, split=','), FUN=function(w){trimws(w)})
  txt.pcci <- lapply(strsplit(txt.pcci, split=','), FUN=function(w){trimws(w)})

  txt.head_diag <- txt.head_diag[-1]
  txt.head_othr <- txt.head_othr[-1]
  txt.head_conf <- c("perc", txt.head_othr)

  txt.head_diag <- gsub("\\s", "_", txt.head_diag, perl=T)
  txt.head_othr <- gsub("\\s", "_", txt.head_othr, perl=T)
  txt.head_conf <- gsub("\\s", "_", txt.head_conf, perl=T)

  ds.boot_diag <- setNames(data.frame(matrix(ncol = length(txt.head_diag), nrow = 0)), txt.head_diag)
  ds.boot_mean <- setNames(data.frame(matrix(ncol = length(txt.head_othr), nrow = 0)), txt.head_othr)
  ds.boot_bias <- setNames(data.frame(matrix(ncol = length(txt.head_othr), nrow = 0)), txt.head_othr)
  ds.boot_ster <- setNames(data.frame(matrix(ncol = length(txt.head_othr), nrow = 0)), txt.head_othr)
  ds.boot_medi <- setNames(data.frame(matrix(ncol = length(txt.head_othr), nrow = 0)), txt.head_othr)

  ds.boot_stci <- setNames(data.frame(matrix(ncol = length(txt.head_conf), nrow = 0)), txt.head_conf)
  ds.boot_pcci <- setNames(data.frame(matrix(ncol = length(txt.head_conf), nrow = 0)), txt.head_conf)

  ds.boot_diag[1,] <- txt.diag
  ds.boot_mean[1,] <- txt.mean
  ds.boot_bias[1,] <- txt.bias
  ds.boot_ster[1,] <- txt.ster
  ds.boot_medi[1,] <- txt.medi


  for(i in 1:length(txt.stci)){
    ds.boot_stci[i,] <- txt.stci[[i]][1:length(txt.stci[[i]])]
  }

  for(i in 1:length(txt.pcci)){
    ds.boot_pcci[i,] <- txt.pcci[[i]][1:length(txt.pcci[[i]])]
  }


  assign('ds.boot_diag', ds.boot_diag, envir = .GlobalEnv)
  assign('ds.boot_mean', ds.boot_mean, envir = .GlobalEnv)
  assign('ds.boot_bias', ds.boot_bias, envir = .GlobalEnv)
  assign('ds.boot_ster', ds.boot_ster, envir = .GlobalEnv)
  assign('ds.boot_medi', ds.boot_medi, envir = .GlobalEnv)

  assign('ds.boot_stci', ds.boot_stci, envir = .GlobalEnv)
  assign('ds.boot_pcci', ds.boot_pcci, envir = .GlobalEnv)

}

