#' Load VPC data
#'
#' @param fpath path to VPC data "folder should include vpc_results.csv"
#' @param dsname opzoeken
#'
#' @return vpc datasets
#' @export
#'
#' @examples
#'
#'

func.vpcdat <- function(fpath, dsname=F){

  wd.vpc <- fpath
  fn.vpc <- paste0("vpctab")

  ds.vpcobs <- read.table(file.path(wd.vpc, fn.vpc), skip = 0, header=T, sep=",")

  txt <- readLines(file.path(wd.vpc, "vpc_results.csv"))
  txt <- gsub('(")+','',txt)


  if(sum(grepl("strata", txt))>1){  # Stratified VPC

    wh.strat.prv <- strsplit(txt[which(grepl("strata", txt))[1]], "\\s")[[1]]
    wh.strat.cat <- wh.strat.prv[which(wh.strat.prv %in% "=")-1]

    num.strat <- length(which(grepl("VPC results strata", txt)))

    ifelse(length(unique(rle(ds.vpcobs[,wh.strat.cat])$values))==num.strat,
           sprintf("Num of strata from VPC_results is equal to vpcobs dataset!"),
           sprintf("Num of strata from VPC_results is NOT equal to vpcobs dataset!"))

    for(n.str in 1:num.strat){

      n.strat <- sort(unique(rle(ds.vpcobs[,wh.strat.cat])$values))[n.str]

      wh.begdat <- which(grepl(paste0("VPC results strata ",wh.strat.cat," = ", n.strat), txt))[1]+3
      wh.enddat <- which(grepl('^$', txt[wh.begdat:length(txt)]))[1] + wh.begdat - 2

      txt.vpcdat <- txt[wh.begdat:wh.enddat]
      txt.vpcdat <- lapply(strsplit(txt.vpcdat, split=','), FUN=function(w){trimws(w)})

      if(n.str==1){
        ds.header <- txt.vpcdat[[1]][2:length(txt.vpcdat[[1]])]
        ds.vpcdat <- setNames(data.frame(matrix(ncol = length(ds.header), nrow = 0)), ds.header)}

      prev.rows <- nrow(ds.vpcdat)

      for(i in 2:length(txt.vpcdat)){
        ds.vpcdat[i-1+prev.rows, 1:(length(txt.vpcdat[[i]][2:length(txt.vpcdat[[i]])]))] <- txt.vpcdat[[i]][2:length(txt.vpcdat[[i]])]
        ds.vpcdat$CAT[i-1+prev.rows] <- n.str
      }

    }


  }else{ #no stratified VPC

    wh.begdat <- which(grepl("median.idv", txt))[1]
    wh.enddat <- which(grepl('^$', txt[wh.begdat:length(txt)]))[1] + wh.begdat - 2

    txt.vpcdat <- txt[wh.begdat:wh.enddat]
    txt.vpcdat <- lapply(strsplit(txt.vpcdat, split=','), FUN=function(w){trimws(w)})


    ds.header <- txt.vpcdat[[1]][2:length(txt.vpcdat[[1]])]

    ds.vpcdat <- setNames(data.frame(matrix(ncol = length(ds.header), nrow = 0)), ds.header)

    for(i in 2:length(txt.vpcdat)){
      ds.vpcdat[i-1,] <- txt.vpcdat[[i]][2:length(txt.vpcdat[[i]])]
    }

  }


  ds.vpcdat <- ds.vpcdat[,!grepl("PI", names(ds.vpcdat))] # Delete PIs

  names(ds.vpcdat) <- gsub("%", "", names(ds.vpcdat)) # Delete % sign
  names(ds.vpcdat) <- gsub("\\s", ".", names(ds.vpcdat)) # Turn spaces into dot

  names(ds.vpcdat) <- gsub("from", "low", names(ds.vpcdat))
  names(ds.vpcdat) <- gsub("to",    "up", names(ds.vpcdat))
  names(ds.vpcdat) <- gsub("95CI.for.", "", names(ds.vpcdat))
  names(ds.vpcdat) <- gsub("^(\\d)(.)", "q\\1\\2", names(ds.vpcdat)) # Put q in front of number -> correct colnames
  names(ds.vpcdat) <- gsub("\\.\\.", ".", names(ds.vpcdat)) # Remove double dots

  names(ds.vpcdat)[1:3] <- c("bin_min","bin_max","bin_mid")

  ds.vpcdat <- data.frame(lapply(ds.vpcdat, as.numeric), stringsAsFactors=FALSE)

  ds.vpcrug <- data.frame(bins=c(ds.vpcdat$bin_mid,ds.vpcdat$bin_max[length(ds.vpcdat$bin_max)]), y=NA)

  #rename column CAT

  if("CAT" %in% names(ds.vpcdat)){ names(ds.vpcdat)[which(names(ds.vpcdat) %in% "CAT")] <- wh.strat.cat}


  if(!dsname){
    assign('ds.vpcdat', ds.vpcdat, envir = .GlobalEnv)
    assign('ds.vpcobs', ds.vpcobs, envir = .GlobalEnv)
  }else{
    assign(paste0('ds.vpcdat', runno), ds.vpcdat, envir = .GlobalEnv)
    assign(paste0('ds.vpcobs', runno), ds.vpcobs, envir = .GlobalEnv)
  }
}
