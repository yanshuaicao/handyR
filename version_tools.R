############################################################
### Some tools for getting code version info, so that
### experiment results could include code version 
### that produced it.
###
### Author: Yanshuai Cao, 2014 May
############################################################

parse.hg.out <- function(hg.sum.out) {
  version.info <- gsub('--','-',gsub(':','-',paste(gsub(' ', '-', hg.sum.out[1]), gsub(' ','-',hg.sum.out[3]),sep='-')))
  has.pending.commit <- any(grep('modified',hg.sum.out[4])) || any(grep('unknown',hg.sum.out[4]))
  has.pending.update <- !any(grep('(current)',hg.sum.out))

  result <- list()
  result$version.info <- version.info
  result$has.pending.commit <- has.pending.commit
  result$has.pending.update <- has.pending.update
  
  return(result)
}


get.code.version.info <- function() {
  
  hg.sum <- try(system('hg sum', intern = TRUE))
  if (is.null(attr(hg.sum, 'status'))) {
    return(parse.hg.out(hg.sum))
  } else {
    stop('Corrently only supporting hg, and this dir is not tracked by it')
  }
}
