#' MEDA run all plotting functions on a dataset with default options.
#'
#' @param dat the data
#' @param ccol colors for columns/features.
#' @param trans transform data before running meda.
#' see \code{\link{transformData}}
#' @param ... passed to transformData
#' 
#' @return an object of type d1heat
#'
#' @details For each feature column a 1D heatmap is generated and
#' plotted as a geom_tile object.
#'
#'
#' @examples
#' dat <- iris[, -5]
#' ccol <- c("darkgreen", "darkgreen", "purple", "purple")
#' L <- runAll(dat, ccol = ccol, trans = "log")
#' plot(L[[1]])
#' plot(L[[2]], bincount=TRUE)
#' plot(L[[3]])
#' plot(L[[4]])
#' plot(L[[5]])
#' plot(L[[6]])
#' plotDend(L[[7]])
#' plot(L[[7]])
#' stackM(L[[7]])
#' clusterMeans(L[[7]])
#' 
#' @export 
runAll <- function(dat, ccol = "black", trans = NULL, ...){

  dato <- dat
  
  if(!is.null(trans)){
    dat <- transformData(dato, type = trans, ...)[[1]]
    #dat <- transformData(dato, type = trans)[[1]]
  }

  outL <- list()

  print("Running mlocation")
  outL$mlocationDat <- mlocation(dat, ccol = ccol)
  
  print("Running d1heat")
  outL$d1headDat <- d1heat(dat, ccol = ccol)
  
  print("Running cumvar")
  outL$cumvarDat <- cumvar(dat)
  
  print("Running outliers")
  outL$outliersDat <- outliers(dat)
  
  print("Running pairHex")
  outL$pairHexDat <- invisible(pairhex(dat, maxd = 6))

  print("Running correlation")
  outL$corDat <- corDat <- medacor(dat, ccol = ccol)

  print("Running hmc")
  outL$hmcDat <- hmc(scale(dat, center = TRUE, scale = TRUE), 
                             maxDepth = 6, modelNames = "VVV", ccol = ccol)

  outL$data <- 
    if(!is.null(trans)){
      list(dato, dat)
    } else {
      dato
    }

  return(outL)
}

#   Time:
##  Working status:
### Comments:
####Soli Deo Gloria
