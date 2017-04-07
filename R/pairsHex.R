#' Generate a pairs hex binned plot
#'
#' @param dat data
#' @param maxd maximum dimensions to plot
#' @param colramp Color Ramp used, BTC or BTY
#' @param ccol colors for features
#' @param loess boolean for loess curve
#' @param lmline boolean for lm line
#'
#' @return A lattice splom
#' @importFrom hexbin BTC
#' @importFrom lattice splom
#'
#' @examples
#' dat <- iris[,-5]
#' pairhex(dat, colramp = magent, ccol = c("blue", "red"))
#' @export 
### Pairs Plots
pairhex <- function(dat, maxd = Inf, colramp = BTC, ccol = "black", loess = TRUE, lmline = TRUE) {

  d <- dim(dat)[2]
  du <- ifelse(d > maxd, maxd, d)

  if(d > maxd){
    tmp <- dat[, 1:du]
    t1 <-paste("pairs hex binned plot of first", du, "dimensions")
    if(ccol != "black"){ ccol <- ccol[1:du] }
  } else {
    tmp <- dat
    t1 <-paste("pairs hex binned plot of the data")
  }

  ### the following from https://procomun.wordpress.com/2011/03/18/splomr/
  splom(tmp,
   as.matrix = TRUE,
   panel=panel.hexbinplot,
   colramp=colramp,
   #style = "nested.centroids",
   diag.panel = function(x, ...){
   yrng <- current.panel.limits()$ylim
   d <- density(x, na.rm=TRUE)
   d$y <- with(d, yrng[1] + 0.95 * diff(yrng) * y / max(y) )
   panel.lines(d)
   diag.panel.splom(x, ...)
   },
   lower.panel = function(x, y, ...){
   panel.hexbinplot(x, y, colorkey = TRUE, ...)
   if(loess) panel.loess(x, y, ..., col = 'red')
   if(lmline) panel.lmline(x, y, ..., col = 'orange')
   },
   pscale=0, varname.cex=0.7
   )
} ## END FUNCTION
