#' Generate location estimates
#'
#' @param dat data
#' @param ccol colors for features. 
#' @param CI specified confidence interval about mean.
#'
#' @return a list of ggplot objects, see details. 
#' 
#' @importFrom data.table melt
#' @importFrom stats median qnorm
#' 
#' @examples
#' dat <- iris[, -5]
#' loc <- mlocation(dat, ccol = 1:4)
#' plot(loc)
#'
#' @importFrom stats mad
#' @export 
### Location Estimates 
mlocation <- function(dat, ccol = NULL, CI = 0.95){

  n <- nrow(dat)
  Mean <- apply(dat, 2, mean)
  Median <- apply(dat, 2, median)
  SD <- apply(dat, 2, sd)
 
  zs <- qnorm(CI + (1- CI)/2)

  lci <- c(Mean - zs*SD/sqrt(n), Median - mad(Median))
  uci <- c(Mean + zs*SD/sqrt(n), Median + mad(Median))
 
  loc1 <- melt(cbind(Mean, Median))

  loc1 <- cbind(loc1, l = lci, u = uci)

  loc <- structure(list(dat = loc1, ccol = ccol), 
                   class = c("mlocation"))
  return(loc)
} ### END meda::mlocation


#' Generate location estimate plots
#'
#' @param x an object of type location
#' @param ... unused
#'
#' @return a list of ggplot objects.
#' 
#' @import ggplot2
#' @importFrom gplots colorpanel
#' @importFrom gridExtra grid.arrange
#' 
#' @method plot mlocation
#' @export 
plot.mlocation <- function(x, ...){
 
  loc <- x 
  dm <- loc$dat

  ccol <- if(!is.null(loc$ccol)){ 
    loc$ccol
  } else {
    "black"
  }

  p1 <- 
    ggplot(dm, aes(x = Var1, y = Var2, fill = value)) +  
      geom_raster() +# coord_flip() + 
      theme(panel.background = element_blank(), 
            axis.title = element_blank())

  p2 <- 
    ggplot(dm, aes(x = Var1, y = value, ymin = l, ymax = u, 
                   group = Var2, color = Var2)) + 
          geom_line(alpha = 0.7, size = 1) + 
          geom_errorbar(aes(ymin = l, ymax = u), width = 0.5, size = 1, 
                        alpha=0.6) + 
          scale_color_discrete(
            labels = c("Mean \u00B1sd", "Median \u00B1MAD")) + 
          theme(legend.title = element_blank(),
                axis.title = element_blank()) 

  if(dim(dm)[2] > 8){
    p1 <- p1 + coord_flip() + 
      theme(axis.text.y=element_text(color=ccol))
    p2 <- p2 + coord_flip() +
      theme(axis.text.y=element_text(color=ccol))
  } else {
    p1 <- p1 + theme(axis.text.x=element_text(color=ccol, angle = 90))
    p2 <- p2 + theme(axis.text.x=element_text(color=ccol, angle = 90))
  }

  lout <- list(pheat = p1, pline = p2)
  (out <- grid.arrange(lout$pheat,lout$pline, ncol = 2))
  invisible(out)
}

