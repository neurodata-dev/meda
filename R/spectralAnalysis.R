#' Generate singular vector data
#'
#' @param dat data 
#' @param ccol feature colors 
#' @param maxd maximum dimensions to plot
#'
#' @return pairs plot and heatmap of right singular vectors
#'
#' @importFrom gplots colorpanel
#' @importFrom data.table melt
#' @importFrom lattice splom
#'
#' @examples
#' dat <- iris[, -5]
#' p1 <- rsv(dat, ccol = 2:5)
#' 
#' @export 
### Spectral plots, singularvectors 
rsv <- function(dat, ccol = "black", maxd = Inf) {
  d <- dim(dat)[2]
  dm <- as.matrix(dat) 
  covM <- cov(dm)

  dmax <- ifelse(d > maxd, maxd, d)

  v <- svd(covM, nv = dmax)$v

  colnames(v) <- sprintf(paste0("rsv%0",nchar(ncol(v)), "d"), 1:ncol(v))
  rownames(v) <- colnames(dat)
  mv <- melt(v)
  
  mv$Var2 <- ordered(mv$Var2, levels = rev(levels(mv$Var2)))

  out <- structure(list(dat = mv), class = "rsv")
}


#' Generate singular vector data
#'
#' @param x an object of type rsv
#' @param ... used for ccol feature colors
#'
#' @return pairs plot and heatmap of right singular vectors
#'
#' @importFrom gplots colorpanel
#' @importFrom lattice splom
#'
#' @method plot rsv
#' @export 
### Spectral plots, singularvectors 
plot.rsv <- function(x, ...){
  
  mycol <- colorpanel(255, "darkred", "gray95", "darkblue")

  sc <- scale_fill_gradientn(colours = mycol)

  g1 <- 
    ggplot(mv, aes(x = Var1, y = Var2, fill = value)) + 
    geom_raster() + 
    sc + xlab(NULL) + ylab("Right Singular Vectors") + 
    theme(panel.background = element_rect(fill = "gray75")) +
    theme(panel.grid = element_blank()) +
    theme(axis.text.x = element_text(color = ccol, angle = 90))

  g2 <- 
    splom(v,
     as.matrix = TRUE, 
     panel=panel.xyplot,
     diag.panel = function(x, ...){
     yrng <- current.panel.limits()$ylim
     d <- density(x, na.rm=TRUE)
     d$y <- with(d, yrng[1] + 0.95 * diff(yrng) * y / max(y) )
     panel.lines(d)
     diag.panel.splom(x, ...)
     },
     pch = 19, col = ccol, 
     pscale=0, varname.cex=0.7
     )

  lout <- list(g1 = g1, g2 = g2)
  grid.arrange(g1, g2, ncol =2)
  invisible(lout)
  NextMethod("plot")
} ## END p.eig

