#' Generate 1d heatmaps
#'
#' @param dat the data
#' @param breaks see \code{\link[graphics]{hist}}
#' @param ccol colors for features.
#' 
#' @return an object of type d1heat
#'
#' @details For each feature column a 1D heatmap is generated and
#' plotted as a geom_tile object.
#'
#' @import ggplot2 
#' @importFrom gplots colorpanel
#' @importFrom data.table data.table 
#' @importFrom data.table melt
#'
#' @examples
#' dat <- iris[, -5]
#' d1 <- d1heat(dat, breaks = 14, ccol = 1:4)
#' plot(d1)
#' @export 
### 1D heatmap
d1heat <- function(dat, breaks = "Scott", ccol = "black") {

  dat <- data.frame(apply(dat, 2, as.numeric))

  mt <- data.table::melt(dat, measure = 1:ncol(dat))

  H <- hist(mt$value, breaks = breaks, plot = FALSE)
  df <- expand.grid(x = H$mids, y = names(dat))

  bn <- lapply(apply(dat, 2, hist, breaks = H$breaks, plot = FALSE), 
               function(x) { x$counts }) 

  df$Count <- Reduce(c, bn)

  out <- structure(list(dat = df, ccol = ccol, breaks = breaks),
                   class = "d1heat")

}
### END d1heat

#' Generate 1d heatmap plots
#'
#' @param x an object of type d1heat
#' @param ... unused
#' 
#' @return a ggplot object 
#'
#' @import ggplot2 
#' @importFrom gplots colorpanel
#' @importFrom data.table data.table 
#' @importFrom data.table melt
#'
#' @export 
#' @method plot d1heat
plot.d1heat <- function(x, ...){

  d1heat <- x
  df <- d1heat$dat
  ccol <- d1heat$ccol

  mycol <- colorpanel(255, "white", "#094620")
  sc <- scale_fill_gradientn(colours = mycol)
  
  p <- ggplot(df, aes(x, y, fill = Count)) + 
         geom_tile() + 
         theme(axis.title = element_blank(),
               axis.text.y=element_text(color=ccol)) + 
         sc

  return(p)
}

