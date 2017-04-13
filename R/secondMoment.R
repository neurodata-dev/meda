#' Generate a correlation plot
#'
#' @param dat data
#' @param ccol colors for column labels
#'
#' @return A correlation plot
#' @examples 
#' dat <- iris[, -5]
#' ccol <- c("darkgreen", "red", "green", "red")
#' p1 <- medacor(dat, ccol)
#' @export 
### Correlation plots 
medacor <- function(dat, ccol = "black") {
  out <- structure(list(corr = cor(dat), method = "color", tl.cex=1, tl.col = ccol),
                   class = "medacor")
  return(out)
}

#' Generate correlation plots
#'
#' @param x an object of type d1heat
#' @param ... unused
#' 
#' @return a corrplot
#'
#' @importFrom corrplot corrplot
#'
#' @export 
#' @method plot medacor
plot.medacor <- function(x, ...){
  p <- do.call(corrplot, x)
}
