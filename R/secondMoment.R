#' Generate a correlation plot
#'
#' @param dat data
#' @param ccol colors for column labels
#'
#' @importFrom corrplot corrplot
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
#' @method plot medacor
#' @export 
plot.medacor <- function(x, ...) {
  do.call(corrplot, x)
}
