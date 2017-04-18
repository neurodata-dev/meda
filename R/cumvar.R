#' Generate a cumulative variance plot
#'
#' @param dat data
#'
#' @return A correlation plot
#' @details Uses getElbows from
#' \url{http://www.cis.jhu.edu/~parky/Synapse/getElbows.R}
#' @import ggplot2
#' @importFrom stats prcomp
#' 
#' @export 
#' @examples
#' dat <- iris[, -5]
#' c1 <- cumvar(dat)
#' plot(c1)
### Cumulative variance
cumvar <- function(dat){
  tryCatch(source("http://www.cis.jhu.edu/~parky/Synapse/getElbows.R"))   
  
  n  <- dim(dat)[1]

  sv <- svd(dat, nu = 0, nv = 0)$d
  tryCatch(elb <- getElbows(sv, plot = FALSE))

  CS <- data.frame(index = 1:length(sv), cs = (100*cumsum(sv / sum(sv))))
  CS$col <- "" 
  tryCatch(CS$col[elb] <- "elbow")
  CS$col <- as.factor(CS$col)
   
  out <- structure(list(dat = CS), class = "cumvar")
} ## END meda::cumvar


#' Plot method for cumulative variance plots
#' 
#' @param x an object of type cumvar
#' @param ... unused
#'
#' @return A cumulative variance plot
#' 
#' @export
#' @method plot cumvar
plot.cumvar <- function(x, ...){

  CS <- x$dat
  ggcumvar <- 
    ggplot(CS, aes(x = index, y = cs)) + 
    scale_color_manual(values = c("black", "red")) + 
    geom_line() + 
    geom_point(aes(size = as.integer(col), color = col)) + 
    scale_size_continuous(guide = FALSE) + 
    ylab("% Cumulative Variance") + 
    ggtitle("Cumulative Sum of variance in PC's")

  return(ggcumvar)
} ## END plot.cumvar


