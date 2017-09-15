#' Show summary of data and feature types
#'
#' @param dat data
#'
#' @export 
#' @examples
#' dat0 <- iris[, -5]
#' dat1 <- data.frame(matrix(as.integer(rbinom(150*5, 10, 0.5)), nrow=150,ncol=5))
#' z <- complex(real = stats::rnorm(150), imaginary = stats::rnorm(150))
#' dat <- data.frame(dat0, dat1, let = paste0(letters, 1:150), cplx = z,stringsAsFactors = FALSE)
#' cs <- colStats(dat)
#' plot(cs)
#' @importFrom utils head
### Cumulative variance
colStats <- function(dat){

  dat <- as.data.frame(dat, stringsAsFactors = FALSE) 
  n <- dim(dat)[1]
  d <- dim(dat)[2]

  cl <- Reduce(rbind, lapply(dat, typeof))

  wh <- cl %in% c("numeric", "double","complex")

  anyna <- anyNA(dat)

  if(d <= 12){
    h <- head(dat)
  } else {
    s1 <- sample(ncol(dat), 12)
    h <- head(dat[, s1])
  }

  ### Rounding for display purposes 
  if(any(wh)){
    h[wh] <- round(h[wh], 3)
  }

  tab <- table(cl)
  d1 <- as.data.frame(tab)
  
  dl <- structure(list(dat = d1, head = h), 
                   class = c("colstats"))
  return(dl)
} ## END meda::colStats


#' Plot method for cumulative variance plots
#' 
#' @param x an object of type colstats
#' @param ... unused
#'
#' @return A barplot of column types
#'
#' @importFrom gridExtra grid.table
#' @import ggplot2
#'
#' @export
#' @method plot colstats
plot.colstats <- function(x, ...){

  d1 <- x$dat
  h <- x$head
  gg <- ggplot(d1, aes(cl, Freq)) + 
    geom_col() + xlab("Column Type") + 
    ylab("Count")
  
  tab <- tableGrob(h)
  
  lay <- matrix(c(1,2,1,2,1,2), 2,3)
  p <- arrangeGrob(gg, tab, ncol = 1, layout_matrix = lay)

  invisible(plot(p))
} ## END plot.colstats
