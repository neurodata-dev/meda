#' Generate outlier object
#'
#' @param dat data
#' @param k number of neighbors if nrow(dat) in (1e4,1e5]
#' @param ... used to pass arguments to randomForest
#'
#' @return An outlier plot
#' @details
#' For sample size <= 1e4 an outlier measure is calculated from the 
#' randomForest package, points with measure greater than 3 sd from the
#' mean are considred outliers.  Need to add case for n > 1e4.
#' @import ggplot2
#' @importFrom randomForest randomForest outlier
#' 
#' @export 
#' @examples
#' dat <- iris[, -5]
#' out <- outliers(dat)
#' plot(out)
### Outlier plots
outliers <- function(dat, k = sqrt(dim(dat)[1]), ...) {

  n <- dim(dat)[1]

  if(n > 1e4){
    s1 <- sample(1:n, 1e4)
    rf1 <- randomForest(dat[s1,], proximity = TRUE)
  } else {
    rf1 <- randomForest(dat, proximity = TRUE)
  }

  out <- outlier(rf1)
  l1 <- mean(out) + 3*sd(out)
  status <- factor(out < l1, labels = c("outlier", "inlier"))
  df1 <- data.frame(x = 1:length(out), outD = out, status = status) 

  out <- structure(list(dat = df1, l1=l1), class = "outliers")
} ## END meaa::outliers

#' Generate an outlier plot
#'
#' @param x an object of type outliers
#' @param ... unused
#'
#' @return An outlier plot
#' @import ggplot2
#' @importFrom randomForest randomForest outlier
#' 
#' @export 
#' @method plot outliers
plot.outliers <- function(x, ...){

  outliers <- x
  df1 <- outliers$dat
  l1 <- outliers$l1
  p <- 
    ggplot(data = df1, aes(x = x, y = outD, color = status)) + 
    geom_point() + geom_hline(yintercept = l1, aes(label = l1), show.legend = TRUE) + 
    ylab("Outlier Measure") + xlab("index") + 
    ggtitle("Outliers 3*sd from mean")
    
  return(p)
  NextMethod("plot")
} ## END plot.outliers

