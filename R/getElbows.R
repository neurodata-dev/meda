#' Given a decreasingly sorted vector, return the given number of elbows
#'
#' @param dat a input vector (e.g. a vector of standard deviations), or a input feature matrix.
#' @param n the number of returned elbows.
#' @param threshold either FALSE or a number. If threshold is a number, then 
#' all the elements in d that are not larger than the threshold will be ignored.
#' @param plot logical. When TRUE, it depicts a scree plot with highlighted elbows.
#' 
#' @return q a vector of length n.
#'
#' @details Computes the "elbows" as defined in Zhu and Ghodsi 2006. 
#' 
#' @references Obtained from #' \url{http://www.cis.jhu.edu/~parky/Synapse/getElbows.R} with
#' permission from the auther. 
#' @references Zhu, Mu and Ghodsi, Ali (2006), "Automatic dimensionality selection from
#' the scree plot via the use of profile likelihood", Computational
#' Statistics & Data Analysis, Volume 51 Issue 2, pp 918-930, November, 2006. 
#'
#' @author Youngser Park \url{http://www.cis.jhu.edu/~parky/}
#' 
#' @importFrom stats dnorm
#' @examples 
#' v <- sort(runif(25), decreasing = TRUE)
#' #v <- prcomp(iris[, -5])$sd
#' getElbows(v)
#' 
#' @export 
getElbows <- function(dat, n = 3, threshold = FALSE, plot = TRUE) {
## Time-stamp: <Fri Nov 13, 2015 08:16:26 YP>

  if (is.matrix(dat)) {
      d <- sort(apply(dat,2,sd), decreasing=TRUE)
  } else {
      d <- sort(dat,decreasing=TRUE)
  }

  if (!is.logical(threshold))
      d <- d[d > threshold]
  
  p <- length(d)
  if (p == 0)
      stop(paste("d must have elements that are larger than the threshold ",
                 threshold), "!", sep="")

  lq <- rep(0.0, p)                     # log likelihood, function of q
  for (q in 1:p) {
      mu1 <- mean(d[1:q])
      mu2 <- mean(d[-(1:q)])              # = NaN when q = p
      sigma2 <- (sum((d[1:q] - mu1)^2) + sum((d[-(1:q)] - mu2)^2)) /
          (p - 1 - (q < p))
      lq[q] <- sum( dnorm(  d[1:q ], mu1, sqrt(sigma2), log=TRUE) ) +
          sum( dnorm(d[-(1:q)], mu2, sqrt(sigma2), log=TRUE) )
  }
  
  q <- which.max(lq)
  if (n > 1 && q < (p-1)) {
      q <- c(q, q + getElbows(d[(q+1):p], n-1, plot=FALSE))
  }
  
  if (plot==TRUE) {
      if (is.matrix(dat)) {
          sdv <- d # apply(dat,2,sd)
          plot(sdv,type="b",xlab="dim",ylab="stdev")
          points(q,sdv[q],col=2,pch=19)
      } else {
          plot(dat, type="b")
          points(q,dat[q],col=2,pch=19)
      }
  }
  
  return(q)
}
