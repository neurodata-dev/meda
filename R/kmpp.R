#' K-Means++ initialization to K-means
#'
#' @param x The data formatted as a matrix.
#' @param k The number of clusters desired. \code{k < nrows(x) || length(x)}
#' 
#' @return An object of class `'kmeans''.
#'
#' @details A set of centers are calculated as suggested by
#' Arthur and Vassilvitskii and used to initialize the kmeans algorithm.
#' The first center point is chosen at random.  Each subsequent center
#' is chosen with probability \eqn{P = D(x')^2/( \sum_{x \in X} D(x)^2)} (see references)
#' 
#'       
#' @seealso \code{\link[stats]{kmeans}, \link{bhkmpp}}
#'
#' @examples
#' set.seed(13)
#' z <- c(0,4,12,16)[sample(4, 1e3, replace=TRUE)]
#' x <- rnorm(1e3, mean = z, sd = 1)
#' kdf <- kmpp(x, 4)
#' par(mfrow = c(2,1))
#' plot(x, as.factor(z), col = as.factor(z), pch = '|')
#' title('Original Data')
#' plot(x, kdf$cluster, col = kdf$cluster, pch = '|')
#' title('Clustered Data')
#'
#' 
#' set.seed(23)
#' s1 <- sample(4, 1e3, replace = TRUE)
#' z <- matrix(c(0,4,0,4,0,0,4,4), ncol=2)[s1,]
#' xy <- data.frame(x = rnorm(1e3, mean = z[,1]), y = rnorm(1e3, mean=z[,2]))
#' kdf2 <- kmpp(xy,4)
#' par(mfrow = c(2,1))
#' opal <- palette(adjustcolor(palette(), alpha.f = 0.55))
#' plot(xy, col = s1, pch = 19)
#' title('Original Data')
#' plot(xy, col = kdf2$cluster, pch = 17)
#' title('Clustered Data')
#' palette('default')
#'
#' @references Arthur, David, and Sergei Vassilvitskii. 
#' 'k-means++: The advantages of careful seeding.'
#' Proceedings of the eighteenth annual ACM-SIAM
#' symposium on Discrete algorithms. Society for Industrial and Applied
#' Mathematics, 2007.
#' \url{http://ilpubs.stanford.edu:8090/778/1/2006-13.pdf}
#'
#' @importFrom stats quantile complete.cases
#' @export
kmpp <- function(x, k = 2) {
    la2 <- function(a, b) {
        apply(t(apply(as.matrix(b), 1, function(rr) (rr - a)^2)), 1, sum)
    }
    
    ## Check if data is one dimensional
    if (is.null(dim(x))) {
        la1 <- function(a, b) {
            (b - a)^2
        }
        
        ### Step 1a
        cp <- sample(length(x), 1)
        ind <- c(cp)
        centers <- x[cp]
        
        p <- la1(centers[1], x)
        P <- p/sum(p)  ## P = D(x')^2/( \sum_{x in X}  D(x)^2)
        
        ### Step 1b-c
        for (i in 2:k) {
            # set.seed(2^13)
            cp <- sample(length(x), 1, prob = P)  ## c'
            centers[i] <- x[cp]
            ind <- c(ind, cp)
            
            pnew <- la1(x[cp], x)
            p <- apply(cbind(la1(x[cp], x), p), 1, min)
            P <- p/sum(p)
        }
        
        kObj <- stats::kmeans(x, centers = centers)
        
        return(kObj)
        
    } else {
        
        ### Step 1a
        cp <- sample(nrow(x), 1)
        
        ind <- c(cp)
        
        centers <- list()
        
        centers[[1]] <- x[cp, ]
        
        p <- la2(x[cp, ], x)
        P <- p/sum(p)  ## P = D(x')^2/( \sum_{x in X} D(x)^2)
        
        ### Step 1b-c
        for (i in 2:k) {
            # set.seed(2^13)
            cp <- sample(nrow(x), 1, prob = P)  ## c'
            centers[[i]] <- x[cp, ]
            ind <- c(ind, cp)
            
            p <- apply(cbind(la2(x[cp, ], x), p), 1, min)  #apply(cbind(p,ds[,cp]),1,min)
            P <- p/sum(p)
        }
        
        centers <- Reduce("rbind", centers)
        
        kObj <- stats::kmeans(x, centers = centers)
        
        return(kObj)
    }  ### END ELSE
}  ### END FUNCTION
# Time: About 3-6 hours Working status: Works as expected.  Comments: Might need
# work to be robust against messy data.  Soli Deo Gloria
