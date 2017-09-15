#' Binary Hierarchical k-means++
#'
#' @param x A matrix, data.frame, or data.table with the data of
#' interest.
#' @param blevels An integer specifying how many levels in the binary
#' tree to compute.
#'
#' @return A list with the cluster labels '$L' and level means
#' '$levelMeans'.
#'
#' @details Runs the kmeans algorithm with the kmeans++ initialization
#' in a binary hieracrhical fasion. 
#'
#' @seealso \code{\link[stats]{kmeans}, \link{kmpp}}
#'
#' @references Arthur, David, and Sergei Vassilvitskii. 
#' 'k-means++: The advantages of careful seeding.'
#' Proceedings of the eighteenth annual ACM-SIAM
#' symposium on Discrete algorithms. Society for Industrial and Applied
#' Mathematics, 2007.
#' \url{http://ilpubs.stanford.edu:8090/778/1/2006-13.pdf}
#'
#' @examples
#' \dontrun{
#' require(meda)
#' set.seed(13)
#' z <- c(0,4,12,16)[sample(4, 1e3, replace=TRUE)]
#' x <- rnorm(1e3, mean = z, sd = 1)
#' kdf <- bhkmpp(x, 2)
#' par(mfrow = c(3,1))
#' plot(x, as.factor(z), col = as.factor(z), pch = '|')
#' title('Original Data')
#' plot(x, kdf$L[,1], col = kdf$L[,1], pch = '|')
#' title('Clustered Data: Level 1')
#' plot(x, kdf$L[,2], col = kdf$L[,2], pch = '|')
#' title('Clustered Data: Level 2')
#' 
#' set.seed(23)
#' s1 <- sample(4, 1e3, replace = TRUE)
#' z <- matrix(c(0,4,0,4,0,0,4,4), ncol=2)[s1,]
#' xy <- data.frame(x = rnorm(1e3, mean = z[,1]), y = rnorm(1e3, mean=z[,2]))
#' kdf2 <- bhkmpp(xy,2)
#' par(mfrow = c(3,1))
#' opal <- palette(adjustcolor(palette(), alpha.f = 0.55))
#' plot(xy, col = s1, pch = 19)
#' title('Original Data')
#' plot(xy, col = kdf2$L[,1], pch = 17)
#' title('Clustered Data: Level 1')
#' plot(xy, col = kdf2$L[,2], pch = 17)
#' title('Clustered Data: Level 2')
#' palette('default')
#' }
#'
#' @importFrom knor Kmeans
#' @export
bhkmpp <- function(x, blevels) {
    k0 <- Kmeans(as.matrix(x), centers = 2,  init = "kmeanspp")
    dx <- !is.null(dim(x))
    L <- data.frame(lv1 = k0$cluster)
    
    for (y in 2:blevels) {
        L[[y]] <- 0L
    }
    colnames(L) <- paste0("lv", 1:blevels)
    
    for (j in 1:(blevels - 1)) {
        for (i in sort(unique(L[[j]]))) {
            kv <- if (dx) {
                Kmeans(x[L[[j]] == i, ], centers = 2, init = "kmeanspp")
            } else {
                Kmeans(as.matrix(x[L[[j]] == i]), centers = 2, init = "kmeanspp")
            }
            
            if (i != 1) {
                kv$cluster <- as.integer(kv$cluster + 2 * (i - 1))
            }
            
            L[L[[j]] == i, ][[j + 1]] <- kv$cluster
        }
    }

    means <- list()
    for(i in 1:ncol(L)){
      cluster <- list()
      for(uj in 1:max(L[[i]])){
        ind <- which(L[[i]] == uj)
  
        if(!is.null(dim(x))){
          cluster[[uj]] <- apply(x[ind,], 2, mean)
        } else {
          cluster[[uj]] <- mean(x[ind])
        }
      }
      tmp <- Reduce(rbind, cluster)
      rownames(tmp) <- NULL

      means[[i]] <- tmp
    }

    out <- list(L = L, levelMeans = means)

    return(out)
}
# Time: About 3-6 hours. Working status: Works as expected.  Comments: Might need
# work to be robust against messy data.  Soli Deo Gloria
