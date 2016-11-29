#' Hierarchical MCLUST
#'
#' @param dat A data frame or matrix
#' file.
#' @return A vector of cluster labels
#'
#' @details Performs hierarchical mclust for k (G) = 1 or 2.
#' If bic yields k = 2, then clustering is performed again.
#' If bic yields k = 1, then that cluster is frozen. 
#'
#' @importFrom mclust mclustBIC Mclust
#'
#' @examples
#' library(meda)
#' Z <- rep(1:5, each = 50)
#' mu <- c(-10,-5,5,10,20)
#' set.seed(391857)
#' test <- rnorm(length(Z), mean = mu[Z], sd = 1)
#' L <- hmc(test)
#' plot(cbind(test,0), col = Z)
#' for(i in 2:dim(L)[2]){
#'  points(cbind(test,i/dim(L)[2]), col=L[,i])
#'  }
#' 
#' mu2d <- matrix(3*c(1,0,-1,0,-2,0,1,0,-1,-2), 5,2)
#' set.seed(1234)
#' test2d <- data.frame(
#'             x = rnorm(length(Z), mean = mu2d[Z,1], sd = 1),
#'             y = rnorm(length(Z), mean = mu2d[Z,2], sd = 1))
#'
#' L <- hmc(test2d)
#' plot(test2d, col = L[, dim(L)[2]], pch = Z)
#' 
#' @export



hmc <- function(dat){
  hmc1 <- function(dat){
    L <- data.frame(continue = rep(TRUE, length(dat)), lab = rep(1, length(dat)))
  
    while(any(L$continue == TRUE)){
      tmp <- L[, dim(L)[2]]
      for(i in sort(unique(L[,dim(L)[2]][L$continue == TRUE]))){

        if(length(dat[L[,dim(L)[2]] == i & L$continue == TRUE]) > 1){
          bicP <- mclust::mclustBIC(dat[L[,dim(L)[2]] == i & L$continue == TRUE], G = 1:2)
          mcP <- mclust::Mclust(dat[L[, dim(L)[2]] == i & L$continue == TRUE], x = bicP)
        } else {
          mcP$G <- 0
        }
   
        if(mcP$G == 2){
            tmp[L[,dim(L)[2]] == i & L$continue == TRUE] <- 
              mcP$classification + 2 * (i - 1)
          } else {
          L$continue[L[, dim(L)[2]] == i & L$continue == TRUE] <- FALSE 
        }
      }
      if(any(L$continue == TRUE)){
        L <- cbind(L,L[, dim(L)[2]] + tmp)
        }
    }
    
    out <- data.frame(L)
    colnames(out) <- paste0("V", 1:dim(out)[2])
    out <- apply(as.matrix(out[,-1]), 2, function(x) as.numeric(factor(x)))
    return(out)
  } ## END hmc1

  if(is.null(dim(dat))){
    return(hmc1(dat))
  } else {
    L <- data.frame(continue = rep(TRUE, nrow(dat)), lab = rep(1, nrow(dat)))
  
    while(any(L$continue == TRUE)){
      tmp <- L[, dim(L)[2]]
      for(i in sort(unique(L[,dim(L)[2]][L$continue == TRUE]))){

        if(nrow(dat[L[,dim(L)[2]] == i & L$continue == TRUE,]) > 1){
          bicP <- mclust::mclustBIC(dat[L[,dim(L)[2]] == i & L$continue == TRUE,], G = 1:2)
          mcP <- mclust::Mclust(dat[L[, dim(L)[2]] == i & L$continue == TRUE,], x = bicP)
        } else {
          mcP$G <- 0
        }
   
        if(mcP$G == 2){
            tmp[L[,dim(L)[2]] == i & L$continue == TRUE] <- 
              mcP$classification + 2 * (i - 1)
          } else {
          L$continue[L[, dim(L)[2]] == i & L$continue == TRUE] <- FALSE 
        }
      }
      if(any(L$continue == TRUE)){
        L <- cbind(L,X = L[, dim(L)[2]] + tmp)
      }
    }
    
    out <- data.frame(L)
    colnames(out) <- paste0("V", 1:dim(out)[2])
    out <- apply(as.matrix(out[,-1]), 2, function(x) as.numeric(factor(x)))
    return(out)
  }
}
