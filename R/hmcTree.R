#' Generate a binary hierarchical mclust tree
#'
#' @param data a data matrix
#' @param maxDepth maximum tree depth
#'
#' @return binary hierarchical mclust classification output
#' @details BIC is run for k = {1,2}, if k = 2 then each node is
#' propagated down the tree.  If k = 1, then that node is frozen. 
#' @import data.tree
#' @importFrom abind abind
#' @importFrom mclust mclustBIC Mclust
#' @importFrom data.table melt
#'
#' @examples
#' dat <- iris[, -5]
#' truth <- iris[, 5]
#' hmcL <- hmcTree(dat)
#' plot(as.dendrogram(L))
#' @export 
### Binary Hierarchical Mclust Classifications 
hmcTree <- function(dat, maxDepth = 6){
  ## Helper function
  splitNode = function(node){
    if(node$continue == TRUE && isLeaf(node)){
       b <- mclustBIC(node$data, G = 1:2)
       mc <- Mclust(node$data, x = b)
       if(mc$G == 2){
         dat1 <- node$data[mc$classification == 1,]
         dat2 <- node$data[mc$classification == 2,]

         node$AddChild(paste0(node$name, "1"), 
                       data = dat1, dataid = rownames(dat1), continue = TRUE,
                       mean = mc$parameters$mean[,1], 
                       cov = mc$parameters$variance$sigma[,,1]
                       )
         node$AddChild(paste0(node$name, "2"), 
                       data = dat2, dataid = rownames(dat2), continue = TRUE,
                       mean = mc$parameters$mean[,2], 
                       cov = mc$parameters$variance$sigma[,,2]
                       )
         node$model = mc
       } else {
         node$continue = FALSE
       }
    }
  } ## END splitNode

  splitNode1 = function(node){
    if(node$continue == TRUE && isLeaf(node)){
       b <- mclustBIC(node$data, G = 1:2)
       mc <- Mclust(node$data, x = b)
       if(mc$G == 2){
         dat1 <- node$data[mc$classification == 1,]
         dat2 <- node$data[mc$classification == 2,]

         node$AddChild(paste0(node$name, "1"), 
                       data = dat1, dataid = rownames(dat2), continue = TRUE,
                       mean = mc$parameters$mean[1], 
                       cov = mc$parameters$variance$sigma[1]
                       )
         node$AddChild(paste0(node$name, "2"), 
                       data = dat2, dataid = rownames(dat1), continue = TRUE,
                       mean = mc$parameters$mean[2], 
                       cov = mc$parameters$variance$sigma[2]
                       )
         node$model = mc
       } else {
         node$continue = FALSE
       }
    }
  } ## END splitNode1

  dat <- as.data.frame(dat)
  dataid <- if(!is.null(rownames(dat))){ 
              rownames(dat) 
              } else { 
                1:dim(dat)[1] 
              }

  node <- Node$new("1", data = dat, dataid = rownames(dat), continue = TRUE, model = NULL)

  if(is.null(dim(dat)) || dim(dat)[2] == 1){
    while(node$height < maxDepth && 
         any(Reduce(c,node$Get('continue', format = list, filterFun = isLeaf)))){
      node$Do(splitNode1, filterFun = isLeaf)
      }
  } else {
    while(node$height < maxDepth && 
         any(Reduce(c,node$Get('continue', format = list, filterFun = isLeaf)))){
      node$Do(splitNode, filterFun = isLeaf)
      }
  }

  ## 
  n <- node$Get('dataid', 'level', filterFun = isLeaf)
  n <- lapply(n, as.numeric)
  m <- node$Get('model')

  g <- node$Get("mean", "level", format = list)
  means <- data.frame(g)[,-1]
  colnames(means) <- gsub("X", "C", colnames(means))

  h <- node$Get("cov", "level", format = list)[-1]
  mn <- melt(n)
  mn$L1 <- as.factor(mn$L1)
  mn$col <- as.numeric(mn$L1)

  outLabels <- mn[order(mn$value), ]

  node$means <- means
  node$sigma <- abind(h, along = 3)
  node$labels <- outLabels

  return(node)
}
