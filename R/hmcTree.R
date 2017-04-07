#' Generate a binary hierarchical mclust tree
#'
#' @param dat a data matrix
#' @param maxDepth maximum tree depth
#' @param modelNames passed to mclust
#'
#' @return binary hierarchical mclust classification output
#' @details BIC is run for k = {1,2}, if k = 2 then each node is
#' propagated down the tree.  If k = 1, then that node is frozen. 
#' @import data.tree
#' @importFrom abind abind
#' @importFrom mclust mclustBIC Mclust
#' @importFrom data.table melt
#'
#' @export 
#' @examples
#' dat <- iris[, -5]
#' truth <- iris[, 5]
#' L <- hmcTree(dat)
### Binary Hierarchical Mclust Classifications 
hmcTree <- function(dat, maxDepth = 6, modelNames = NULL){
  ## Helper function
  splitNode = function(node){
    if(!is.null(dim(node$data)) && 
       dim(node$data)[1] > 5 && 
       node$continue == TRUE && 
       isLeaf(node)){
       b <- mclustBIC(node$data, G = 1:2, modelNames = modelNames)
       node$bic <- b
       mc <- Mclust(node$data, x = b)
       cont <- all(table(mc$classification) > 10)
       node$G <- mc$G
       if(mc$G == 2 && cont){
         node$model <- mc
         dat1 <- node$data[mc$classification == 1,]
         dat2 <- node$data[mc$classification == 2,]

         if(dim(dat1)[1] >= dim(dat2)[1]){
           big <- "1"
           little <- "2"
           #
           node$AddChild(paste0(node$name, big), 
             data = dat1, dataid = rownames(dat1), 
             continue = TRUE,
             num = dim(dat1)[1]/tot,
             mean = mc$parameters$mean[,1], 
             cov = mc$parameters$variance$sigma[,,1],
             cor = cov2cor(mc$parameters$variance$sigma[,,1]))
           #
           node$AddChild(paste0(node$name, little), 
             data = dat2, dataid = rownames(dat2), continue = TRUE,
             num = dim(dat2)[1]/tot,
             mean = mc$parameters$mean[,2], 
             cov = mc$parameters$variance$sigma[,,2],
             cor = cov2cor(mc$parameters$variance$sigma[,,2]))
         } else {
           big <- "2"
           little <- "1"
           #
           node$AddChild(paste0(node$name, little), 
             data = dat2, dataid = rownames(dat2), continue = TRUE,
             num = dim(dat2)[1]/tot,
             mean = mc$parameters$mean[,2], 
             cov = mc$parameters$variance$sigma[,,2],
             cor = cov2cor(mc$parameters$variance$sigma[,,2]))
           #
           node$AddChild(paste0(node$name, big), 
             data = dat1, dataid = rownames(dat1), 
             continue = TRUE,
             num = dim(dat1)[1]/tot,
             mean = mc$parameters$mean[,1], 
             cov = mc$parameters$variance$sigma[,,1],
             cor = cov2cor(mc$parameters$variance$sigma[,,1]))
         }
       } else {
         node$continue = FALSE
       }
    }
  } ## END splitNode

  splitNode1 = function(node){
    if(!is.null(dim(node$data)) && 
       dim(node$data)[1] > 5 && 
       node$continue == TRUE && 
       isLeaf(node)){
       b <- mclustBIC(node$data, G = 1:2, modelNames = modelNames)
       node$bic <- b
       mc <- Mclust(node$data, x = b)
       node$G <- mc$G
       cont <- all(table(mc$classification) > 10)
       if(mc$G == 2 && cont){
         node$model <- mc
         dat1 <- node$data[mc$classification == 1,]
         dat2 <- node$data[mc$classification == 2,]

         if(length(dat1) >= length(dat2)){
           big <- "1"
           little <- "2"
         } else {
           big <- "2"
           little <- "1"
         }
         node$AddChild(paste0(node$name, big), 
                       data = dat1, dataid = rownames(dat2), 
                       continue = TRUE,
                       num = length(dat1)/tot,
                       mean = mc$parameters$mean[1], 
                       cov = mc$parameters$variance$sigma[1],
                       cor = cov2cor(mc$parameters$variance$sigma[1])
                       )
         node$AddChild(paste0(node$name, little), 
                       data = dat2, dataid = rownames(dat1), continue = TRUE,
                       num = length(dat2)/tot,
                       mean = mc$parameters$mean[2], 
                       cov = mc$parameters$variance$sigma[2],
                       cor = cov2cor(mc$parameters$variance$sigma[2])
                       )
       } else {
         node$continue = FALSE
       }
    }
  } ## END splitNode1

  dat <- as.data.frame(dat)
  tot <- dim(dat)[1]
  dataid <- if(!is.null(rownames(dat))){ 
              rownames(dat) 
              } else { 
                1:dim(dat)[1] 
              }

  node <- Node$new("", data = dat, 
                   dataid = rownames(dat), 
                   continue = TRUE, model = NULL, 
                   mean = apply(dat, 2, mean),
                   num = tot/tot)

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

  #g <- node$Get("mean", "level", format = list)
  g <- node$Get("mean", "level", format = list, filterFun = isLeaf)
  means <- data.frame(g)
  colnames(means) <- gsub("X", "C", colnames(means))

  #h <- node$Get("cov", "level", format = list)[-1]
  h <- node$Get("cov", "level", format = list, filterFun = isLeaf)
  mn <- melt(n)
  mn$L1 <- as.factor(mn$L1)
  mn$col <- as.numeric(mn$L1)

  k <- node$Get("cor", "level", format = list, filterFun = isLeaf)

  outLabels <- mn[order(mn$value), ]

  node$ClusterFraction <- node$Get("num", filterFun = isLeaf)
  node$means <- means
  node$sigma <- structure(list(dat =abind(h, along = 3)),
                          class=c("clusterCov", "array"))
  node$cor <- abind(k, along = 3)
  node$labels <- outLabels

  return(node)
}




#' Generate binary hierarchical mclust tree
#'
#' @param dat data 
#' @param truth true labels if any
#' @param maxDim maximum dimensions to plot
#' @param maxDepth maximum tree depth
#' @param modelNames model names for mclust see \code{\link[mclust]{mclustModelNames}}
#'
#' @return binary hierarchical mclust classification output
#' @details BIC is run for k = {1,2}, if k = 2 then each node is
#' propagated down the tree.  If k = 1, then that node is frozen. 
#' If a singleton exists, the level takes a step back. 
#'
#' @export 
#' @examples
#' dat <- iris[, -5]
#' truth <- NULL #iris[,5]
#' maxDim = 6; modelNames = c("VVV"); maxDepth = 6
#' d1 <- hmc(dat, truth = truth, modelNames = c("VVV"), maxDim = 6)
#' plot(d1)
#' plotDend(d1)
### Binary Hierarchical Mclust Classifications 
hmc <- function(dat, truth = NULL, maxDim = Inf, maxDepth = 6,
                  modelNames = NULL) {

  d <- dim(dat)[2]
  n <- dim(dat)[1]

  size <- max(min(1.5/log10(n), 1.25), 0.05)
  shape <- if(!is.null(truth)){ 
    as.numeric(factor(truth))
  } else {
    20
  }

  dmax <- ifelse(d > maxDim, maxDim, d)

  L <- hmcTree(dat, maxDepth, modelNames = modelNames)
  
  out <- structure(list(dat = L, dmax = dmax, shape = shape, size = size), class = "hmc")
  
  return(out)
}

#' Generate binary hierarchical mclust tree plot
#'
#' @param x an object of type hmc
#' @param ... plotDend Boolean for dendrogram plot and maxd for max
#' plotting dimension
#'
#' @method plot hmc
#' @export 
### Binary Hierarchical Mclust Classifications 
plot.hmc <- function(x, ...){

  dl <- x
  L <- dl$dat
  shape <- dl$shape
  size <- dl$size
  dmax <- ifelse(is.null(list(...)$maxd), dl$dmax, list(...)$maxd)

  print("Fraction of points in each cluster:")
  print(table(L$labels$col)/length(L$labels$col))
  
  pairs(dl$dat$data[, 1:dmax], 
        pch = shape, 
        col =  L$labels$col, 
        cex = size, 
        main = "Color is classification; if present, shape is truth"
        )
}


#' plot a dendrogram from an hmc object
#'
#' @param dl an hmc object
#'
#' @return a dendrogram plot
#' @import data.tree
#' @import dendextend
#'
#' @export 
plotDend <- function(dl){
  if(class(dl) != "hmc"){
    stop("must be of class hmc")
  } else {
    tree <- dl$dat 
    dend <- as.dendrogram(Sort(tree, "name"))
    num <- tree$Get("num")
    cond <- as.numeric(tree$Get("G", by = "level"))
    dend <- dend %>%
                 dendextend::set("branches_lwd", 10*as.numeric(num)) %>% 
                 dendextend::set("branches_lty", c(1)) %>%
                 dendextend::set("nodes_pch", c(15, 17)[cond]) %>%
                 dendextend::set("nodes_cex", 2.5) %>%
                 dendextend::set("nodes_col", c("red", "green")[cond])

    plot(dend, center = TRUE)
    round(tree$Get("num", filterFun=isLeaf), 4)
  }
}
