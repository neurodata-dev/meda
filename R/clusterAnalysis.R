#' Generate cluster parameter plots
#'
#' @param L of class hmc
#' @param ccol colors for features
#'
#' @return heatmap and line plot of cluster means
#' @export 
#' @examples
#' dat <- iris[, -5]
#' truth <- iris[, 5]
#' L <- hmc(dat, truth = truth, modelNames = c("VVV"))
#' clusterMeans(L, ccol = 1:4)
### Model Parameter Plots
clusterMeans <- function(L, ccol = "black") {
  modMeans <- L$dat$means
  cf <- L$dat$ClusterFraction
  ccol <- rev(ccol)

  means <- as.matrix(modMeans)
  colnames(means) <- 
    if(!is.null(colnames(means))){
      as.factor(colnames(means))
    } else {
      as.factor(paste0("C", 1:ncol(means)))
    }

  d1 <- melt(means)

  d1$ClusterFraction <- (rep(cf, times = table(d1$Var2)))
  d1$alp <- (1 - d1$ClusterFraction) + min(d1$Cluster)
  
  g1 <- 
    ggplot(d1, aes(x = Var1, y = Var2, fill = value)) + 
    geom_raster() + 
    coord_flip() + 
    scale_x_discrete(name="", limits = rev(levels(d1$Var1))) +
    theme(axis.title = element_blank(), 
          axis.text.x = element_text(angle = 90),
          axis.text.y = element_text(color = ccol))

  g2 <- 
    #ggplot(d1, aes(x = Var1, y = value, group = Var2, color = as.factor(Var2), size = ClusterFraction)) + 
    ggplot(d1, aes(x = Var1, y = value, group = Var2)) +
    geom_line(aes(colour = as.factor(Var2), alpha = alp, size = d1$ClusterFraction)) + 
    coord_flip() + 
    scale_x_discrete(name="", limits = rev(levels(d1$Var1))) +
    scale_color_discrete(name = "Cluster") + 
    theme(axis.title = element_blank(), 
          axis.text.y = element_text(color = ccol),
          legend.direction = "horizontal", 
          legend.position = "bottom") + 
    guides(alpha = FALSE, size = FALSE, colour = guide_legend(override.aes = list(size = 3)))

  out <- list(pheat = g1, pline = g2)
  OUT <- grid.arrange(out$pheat, out$pline, ncol=2)
  invisible(OUT)
} ### END clusterMeans



#' Generate cluster covariance/correlation plots
#'
#' @param x object of type hmc
#' @param ... used for 'ccol' feature colors
#'
#' @return heatmaps of cluster covariances.
#' @importFrom data.table melt
#'
#' @examples
#' dat <- iris[, -5]
#' truth <- iris[, 5]
#' L <- hmc(dat, truth = truth, modelNames = c("VVV"))
#' plot(L, ccol = 1:4)
#' clusterCov(L)
#' @export 
#' @method plot clusterCov
### Cluster Covariance Plots
plot.clusterCov <- function(x, ...){
  
  ccov <- x
  #ccov <- x$dat
  ccol <- if(!is.null(list(...)$ccol)){
    list(...)$ccol
  } else {
    "black"
  }
  m1 <- data.table::melt(ccov$dat)
  m1$Var2 <- ordered(m1$Var2, levels = rev(levels(m1$Var1)))
  m1$Var3 <- factor(sprintf("Cluster_%02d", m1$Var3))
 
  m1v <- max(abs(m1$value))

  g1 <-
    ggplot(m1, aes(x = Var1, y = Var2, group = Var3, fill = value)) +
    geom_raster() +
    scale_fill_gradient2(
      low = "darkred", 
      mid = "gray98",
      high = "darkblue",
      midpoint = 0) +
    facet_wrap(~ Var3, ncol = 2) +
    theme(axis.title = element_blank(),
          axis.text.x = element_text(angle = 90, color = ccol),
          axis.text.y = element_text(color = rev(ccol)))

  out <- g1
  return(out)
} ### END plot.clusterCov


#' Generate cluster covariance plots
#'
#' @param x an object of type hmc of clusterCov
#' 
#' @return a cluster covariance plot
#' 
#' @examples
#' dat <- iris[, -5]
#' truth <- iris[, 5]
#' L <- hmc(dat, truth = truth, modelNames = c("VVV"))
#' clusterCov(L$dat$sigma)
#' @export 
clusterCov <- function(x){
  ccov <- if(class(x)[[1]] == "hmc"){
    x$dat$sigma
  } else {
    if(class(x)[[1]] == "clusterCov"){
      x
    }
  }
  plot(ccov)
}


#' Generate stacked level mean plot
#'
#' @param hmcL object of type hmc
#' @param ccol colors for feature labels
#' @param centered boolen that skips level one if data was centered
#' @param maxDepth maximum number of levels to plot
#' @param depth forces via copying levels to show down to depth.
#'
#' @return a stacked level mean plot
#'
#' @importFrom ggplot2 ggplot
#' @examples
#' dat <- scale(iris[, -5], center = TRUE, scale = FALSE)
#' L <- hmc(dat)
#' plot(L, plotDend = TRUE)
#' stackM(L, centered = TRUE)
#' @export 
stackM <- function(hmcL, ccol = "black", centered = FALSE, maxDepth = Inf, depth = 5){

  tree <- hmcL$dat
  node <- Clone(tree)

  node$Set(nlevel = node$Get('level'))

  if(is.infinite(maxDepth)){ maxDepth <- node$height }
  if(maxDepth > node$height){maxDepth <- node$height }

  iStart <- if(centered){ 2 } else { 1 }
  
  M <- list()
  for(i in iStart:maxDepth){
    travi <- Traverse(node, filterFun = function(x) x$nlevel == i)
  
    gi <- Get(travi, "dataid", format = function(x) list(as.numeric(x)))
    id <- Get(travi, "dataid", format = function(x) list(as.numeric(x)))
  
    len <- Get(travi, "dataid", format = function(x) length(x))
  
    lvU <- Get(travi,"isLeaf")
  
    newLv <- i + lvU
    Set(travi, nlevel=newLv)
  
    m <- Get(travi, "mean", format = list)
  
    asdf <- mapply(function(x,y) {
              matrix(rep(x,each=y), 
                     nrow =length(x), 
                     ncol = y, 
                     byrow=TRUE)
              }, m, len, SIMPLIFY = FALSE)

    M[[i]] <- Reduce(cbind, asdf)
    rownames(M[[i]]) <- paste0("L", i, names(m[[i]]))
  }
 
  if(i < depth){
    for(j in (i+1):depth){
      M[[j]] <- M[[i]]
      rownames(M[[j]]) <- paste0("L", j,"-",i, names(m[[1]]))
    }
  }
  
  MM <- data.frame(Reduce(rbind, M))

  mt <- melt(MM, id.var = NULL)
  mt$variable <- as.numeric(gsub("X", "", mt$variable))
  
  gd <- expand.grid(rownames(MM), 1:ncol(MM))[,1]

  ggd <- data.frame(mt,gd)
  ggd$gd <- factor(gd, levels = rev(levels(gd)), ordered = TRUE)

  #pal <- rainbow(255, start = 0,end = 0.5, v = 0.5, s = 0.8)
  pal <- colorpanel(255, "black", "pink")
  pal <- colorpanel(255, "darkorchid4", "gray99", "darkgreen")

  ln <- length(names(m[[1]]))
  levsep <- seq(ln, nrow(MM), ln)[1:(nrow(MM)/ln -1)] + 0.5


  p <- 
    ggplot(ggd, aes(x = variable, y = gd, fill = value)) + 
         scale_fill_gradient2(low = "darkorchid4", 
                              mid = "gray99", 
                              high = "darkorange3",
                              midpoint = 0) + 
         #scale_fill_viridis() + 
         #scale_fill_gradientn(colours = pal) + 
         geom_tile() + 
         theme(axis.title = element_blank(),
               axis.text.y = element_text(color = rev(ccol)),
               panel.background = element_blank(),
               panel.grid.major.y =element_blank(),
               panel.ontop = FALSE) + 
         geom_hline(yintercept = levsep, size = 1)
  return(p)
} ## END stackM

