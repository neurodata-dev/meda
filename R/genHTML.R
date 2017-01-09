#' Generate an Rmarkdown with exploratory plots
#'
#' @param x data 
#' @param outfile A string denoting the location to put the output html
#' file.
#' @param use.plotly A Boolean to specify if ggplotly is used.
#' @param scale A Boolean to specify whether column-wise scaling is
#' performed before analysis.
#' @param dmethod a string denoting the method to use for dimension
#' reduction 
#' @param nmethod a string denoting the method to use for sample
#' reduction 
#' @param truth an n length vector of integer values corresponding to
#' the true classes of the data.
#' @param colCol an n length vector of column colors.
#' @param colRow an n length vector of column colors.
#' 
#' @return An html document via RMarkdown.
#'
#' @details Generates an html file of various exploratory plots via
#' RMarkdown. "There is no excuse for failing to plot and look." ~ J. W.
#' Tukey (Exploratory Data Analysis, p 43)
#'       
#' @seealso \code{\link[rmarkdown]{render}}
#'
#' @importFrom graphics pairs plot
#' @importFrom grDevices gray.colors
#' @importFrom rmarkdown render
#' @importFrom plotly plot_ly
#' @importFrom stats cor
#' @import knitr
#' @import irlba
#' @import ggplot2
#' @import stats
#' @import corrplot
#' @import rCUR
#'
#' @examples
#' require(meda)
#' dat <- iris[, -5]
#' truth <- as.numeric(iris[, 5])
#' featCol <- c("red", "purple", "darkgreen")
#' #p.heat(dat)
#' p.violin(dat)
#' p.outlier(dat)
#' do.call(corrplot, p.cor(dat))
#' p.cumvar(dat)
#' p.pairs(dat)
#' out <- p.bic(dat)
#' p.mclust(out$dat, out$bicO)
#' p.hmclust(dat, truth = iris[,5]) 
#' 
#' @export


genHTML <- function(x, outfile, use.plotly = FALSE, 
                    scale = FALSE, dmethod = "cur", 
                    nmethod = "cur", truth = NULL, 
                    colCol = NULL, colRow = NULL) {

  use.plotly <- use.plotly

  if(scale){
    dat <- scale(x, center = TRUE, scale = TRUE) 
  } else {
    dat <- as.matrix(x)
  }
   
  maxchar <- max(nchar(as.character(colnames(dat))))

  ### Structure of Data
  colStr <- table(Reduce(c,lapply(as.data.frame(dat), class)))
  complete <- all(complete.cases(dat))
  nas <- anyNA(dat)
  negs <- any(dat < 0)

  rmd <- system.file("extdata", "skeleton.Rmd", package = "meda")

  render(rmd, output_file = outfile)
}


#' Data compression 
#'
#' @param dat data
#' @param dir direction in which to compress, '1' indicates rows, '2'
#' indicates columns, and '3' performs columns then rows. 
#' @param nmethod a string specifying the method to use for compressing
#' rows. See details for options.
#' @param dmethod a string specifying the method to use for compressing
#' columns. See details for options.
#' @param nnum a number indicating desired sample size.
#' @param dnum a number indicating desired dimension. 
#'
#' @return a compressed version of the input data.
#'
#' @details Data compression:  
#' nmethod: \code{samp} random sampling, \code{kmpp} kmeans++
#' initialization, \code{cur} CUR decomposition.    
#' dmethod: \code{samp} random sampling, \code{pca} pca, \code{irlba}
#' irlba to compute parital svd, \code{cur} CUR decomposition.  
#'
#' @importFrom rCUR CUR
#'
#' @examples
#' require(meda)
#' dat <- iris[,-5]
#' dat <- comp(dat,2,dmethod = 'cur', dnum=2)
#' @export 
### Data compression
comp <- function(dat, dir = 2, nmethod = "samp", dmethod = "samp", nnum = 1e3, dnum = 100){

 X <- as.matrix(dat) 

 nmethod <- tolower(nmethod)
 dmethod <- tolower(dmethod)

 if(dmethod == 'pca' && nrow(X) >= 400 && ncol(X) > dnum) dmethod <- "irlba"

 dcomp <- function(dx, method){
   if(dnum >= ncol(X)) stop("dnum >= ncol(dat), recheck your dimensions!")
   switch(method, 
          samp = {out <- dx[, sample(1:dim(X)[2], dnum)]},
          pca  = {out <- prcomp(dx, center = TRUE, scale = TRUE)$x}, 
          irlba = {out <- dx %*% irlba(dx, nv=dnum, center = apply(dx,2,mean), scale = apply(dx,2,sd))$v;
                   colnames(out) <- paste0("PC", 1:ncol(out))},
          cur  = {out <- rCUR::CUR(dx, k = dnum, method = 'random')@C[, 1:dnum];
                   colnames(out) <- paste0("CUR", 1:ncol(out))}
          )
   return(out)
 } ##end dcomp

 ncomp <- function(dx, method){
   if(nnum >= nrow(X)) stop("nnum >= nrow(dat), recheck your dimensions!")
   switch(method, 
          samp = {out <- dx[sample(1:dim(X)[1], nnum),]},
          kmpp = {out <- kmpp(dx, k = nnum, runkm = FALSE)},
          cur  = {out <- rCUR::CUR(dx, r = nnum, k = dnum, method = 'random')@R}
          #cur  = { out <- rCUR::CUR(dx, r = nnum, k = dnum, method = 'random')@R [1:nnum, ]}
          )
   return(out)
 } ##end ncomp

 compDat <- switch(dir, 
                   ncomp(X, nmethod),
                   dcomp(X, dmethod),
                   ncomp(dcomp(X, dmethod), nmethod)
                   )

 return(compDat)
} ###END comp

#' Try to plot data
#'
#' @param FUN a p.* function from meda::
#' @param dat data
#' @param use.plotly Boolean for plotly use
#' 
#' @return The output of FUN or an error message.
#'
#' @export 
### Try to plot
p.try <- function(FUN, dat, use.plotly = NULL) {
  out <- tryCatch(
    {
      if(is.null(use.plotly)) {
        do.call(FUN, args = list(dat = dat))
        } else {
        do.call(FUN, args = list(dat = dat, use.plotly = use.plotly))
        }
    },
    error = function(cond) {
      message("Something bad happend, check your data and try again.")
      message(cond)
      return(NA)
    },
    warning = function(cond) {
      message("Function returned a warning, check your data and try again.")
      message(cond)
      return(NULL)
    }
  )
  return(out)
}


#' Generate a heatmap plot
#'
#' @param dat data
#' 
#' @return A heatmap plot of the data compressed if necessary.
#'
#' @importFrom heatmaply heatmaply
#' @export 
### Heatmaps 
p.heat <- function(dat){
  tmp <- dat
  heatmaply(tmp)
}


#' Generate violin/jitter plot of data
#'
#' @param dat data
#' @param use.plotly Boolean to use plotly
#' @param ... used to pass arguments to \code{\link{comp}}
#' 
#' @seealso \code{\link{comp}}
#' 
#' @return A jitter/violin plot
#'
#' @export 
### Violin or Jitter plots
p.violin <- function(dat, use.plotly, ...) {
  
  n <- dim(dat)[1]

  #if(dim(dat)[2] > 30){
  #  dat <- comp(dat, dir = 2, ..., dnum = 30)
  #  }
  
  mdat <- data.table::melt(as.data.frame(dat), id = NULL)

  gg <- ggplot(mdat, aes(x = factor(variable), y = value)) +
          xlab("Var") + ylab("value")

  gg.violin <- if(n > 1000){
    samp <- lapply(unique(mdat$variable), function(x){ 
                     out <- NULL
                     if(nrow(mdat[mdat$variable == x, ]) > 1e2){
                       sm <- sample(nrow(mdat[mdat$variable == x,]), 1e2)
                       out <- mdat[mdat$variable == x,][sm,]
                     }
                     return(out)
              })
    smdat <- Reduce("rbind", samp)
    gg + 
      geom_jitter(data = smdat, aes(x = factor(variable), y = value), 
                  shape = 19, size = 1, alpha = 1/10, 
                  width = 0.2) +
      geom_violin(alpha = 0.15, colour = 'red3') + 
      coord_flip()
    } else {
      gg + 
        geom_jitter(shape = 19, alpha = 0.25, width = .6) + 
        geom_violin(alpha = 0.25, color = 'red3')
    }
  return(gg.violin)
}


#' Generate a correlation plot
#'
#' @param dat data
#' @param nmethod a string specifying the method to use for compressing
#' rows.
#' @param dmethod a string specifying the method to use for compressing
#' columns
#'
#' @return A correlation plot
#' @export 
### Correlation plots 
p.cor <- function(dat, nmethod = "samp", dmethod = "samp") {
  #if(nrow(dat) > 1e5){
  #  dat <- comp(dat, dir = 1, nmethod = nmethod, nnum = 1e3)
  #}
  out <- list(corr = cor(dat), method = "color", tl.cex = 1)
  return(out)
}


#' Generate an outlier plot
#'
#' @param dat data
#' @param k number of neighbors if nrow(dat) in (1e4,1e5]
#' @param ... Unused 
#' @return An outlier plot
#' @details
#' For sample size <= 1e4 an outlier measure is calculated from the 
#' randomForest package, points with measure greater than 3 sd from the
#' mean are considred outliers.  Need to add case for n > 1e4.
#' @import ggplot2
#' @importFrom randomForest randomForest outlier
#' @importFrom rflann Neighbour
#' @export 
### Outlier plots
p.outlier <- function(dat, k = sqrt(dim(dat)[1]), ...) {

  n <- dim(dat)[1]

  #if(n > 1e5){
  #  dat <- comp(dat, dir = 1, nmethod = "samp", nnum = 1e4)
  #}

  if(n <= 1e4) {
    rf1 <- randomForest(dat, proximity = TRUE)
    out <- outlier(rf1)
   
    l1 <- mean(out) + 3*sd(out)
    status <- factor(out < l1, labels = c("outlier", "inlier"))
    df1 <- data.frame(x = 1:length(out), outD = out, status = status) 
    p <- 
      ggplot(data = df1, aes(x = x, y = outD, color = status)) + 
      geom_point() + geom_hline(yintercept = l1, aes(label = l1), show.legend = TRUE) + 
      ylab("Outlier Measure") + xlab("index") + 
      ggtitle("Outliers 3*sd from mean")
      
    return(p)
  }

  if(n > 1e4 & n <= 1e5) {
    dat <- as.matrix(dat)
    KNN <- Neighbour(dat, dat, k = (sqrt(nrow(dat))+1), build = 'kmeans', cores = 0, checks = 1)
    KNN$indices <- KNN$indices[, -1] 
    KNN$distances <- KNN$distances[, -1]

    mdk <- c()
    for(i in 1:nrow(KNN$indices)){
      ind <- c(i,KNN$indices[i,])
      d2 <- as.matrix(dist(dat[ind, ]))[-1,1]^2
      mdk[i] <- mean(d2)
    }

    l1 <- mean(mdk) + 3 * sd(mdk)
    status <- factor(mdk < l1, labels = c("outlier", "inlier"))
    df1 <- data.frame(x = 1:length(mdk), y = mdk, status = status)

    p <- 
      ggplot(data = df1, aes(x = x, y = y, color = status)) + 
      geom_point() + geom_hline(yintercept = l1, aes(label = l1), show.legend = TRUE) + 
      ylab("Mean Difference from KNN") + xlab("index") + 
      ggtitle("Outliers 3*sd from mean, K = sqrt(n)")

    return(p)
  } else {
    tmp <- comp(dat, dir = 1, nmethod = "samp", nnum = 1e3)
    rf1 <- randomForest(tmp, proximity = TRUE)
    out <- outlier(rf1)
   
    l1 <- mean(out) + 3*sd(out)
    status <- factor(out < l1, labels = c("outlier", "inlier"))
    df1 <- data.frame(x = 1:length(out), outD = out, status = status) 
    p <- 
      ggplot(data = df1, aes(x = x, y = outD, color = status)) + 
      geom_point() + geom_hline(yintercept = l1, aes(label = l1), show.legend = TRUE) + 
      ylab("Outlier Measure") + xlab("index") + 
      ggtitle("Outliers 3*sd from mean, using a sample")
      
    return(p)
  }
} ## END p.outlier


#' Generate a cumulative variance plot
#'
#' @param dat data
#'
#' @return A correlation plot
#' @details Uses getElbows from
#' \url{http://www.cis.jhu.edu/~parky/Synapse/getElbows.R}
#' @import ggplot2
#' @importFrom stats prcomp
#' @export 
### Cumulative variance
p.cumvar <- function(dat){
  tryCatch(source("http://www.cis.jhu.edu/~parky/Synapse/getElbows.R"))   
  
  n  <- dim(dat)[1]

  sv <- svd(dat, nu = 0, nv = 0)$d
  tryCatch(elb <- getElbows(sv, plot = FALSE))

  CS <- data.frame(index = 1:length(sv), cs = (100*cumsum(sv / sum(sv))))
  CS$col <- "" 
  tryCatch(CS$col[elb] <- "elbow")
  CS$col <- as.factor(CS$col)
   
  gg.cumvar <- 
    ggplot(CS, aes(x = index, y = cs)) + 
    scale_color_manual(values = c("black", "red")) + 
    geom_line() + 
    geom_point(aes(size = as.integer(col), color = col)) + 
    scale_size_continuous(guide = FALSE) + 
    ylab("% Cumulative Variance") + 
    ggtitle("Cumulative Sum of variace in PC's")

  return(gg.cumvar)
}


#' Generate a pairs hex binned plot
#'
#' @param dat data
#'
#' @return A lattice splom
#' @importFrom hexbin BTC
#' @importFrom lattice splom
#'
#' @export 
### Pairs Plots
p.pairs <- function(dat) {

  du <- ifelse(dim(dat)[2] > 8, 8, dim(dat)[2])
  t1 <-paste("pairs hex binned plot of first", du, "dimensions")


  tmp <- dat[, 1:du]

  ### the following from https://procomun.wordpress.com/2011/03/18/splomr/
  splom(tmp,
   panel=panel.hexbinplot,
   colramp=BTC,
   diag.panel = function(x, ...){
   yrng <- current.panel.limits()$ylim
   d <- density(x, na.rm=TRUE)
   d$y <- with(d, yrng[1] + 0.95 * diff(yrng) * y / max(y) )
   panel.lines(d)
   diag.panel.splom(x, ...)
   },
   lower.panel = function(x, y, ...){
   panel.hexbinplot(x, y, colorkey = TRUE, ...)
   panel.loess(x, y, ..., col = 'red')
   panel.lmline(x, y, ..., col = 'orange')
   },
   pscale=0, varname.cex=0.7
   )
} ## END FUNCTION


#' Generate a BIC plot
#'
#' @param dat data
#' @param timeLimit Time limit for bic computation.
#' @param print boolean for printing and plotting output.
#'
#' @return A BIC plot and as a side-effect
#' @details Uses getElbows from
#' \url{http://www.cis.jhu.edu/~parky/Synapse/getElbows.R}
#'
#' @importFrom mclust mclustBIC
#' @examples
#' out <- p.bic(iris[, -5])
#' @export 
### BIC plot
p.bic <- function(dat, timeLimit = 8*60, print = FALSE) {
  tryCatch(source("http://www.cis.jhu.edu/~parky/Synapse/getElbows.R"))   

  out <- NULL

  sv <- svd(dat, nu = 0, nv = 0)$d
  tryCatch(elb <- getElbows(sv, plot = FALSE))

  #setTimeLimit(cpu = timeLimit, transient = FALSE)
  bicO <- mclust::mclustBIC(dat, G = 1:10)
  out <- list(bic = bicO, data = dat)

  if(print) print(summary(bicO))

  plot(bicO) 

  return(out)
}


#' Generate mclust output
#'
#' @param dat data that p.bic has been run on
#' @param bic output from p.bic or \code{\link[mclust]{mclustBIC}}
#' @param truth true labels if any
#' @param print Boolean to print parameter estimates.
#'
#' @return mclust classification output
#' @examples
#' dat <- iris[, -5]
#' out <- p.bic(dat)
#' truth <- iris[, 5]
#' p.mclust(out$dat, out$bicO, truth = NULL)
#' @export 
### Mclust Classifications 
p.mclust <- function(dat, bic, truth = NULL, print = FALSE) {

  n <- nrow(dat)
  d <- ncol(dat)

  mod1 <- Mclust(dat, x = bic)
  if(print) print(mod1$parameters[[3]])

  shape <- if(exists('truth') && !is.null(truth)){
    as.numeric(factor(truth))
  } else {
    19
  }

  size <- max(min(1/log(n), 1), 0.05)

  if(d > 8){
    pairs(as.data.frame(dat)[, 1:8], 
          col = mod1$classification, 
          pch = shape,
          cex = size,
          main = "Shape is truth, if given; color is classification\n Pairs plot of first 8 dimensions")
  } else {
    pairs(as.data.frame(dat), 
          col = mod1$classification, 
          pch = shape,
          cex = size, 
          main = "Shape is truth, if given; color is classification")
  }
}

#' Generate binary hierarchical mclust output
#'
#' @param dat data that p.bic has been run on
#' @param truth true labels if any
#'
#' @return binary hierarchical mclust classification output
#' @examples
#' dat <- iris[, -5]
#' truth <- iris[,5]
#' p.hmclust(dat, truth)
#' @export 
### Binary Hierarchical Mclust Classifications 
p.hmclust <- function(dat, truth = NULL) {

  dat <- as.matrix(dat)
  d <- ncol(dat)
  n <- nrow(dat)
 
  size <- max(min(1/log(n), 1), 0.05)

  shape <- if(!is.null(truth)){ 
    as.numeric(factor(truth))
  } else {
    20
  }
  

  if(d > 8){
    lab <- hmc(dat[, 1:8])
    labL <- lab[, dim(lab)[2]]
    pairs(dat[, 1:8], 
          pch = shape, 
          col =  labL, 
          cex = size,
          main = "Shape is truth, if given; color is classification")
  } else {
    lab <- hmc(dat)
    labL <- lab[, dim(lab)[2]]
    pairs(dat, 
          pch = shape, 
          col =  labL, 
          cex = size, 
          main = "Shape is truth, if given; color is classification")
  }


  outHMCclusters <<- lapply(unique(labL), function(x){ 
                  list(class = labL,
                       mean = apply(dat[labL == x,], 2, mean),
                       cov = cov(dat[labL == x,])
                       )
          }
  )
  #return(out)
}


#' Generate jittered scatter plots colored by class / cluster
#'
#' @param dat data 
#' @param clusterLab true or predicted labels if any
#'
#' @return A jittered scatter plot
#' @importFrom RColorBrewer brewer.pal
#' @importFrom utils stack 
#' @examples
#' dat <- iris[, -5]
#' clusterLab <- iris[, 5]
#' p.jitter(dat, clusterLab)
#' @export 
### Jittered scatter plots

p.jitter <- function(dat, clusterLab = NULL) {

  if(is.null(clusterLab)){
    clusterLab <- rep(1, dim(dat)[1])
  }

  dat <- as.data.frame(dat)
  l <- length(unique(clusterLab))
  ggCol <- brewer.pal(min(length(unique(clusterLab)),9),"Set1")
  
  cLab <- factor(clusterLab, ordered = FALSE) 
  gdat <- data.frame(stack(dat), cLab)

  gg.jitter <- 
    ggplot(data = gdat, aes(x = ind, y = values, color = cLab)) +
    scale_color_discrete(name="Cluster") + 
    geom_point(alpha=0.25, position=position_jitterdodge()) +
    geom_boxplot(alpha =0.35, outlier.color = NULL) +
    xlab("") + ylab("")

  return(gg.jitter)
}


#' Generate 3D pca of correlation matrix
#'
#' @param dat data 
#' @param colCol colors for the columns of the data matrix
#'
#' @return a 3d scatter plot of first three PCs.
#' @examples
#' dat <- iris[, -5]
#' truth <- iris[, 5]
#' colCol <- c("red", "green", "blue", "purple")
#' p.3dpca(dat, colCol)
#' @export 
### Mclust Classifications 
p.3dpca <- function(dat, colCol = NULL) {

  dat <- as.matrix(dat)
  cor <- cor(dat)
  if(is.null(colCol)){ colCol <- "darkblue" }

  W <- svd(cor, nv = 3, nu = 0)$v
  pca <- cor %*% W
  
  rgl::plot3d(pca[,1],pca[,2],pca[,3],type='s', 
              col = colCol, xlab = "PC1", 
              ylab = "PC2", zlab = "PC3")

  rgl::rgl.texts(pca[,1],pca[,2],pca[,3],
                 col = colCol,
                 abbreviate(rownames(pca)),
                 adj=c(0,2))

  subid <- currentSubscene3d()
  rglwidget(elementId="rgl-pca0",width=720,height=720)
  
} ## END p.3dpca


