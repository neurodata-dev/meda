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
#' @import ggplot2
#' @import stats
#' @import corrplot
#' @import rCUR
#'
#' @examples
#' require(meda)
#' dat <- iris[, -5]
#' p.heat(dat, FALSE, dmethod = "samp", nmethod = "kmpp")
#' p.violin(dat)
#' p.outlier(dat)
#' do.call(corrplot, p.cor(dat))
#' p.cumvar(dat)
#' p.pairs(dat)
#' out <- p.bic(dat)
#' p.mclust(out$dat, out$bicO)
#' 
#' @export


genHTML <- function(x, outfile, use.plotly = FALSE, 
                    scale = FALSE, dmethod = "pca", 
                    nmethod = "samp") {

  use.plotly <- use.plotly

  dmethod <- "pca"
  nmethod <- "kmpp"

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
#' rows.
#' @param dmethod a string specifying the method to use for compressing
#' columns
#' @param nnum a number indicating desired sample size.
#' @param dnum a number indicating desired dimension. 
#'
#' @return a compressed version of the input data.
#'
#' @importFrom rCUR CUR
#' 
#' @export 
### Data compression
comp <- function(dat, dir = 2, nmethod = "samp", dmethod = "samp", nnum = 1e3, dnum = 100){

 X <- as.matrix(dat) 
 nmethod <- tolower(nmethod)
 dmethod <- tolower(dmethod)

 dcomp <- function(dx, method){
   s1d <- sample(1:dim(X)[2], dnum)
   switch(method, 
          samp = {out <- dx[, s1d]},
          pca  = {out <- prcomp(dx, center = TRUE, scale = TRUE)$x}, 
          cur  = {out <- rCUR::CUR(dx, k = dnum, method = 'random')@C[, 1:dnum]}
          )
   return(out)
 } ##end dcomp

 ncomp <- function(dx, method){
   s1n <- sample(1:dim(X)[1], nnum)
   switch(method, 
          samp = {out <- dx[s1n,]},
          kmpp = {out <- kmpp(dx, k = nnum, runkm = FALSE)},
          cur  = {out <- rCUR::CUR(dx, k = dnum, method = 'random')@R[1:nnum, ]}
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
#' @param use.plotly Boolean to use plotly
#' @param dmethod parameter passed to \code{\link{comp}}
#' @param nmethod parameter passed to \code{\link{comp}}
#' 
#' @return A heatmap plot of the data compressed if necessary.
#'
#' @importFrom data.table melt
#' @export 
### Heatmaps 
p.heat <- function(dat, use.plotly, dmethod = "samp", nmethod = "samp"){

  n <- dim(dat)[1] > 1e3 
  d <- dim(dat)[2] > 1e2

  if(n | d){
    case <- ifelse(n & d, 3, ifelse(d,2,1))
    dat <- comp(dat, dir = case, dmethod = dmethod, nmethod = nmethod, dnum = 1e2, nnum = 1e3)
  }

  if(use.plotly){ 
    plty.heat <- plot_ly(z = dat, type = 'heatmap')
    return(plty.heat)
  } else {
 
    mdat <- data.table::melt(as.data.frame(dat), id = NULL)
    rasf <- factor(rep(colnames(dat), each = dim(dat)[1]), levels = colnames(dat), ordered = TRUE)
    ras <- data.frame(x = rasf, y = 1:(dim(dat)[1]))
    ras$z <- mdat$value 

    gg.heat <- 
      ggplot(ras, aes(x = x, y = y, fill = z)) + 
      geom_raster() + scale_y_reverse(expand = c(0,0)) + 
      scale_fill_gradientn(colours = gray.colors(255, start = 0)) +
      xlab("") + ylab("index") +
      theme(axis.text.x = element_blank(),
            #element_text(angle = 0, vjust = 0.5), 
            panel.background = element_blank())

    return(gg.heat)
    }
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

  if(dim(dat)[2] > 30){
    dat <- comp(dat, dir = 2, ..., dnum = 30)
    }
  
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
#'
#' @return A correlation plot
#' @export 
### Correlation plots 
p.cor <- function(dat) {
  if(nrow(dat) > 1e5){
    dat <- comp(dat, dir = 1, nmethod = nmethod, nnum = 1e3)
  }
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

  if(n > 1e5){
    dat <- comp(dat, dir = 1, nmethod = "samp", nnum = 1e4)
  }

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

  pca <- prcomp(dat, center = TRUE, scale = TRUE)
  tryCatch(elb <- getElbows(pca$sdev, plot = FALSE))

  CS <- data.frame(index = 1:(dim(pca$x)[2]), cs = (100*cumsum(pca$sdev / sum(pca$sdev))))
  CS$col <- "" 
  tryCatch(CS$col[elb] <- "elbow")
  CS$col <- as.factor(CS$col)

   
  gg.cumvar <- 
    ggplot(CS, aes(x = index, y = cs)) + 
    scale_color_manual(values = c("black", "red")) + 
    geom_line() + 
    geom_point(aes(color = col, size = col)) + 
    ylab("% Cumulative Variance") + 
    ggtitle("Cumulative Sum of variace in PC's")

  return(gg.cumvar)
}




#' Generate a pairs plot
#'
#' @param dat data
#'
#' @return A pairs plot
#' @export 
### Pairs Plots
p.pairs <- function(dat) {

  du <- ifelse(dim(dat)[2] > 8, 8, dim(dat)[2])
  
  t1 <-paste("pairs plot of first", du, "dimensions")

  pairs(as.matrix(dat)[, 1:du], pch = 19,  main = t1)
}


#' Generate a BIC plot
#'
#' @param dat data
#' @param timeLimit Time limit for bic computation.
#'
#' @return A BIC plot and as a side-effect
#' @importFrom mclust mclustBIC
#' @examples
#' out <- p.bic(iris[, -5])
#' @export 
### BIC plot
p.bic <- function(dat, timeLimit = 8*60 ) {

  n <- nrow(dat)
  d <- ncol(dat)

  if(d > 100){
    dat <- comp(dat, dir=2, dnum = 100, dmethod = "cur")
  }

  if(n > 1e5){
    dat <- comp(dat, dir = 1, nnum = 1e3, nmethod = 'samp')
  }
  out <- NULL

  setTimeLimit(cpu = timeLimit, transient = FALSE)
  bicO <- mclust::mclustBIC(dat, G = 1:10)
  out <- list(bic = bicO, data = dat)

  print(summary(bicO))
  plot(bicO) 
  return(out)
}


#' Generate mclust output
#'
#' @param dat data that p.bic has been run on
#' @param bic output from p.bic or \code{\link[mclust]{mclustBIC}}
#'
#' @return mclust classification output
#' @examples
#' out <- p.bic(iris[, -5])
#' p.mclust(out$dat, out$bicO)
#' @export 
### Mclust Classifications 
p.mclust <- function(dat, bic) {

  n <- nrow(dat)
  d <- ncol(dat)

  mod1 <- Mclust(dat, x = bic)

  if(d > 8){
    pairs(as.data.frame(dat)[, 1:8], col = mod1$classification, pch = 19)
  } else {
    pairs(as.data.frame(dat), col = mod1$classification, pch = 19)
  }
}

