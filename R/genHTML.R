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
#' @import knitr
#' @import ggplot2
#' @import stats
#' @import corrplot
#' @import rCUR
#'
#' @examples
#' require(meda)
#' x <- iris[, -5]
#' y <- data.frame(matrix(rnorm(1e3*24, mean = rep(c(1,4,2,5), each =
#' 1e3), sd = 0.25), ncol = 24))
#' load("~/neurodata/synaptome-stats/Code/cleanDataWithAttributes.RData")
#' 
#' outfile.1 <- paste0('~/Desktop/ex1.html')
#' outfile.2 <- paste0('~/Desktop/ex2.html')
#' outfile.3 <- paste0('~/Desktop/ex3.html')
#' use.plotly <- FALSE
#' scale <- TRUE
#' print("Now run 'genHTML(x, outfile, use.plotly, scale)'")
#' \dontrun{
#' x <- ex1[, 1:24, with = FALSE]
#' genHTML(x, outfile.1, use.plotly, scale)
#' genHTML(y, outfile.2, use.plotly, scale)
#' genHTML(data01[, 1:24, with = FALSE], outfile.3, use.plotly, scale)
#' p.heat(data01, FALSE, dmethod = "samp", nmethod = "kmpp")
#' p.heat(iris[, -5], FALSE, dmethod = "cur", nmethod = "kmpp")
#' cp <- comp(iris[, -5], dir = 2, nmethod='kmpp', nnum = 75)
#' }
#'
#' @export


genHTML <- function(x, outfile, use.plotly = TRUE, 
                    scale = FALSE, dmethod = "pca", 
                    nmethod = "samp") {

  use.plotly <<- use.plotly
  n <<- dim(x)[1]
  p <<- dim(x)[2]

  dmethod <- "pca"
  nmethod <- "kmpp"

  if(scale){
    dat <<- scale(x, center = TRUE, scale = TRUE) 
  } else {
    dat <<- x
  }
 
  maxchar <- max(nchar(as.character(colnames(dat))))
  #ifelse(maxchar > 8, 


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
          kmpp = {out <- kmpp(dx, k = nnum, runkm = FALSE)}
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
      theme(axis.text.x = element_text(angle = 0, vjust = 0.5), 
            panel.background = element_blank())

    return(gg.heat)
    }
}

#' Generate violin/jitter plot of data
#'
#' @param dat data
#' @param use.plotly Boolean to use plotly
#' 
#' @return A heatmap plot
#'
#' @export 
### Violin or Jitter plots
p.violin <- function(dat, use.plotly) {

  mdat <- data.table::melt(as.data.frame(dat), id = NULL)
  gg <- ggplot(mdat, aes(x = factor(variable), y = value)) +
          xlab("Var") + ylab("value")

  gg.violin <- if(n > 1000){
      gg + geom_violin() + coord_flip()
    } else {
      gg + 
        geom_point(alpha=0.3) + 
        geom_jitter(width = .6) 
    }
  return(gg.violin)
}


#' Generate a correlation plot
#'
#' @param dat data
#'
#' @return A correlation plot
#' @importFrom stats cor
#' @export 
### Correlation plots 
p.cor <- function(dat) {
  out <- list(corr = cor(dat), method = "color", tl.cex = 1)
  return(out)
}


#' Generate an outlier plot
#'
#' @param dat data
#' @param alev alpha level
#'
#' @return An outlier plot
#'
#' @import ggplot2
#' @importFrom robustbase covMcd
#' @importFrom stats mahalanobis
#' @export 
### Outlier plots
p.outlier <- function(dat, alev = 0.01) {

  ## Create data.frame of robust distances (rd) 
  ## as in Hubert et al. 2008
  ## calculated with FAST MCD or covOGK
  mcd <- covMcd(dat)
  tmp <- 1:dim(dat)[1]

  mx <- sqrt(mahalanobis(dat, center = mcd$center, cov = mcd$cov))
  RD <- data.frame(index = as.integer(tmp), rd = mx) 


  aline <- sqrt(qchisq(1 - alev / 100, p, ncp = 0, lower.tail = TRUE, log.p = FALSE))

  RD$outlier <- factor(RD$rd < aline, labels = c("outlier", "inlier"))

  tmp <- t(sapply(RD$rd, function(x) x < aline, simplify=TRUE))

  gg.outlier <- 
    ggplot(data = RD, aes(x = index, y = rd, color = outlier )) + 
  	  geom_point() + 
      labs(list(color = 
        bquote(paste("Outliers at level ", alpha, "=", .(alev))))) +
      geom_hline(yintercept = aline) + 
      ggtitle("Robust Distances of the data") +
      ylab("Robust Distances")

  return(gg.outlier)
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
#' @importFrom stats prcomp
#' @export 
### Pairs Plots
p.pairs <- function(dat) {
  pca <- prcomp(dat, center = TRUE, scale = TRUE)
  du <- ifelse(dim(pca$x)[2] > 8, 8, dim(pca$x)[2])
  
  t1 <-paste("pairs plot of first", du, "dimensions")
  t2 <-paste("pairs plot of first", du, "PCs")

  pairs(dat[, 1:du], pch = '.',  main = t1)
  pairs(pca$x[,1:du], pch = '.', main = t2)
}


#' Generate a BIC plot
#'
#' @param dat data
#' @param timeLimit Time limit for bic computation.
#'
#' @return A BIC plot
#' @importFrom mclust mclustBIC
#' @export 
### BIC plot
p.bic <- function(dat, timeLimit = 8*60 ) {
  local({
    setTimeLimit(cpu = timeLimit, transient = FALSE)
    bicO <<- mclust::mclustBIC(dat, G = 1:10)
  })
  print(summary(bicO))
  plot(bicO) 
}


#' Generate mclust output
#'
#' @param dat data
#'
#' @return mclust classification output
#' @export 
### Mclust Classifications 
p.mclust <- function(dat) {
  if(dim(dat)[1] > 1e5 & dim(dat)[2] > 100){
    stop("Dimensions are too large.")
    } else {
     mod1 <- Mclust(dat, x = bicO)
     plot(mod1, "classification") 
    }
}

