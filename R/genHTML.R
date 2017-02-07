#' Generate an Rmarkdown with exploratory plots
#'
#' @param x data 
#' @param outfile A string denoting the location to put the output html
#' file.
#' @param outdir directory for figures
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
#' ## p.heat(dat)
#' p.1dheat(dat)
#' ## p.violin(dat)
#' p.outlier(dat)
#' do.call(corrplot, p.cor(dat))
#' p.cumvar(dat)
#' p.cumvar(cor(dat))
#' p.pairs(dat)
#' out <- p.bic(dat)
#' mc1 <- p.mclust(out$dat, out$bicO)
#' p <- p.clusterMeans(mc1); grid.arrange(p[[1]], p[[2]], ncol = 2)
#' p.hmclust(dat, truth = iris[,5]) 
#' 
#' @export


genHTML <- function(x, outfile, outdir,  
                    scale = FALSE, dmethod = "cur", 
                    nmethod = "cur", truth = NULL, 
                    colCol = NULL, colRow = NULL) {

  outdir <- outdir
  use.plotly <- FALSE ## Remove this later

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
  sd0 <- apply(dat, 2, function(x) sd(x) == 0)
  sdZ <- any(sd0)

  ## Halt if any columns have sd = 0
 if(sdZ){
   stop(paste("Feature columns", c(1:ncol(dat))[sd0], "have standard deviation = 0!"))
   }

  rmd <- system.file("extdata", "MEDA.Rmd", package = "meda")
  file.copy(rmd, outdir, overwrite = TRUE)

  ## Colors and such 
  if(!exists("colCol") || is.null(colCol)){
    colCol <- "black"
  }

  render(paste0(outdir, "/MEDA.Rmd"), output_file = outfile)
  file.remove(paste0(outdir, "/MEDA.Rmd"))
}

#' Summary of data types
#'
#' @param dat data
#'
#' @return Summary Histogram
#' 
#' @examples
#' dat <- iris[, -5]
#' 
#' @export 

### Location Estimates 
p.summary <- function(dat){
  st <- as.factor(sapply(dat, class))
  
} ### END p.summary

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

#' Generate location estimates
#'
#' @param dat data
#' @param ccol colors for features. 
#'
#' @return a list of ggplot objects, see details. 
#' 
#' @importFrom gplots colorpanel
#' @importFrom gridExtra grid.arrange
#' 
#' @examples
#' dat <- iris[, -5]
#' l1 <- p.location(dat)
#' l2 <- p.location(dat, ccol)
#' grid.arrange(l1[[1]],l1[[2]], ncol = 2)
#' grid.arrange(l2[[1]],l2[[2]], ncol = 2)
#' @export 

### Location Estimates 
p.location <- function(dat, ccol = NULL){

  Mean <- apply(dat, 2, mean)
  Median <- apply(dat, 2, median)

  dm <- melt(cbind(Mean, Median))

  p1 <- 
    ggplot(dm, aes(x = Var1, y = Var2, fill = value)) +  
      geom_raster() +# coord_flip() + 
      theme(panel.background = element_blank(), 
            axis.title = element_blank())

  p2 <- 
    ggplot(dm, aes(x = Var1, y = value, 
                   group = Var2, color = Var2)) + 
          geom_line(alpha = 0.7, size = 1) + 
          theme(legend.title = element_blank(),
                axis.title = element_blank())

  if(dim(dat)[2] > 8){
    p1 <- p1 + coord_flip() + 
      theme(axis.text.y=element_text(color=ccol))
    p2 <- p2 + coord_flip() +
      theme(axis.text.y=element_text(color=ccol))
  } else {
    p1 <- p1 + theme(axis.text.x=element_text(color=ccol))
    p2 <- p2 + theme(axis.text.x=element_text(color=ccol))
  }

  lout <- list(pheat = p1, pline = p2)
  return(lout)
} ### END p.location


#' Generate a heatmap plot
#'
#' @param dat data
#' @param ... used to send options to heatmaply
#' @return A heatmap plot of the data compressed if necessary.
#'
#' @importFrom heatmaply heatmaply
#' @export 
### Heatmaps 
p.heat <- function(dat, ...){
  tmp <- dat
  heatmaply(tmp, ...)
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
#' @importFrom data.table melt
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
                  width = 0.3) +
      geom_violin(alpha = 0.15, colour = 'red3') + 
      coord_flip()
    } else {
      gg + 
        geom_jitter(shape = 19, alpha = 0.25, width = .4) + 
        geom_violin(alpha = 0.25, color = 'red3')
    }
  return(gg.violin)
}

#' Generate 1d heatmaps
#'
#' @param dat the data
#' @param breaks see \code{\link[graphics]{hist}}
#' @param ccol colors for features.
#' 
#' @return a ggplot object 
#'
#' @details For each feature column a 1D heatmap is generated and
#' plotted as a geom_tile object.
#'
#' @import ggplot2 
#' @importFrom gplots colorpanel
#' @importFrom data.table data.table 
#' @importFrom data.table melt
#'
#' @examples
#' dat <- iris[, -5]
#' p.1dheat(dat, ccol = ccol)
#'
#' @export 
### 1D heatmap
p.1dheat <- function(dat, breaks = "Scott", ccol = "black") {

  dat <- data.frame(apply(dat, 2, as.numeric))

  mycol <- colorpanel(255, "white", "#094620")

  sc <- scale_fill_gradientn(colours = mycol)
  
  mt <- data.table::melt(dat, measure = 1:ncol(dat))

  H <- hist(mt$value, breaks = breaks, plot = FALSE)
  df <- expand.grid(x = H$mids, y = names(dat))

  bn <- lapply(apply(dat, 2, hist, breaks = H$breaks, plot = FALSE), 
               function(x) { x$counts }) 

  df$Count <- Reduce(c, bn)
              
  p <- ggplot(df, aes(x, y, fill = Count)) + 
         geom_tile() + 
         theme(axis.title = element_blank(),
               axis.text.y=element_text(color=ccol)) + 
         sc

  return(p)
}
### END p.1dheat


#' Generate a correlation plot
#'
#' @param dat data
#' @param colCol colors for column labels
#'
#' @return A correlation plot
#' @examples 
#' dat <- iris[, -5]
#' colCol <- c("darkgreen", "red", "green", "red")
#' do.call(corrplot, p.cor(dat, colCol))
#' @export 
### Correlation plots 
p.cor <- function(dat, colCol = NULL) {
  out <- list(corr = cor(dat), method = "color", tl.cex = 1, tl.col = colCol)
  return(out)
}


#' Generate an energy distance heatmap 
#'
#' @param dat data
#' @param colCol colors for column labels
#'
#' @return A correlation plot
#' @details Takes a random sample of 1000 rows and computes
#'   energy statistics with p-values.
#' @import foreach
#' @importFrom energy dcor.ttest
#' 
#' @examples 
#' dat <- iris[, -5]
#' colCol <- c("darkgreen", "red", "green", "red")
#' @export 
### Correlation plots 
p.energy <- function(dat, colCol = NULL) {

  if(dim(dat)[1] > 1e3){
     dat <- data.frame(dat[sample(dim(dat)[1],1e3),])
  }

  combcols <- t(combn(dim(dat)[2],2))
  
  dc <- foreach(i = 1:dim(combcols)[1]) %do% {
         set.seed(331*i)
         dcor.ttest(x=dat[,combcols[i,1]],y=dat[,combcols[i,2]])
         }
  
  ms <- matrix(as.numeric(0),dim(dat)[2],dim(dat)[2])
  mp <- matrix(as.numeric(0),dim(dat)[2],dim(dat)[2])
  
  for(i in 1:length(dc)){
      ms[combcols[i,1],combcols[i,2]] <- dc[[i]]$statistic
      ms[combcols[i,2],combcols[i,1]] <- dc[[i]]$statistic
      mp[combcols[i,1],combcols[i,2]] <- dc[[i]]$p.val
      mp[combcols[i,2],combcols[i,1]] <- dc[[i]]$p.val
  }
  
  rownames(ms) <- colnames(dat)
  rownames(mp) <- colnames(dat)
  colnames(ms) <- colnames(dat)
  colnames(mp) <- colnames(dat)
  
  diag(ms) <- as.numeric(0)
  diag(mp) <- as.numeric(1)

  out <- list(statistic = ms, pval = mp)
  return(out)
} ### END p.energy


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
#' @param maxd maximum dimensions to plot
#' @param colramp Color Ramp used, BTC or BTY
#' @param ccol colors for features
#' @param loess boolean for loess curve
#' @param lmline boolean for lm line
#'
#' @return A lattice splom
#' @importFrom hexbin BTC
#' @importFrom lattice splom
#'
#' @examples
#' dat <- iris[,-5]
#' p.pairs(dat, colramp = magent, ccol = c("blue", "red"))
#' p.pairs(dat, colramp = BTC)
#' @export 
### Pairs Plots
p.pairs <- function(dat, maxd = Inf, colramp = BTC, ccol = "black", loess = TRUE, lmline = TRUE) {

  d <- dim(dat)[2]
  du <- ifelse(d > maxd, maxd, d)

  if(d > maxd){
    tmp <- dat[, 1:du]
    t1 <-paste("pairs hex binned plot of first", du, "dimensions")
    if(ccol != "black"){ ccol <- ccol[1:du] }
  } else {
    tmp <- dat
    t1 <-paste("pairs hex binned plot of the data")
  }

  ### the following from https://procomun.wordpress.com/2011/03/18/splomr/
  splom(tmp,
   as.matrix = TRUE,
   panel=panel.hexbinplot,
   colramp=colramp,
   #style = "nested.centroids",
   diag.panel = function(x, ...){
   yrng <- current.panel.limits()$ylim
   d <- density(x, na.rm=TRUE)
   d$y <- with(d, yrng[1] + 0.95 * diff(yrng) * y / max(y) )
   panel.lines(d)
   diag.panel.splom(x, ...)
   },
   lower.panel = function(x, y, ...){
   panel.hexbinplot(x, y, colorkey = TRUE, ...)
   if(loess) panel.loess(x, y, ..., col = 'red')
   if(lmline) panel.lmline(x, y, ..., col = 'orange')
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
  #tryCatch(source("http://www.cis.jhu.edu/~parky/Synapse/getElbows.R"))   
  #sv <- svd(dat, nu = 0, nv = 0)$d
  #tryCatch(elb <- getElbows(sv, plot = FALSE))

  #setTimeLimit(cpu = timeLimit, transient = FALSE)
  out <- NULL
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
#' @param maxd maximum dimensions to plot NOT USED YET
#' @param print Boolean to print parameter estimates.
#'
#' @return mclust classification output
#' @examples
#' dat <- iris[, -5]
#' out <- p.bic(dat)
#' truth <- iris[, 5]
#' tryCatch(md1 <- p.mclust(out$dat, out$bicO, truth = truth))
#' @export 
### Mclust Classifications 
p.mclust <- function(dat, bic, truth = NULL, maxd = Inf, print = FALSE) {

  n <- nrow(dat)
  d <- ncol(dat)

  dmax <- ifelse(d > maxd, maxd, d)

  mod1 <- Mclust(dat, x = bic)
  if(print) print(mod1$parameters[[3]])

  shape <- if(exists('truth') && !is.null(truth)){
    as.numeric(factor(truth))
  } else {
    19
  }

  size <- max(min(1.5/log10(n), 1.25), 0.05)

  if(d > dmax){
    pairs(as.data.frame(dat)[, 1:dmax], 
          col = mod1$classification, 
          pch = shape,
          cex = size,
          main = paste("Color is classification; if present, shape is truth\n Pairs plot of first", dmax, "dimensions")
          )
  } else {
    pairs(as.data.frame(dat), 
          col = mod1$classification, 
          pch = shape,
          cex = size, 
          main = "Color is classification; if present, shape is truth"
         )
  }
  invisible(mod1)
}

#' Generate binary hierarchical mclust output
#'
#' @param dat data 
#' @param truth true labels if any
#' @param maxd maximum dimensions to plot
#'
#' @return binary hierarchical mclust classification output
#' @details BIC is run for k = {1,2}, if k = 2 then each node is
#' propagated down the tree.  If k = 1, then that node is frozen. 
#' If a singleton exists, the level takes a step back. 
#' @examples
#' dat <- iris[, -5]
#' truth <- iris[,5]
#' hmcL <- p.hmclust(dat, truth)
#' L <- p.hmclust(dat)
#' @export 
### Binary Hierarchical Mclust Classifications 
p.hmclust <- function(dat, truth = NULL, maxd = Inf) {

  dat <- as.matrix(dat)
  d <- ncol(dat)
  n <- nrow(dat)
 
  dmax <- ifelse(d > maxd, maxd, d)

  size <- max(min(1.5/log10(n), 1.25), 0.05)

  shape <- if(!is.null(truth)){ 
    as.numeric(factor(truth))
  } else {
    20
  }
  
  lab <- hmc(dat)
  
  singles <- apply(lab, 2, function(x) any(table(x) == 1))
  mi <- max(which(singles == FALSE))

  if(all(singles)){
    labL <- lab[, dim(lab)[2]]
  } else {
    labL <- lab[, mi]
  }

  outHMCclusters <- lapply(unique(labL), function(x){ 
                  list(class = x,
                       mean = apply(dat[labL == x,], 2, mean),
                       cov = cov(dat[labL == x,]),
                       cor = cor(dat[labL == x,])
                       )
          }
  )

  means <- sapply(outHMCclusters, '[[', 2)
  covs <- sapply(outHMCclusters, '[[', 3, simplify = FALSE)
  covs <- array(unlist(covs), dim = c(dim(covs[[1]]), length(covs)))
  colnames(covs) <- colnames(dat)
  rownames(covs) <- colnames(dat)

  cors <- sapply(outHMCclusters, '[[', 4, simplify = FALSE)
  cors <- array(unlist(cors), dim = c(dim(cors[[1]]), length(cors)))
  colnames(cors) <- colnames(dat)
  rownames(cors) <- colnames(dat)

  outL <- list(mean = means, sigma = covs, cor = cors, hlabels = lab, leaves = labL) 

  pairs(dat[, 1:dmax], 
        pch = shape, 
        col =  labL, 
        cex = size, 
        main = "Color is classification; if present, shape is truth"
        )

  print("Fraction of points in each cluster:")
  print(table(labL)/length(labL))
  invisible(outL)
}

#' Generate cluster parameter plots
#'
#' @param modMeans means from a model (clusters in columns)
#' @param ccol colors for features
#'
#' @return heatmap and line plot of cluster means
#' @examples
#' dat <- iris[, -5]
#' out <- p.bic(dat)
#' truth <- iris[, 5]
#' tryCatch(md1 <- p.mclust(out$dat, out$bicO, truth = truth))
#' modMeans <- md1$parameters$mean 
#' p <- p.clusterMeans(modMeans)
#' grid.arrange(p[[1]], p[[2]], ncol = 2)
#' @export 
### Model Parameter Plots
p.clusterMeans <- function(modMeans, ccol = "black") {
  means <- modMeans
  colnames(means) <- paste0("C", 1:ncol(means))
  d1 <- melt(means)

  g1 <- ggplot(d1, aes(x = Var1, y = Var2, fill = value)) + 
    geom_raster() + 
    coord_flip() + 
    theme(axis.title = element_blank(), 
          axis.text.y = element_text(color = ccol))

  g2 <- ggplot(d1, aes(x = Var1, y = value, group = Var2, color = as.factor(Var2))) + 
    geom_line(lwd = 1) + 
    coord_flip() + 
    scale_color_discrete(name = "Cluster") + 
    theme(axis.title = element_blank(), 
          axis.text.y = element_text(color = ccol))
          

  out <- list(pheat = g1, pline = g2)
  invisible(out)
} ### END p.clusterMeans

#' Generate cluster covariance plots
#'
#' @param modSigma mclust output
#' @param ccol feature colors
#'
#' @return heatmap and line plot of cluster means
#' @examples
#' dat <- iris[, -5]
#' out <- p.bic(dat)
#' truth <- iris[, 5]
#' tryCatch(md1 <- p.mclust(out$dat, out$bicO, truth = truth))
#' modSigma <- md1$parameters$variance$sigma
#' p.clusterCov(modSigma)
#' tryCatch(L <- p.hmclust(datLog, truth = truth))
#' p.clusterCov(modSigma)
#' 
#' @export 
### Cluster Covariance Plots
p.clusterCov <- function(modSigma, ccol = "black") {
  ccov <- modSigma
  
  m1 <- melt(ccov)
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
} ### END p.clusterMeans


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

  if(!is.null(clusterLab)){
    ggCol <- brewer.pal(min(length(unique(clusterLab)),9),"Set1")
  } else {
    clusterLab <- rep(1, dim(dat)[1])
  }

  cLab <- factor(clusterLab, ordered = FALSE) 
  dat <- as.data.frame(dat)
  
  gdat <- data.frame(stack(dat), cLab)

  gg.jitter <- 
    ggplot(data = gdat, aes(x = ind, y = values, color = cLab)) +
    scale_color_discrete(name="Cluster") + 
    geom_point(alpha=0.25, position=position_jitterdodge()) +
    geom_boxplot(alpha =0.35, outlier.color = NULL) +
    xlab("") + ylab("")

  return(gg.jitter)
}

#' Generate singular vector plots
#'
#' @param dat data 
#' @param ccol feature colors 
#' @param maxd maximum dimensions to plot
#'
#' @return pairs plot and heatmap of right singular vectors
#'
#' @importFrom gplots colorpanel
#' @importFrom data.table melt
#' @importFrom lattice splom
#'
#' @examples
#' dat <- iris[, -5]
#' p1 <- p.rsv(dat, ccol = c("black", "black", "blue", "blue"))
#' ## grid.arrange(p1[[1]], p1[[2]], nrow = 2)
#' p1[[1]]
#' p1[[2]]
#' @export 
### Spectral plots, singularvectors 
p.rsv <- function(dat, ccol = "black", maxd = Inf) {
  d <- dim(dat)[2]
  dm <- as.matrix(dat) 
  covM <- cov(dm)

  dmax <- ifelse(d > maxd, maxd, d)

  v <- svd(covM, nv = dmax)$v

  colnames(v) <- sprintf(paste0("rsv%0",nchar(ncol(v)), "d"), 1:ncol(v))
  rownames(v) <- colnames(dat)
  mv <- melt(v)
  
  mv$Var2 <- ordered(mv$Var2, levels = rev(levels(mv$Var2)))

  mycol <- colorpanel(255, "darkred", "gray95", "darkblue")

  sc <- scale_fill_gradientn(colours = mycol)

  g1 <- 
    ggplot(mv, aes(x = Var1, y = Var2, fill = value)) + 
    geom_raster() + 
    sc + xlab(NULL) + ylab("Right Singular Vectors") + 
    theme(panel.background = element_rect(fill = "gray75")) +
    theme(panel.grid = element_blank()) +
    theme(axis.text.x = element_text(color = ccol, angle = 90))

  #vP <- if(dim(v)[2] > 8){
  #  v[, 1:8]
  #} else {
  #  v
  #}

  g2 <- 
    splom(v,
     as.matrix = TRUE, 
     panel=panel.xyplot,
     diag.panel = function(x, ...){
     yrng <- current.panel.limits()$ylim
     d <- density(x, na.rm=TRUE)
     d$y <- with(d, yrng[1] + 0.95 * diff(yrng) * y / max(y) )
     panel.lines(d)
     diag.panel.splom(x, ...)
     },
     pch = 19, col = ccol, 
     pscale=0, varname.cex=0.7
     )

  lout <- list(g1 = g1, g2 = g2)
  return(lout)
} ## END p.eig

#' Generate 3D pca of correlation matrix
#'
#' @param dat data 
#' @param colCol colors for the columns of the data matrix
#'
#' @return a 3d scatter plot of first three PCs.
#'
#' @importFrom rgl plot3d
#' @importFrom rgl rgl.texts
#' @importFrom rgl currentSubscene3d
#' @importFrom rglwidget rglwidget
#'
#' @examples
#' dat <- iris[, -5]
#' truth <- iris[, 5]
#' colCol <- c("red", "green", "blue", "purple")
#' p.3dpca(dat, colCol)
#' @export 
### 3D pca
p.3dpca <- function(dat, colCol = NULL) {

  dat <- as.matrix(dat)
  cor <- cor(dat)
  if(is.null(colCol)){ colCol <- "darkblue" }

  W <- svd(cor, nv = 3, nu = 0)$v
  pca <- cor %*% W

  n <- dim(pca)[1]

  size <- max(min(1.5/log10(n), 1.25), 0.05)
  
  rgl::plot3d(pca[,1],pca[,2],pca[,3],type='s', 
              col = colCol, xlab = "PC1", 
              size = size,
              ylab = "PC2", zlab = "PC3")

  rgl::rgl.texts(pca[,1],pca[,2],pca[,3],
                 col = colCol, cex = size,
                 abbreviate(rownames(pca)),
                 adj=c(0,2))

  subid <- currentSubscene3d()
  rglwidget(elementId="rgl-pca0",width=720,height=720)
  
} ## END p.3dpca

