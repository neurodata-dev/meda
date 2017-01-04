#! /usr/local/bin/Rscript
require(meda)
load('~/neurodata/synaptome-stats/Code/cleanDataWithAttributes.RData')

outfile0 <- "FisherIrisData.html"
outfile1 <- "SynDataRaw.html"
outfile2 <- "SynDataLog.html"
outfile3 <- "SynData01e3.html"

#set.seed(2^13)
set.seed(1234)
dat <- data01[sample(nrow(data01), 1e3), 1:24, with = FALSE]

#transformData <- function(x, type = c("1e3"), t98 = FALSE, ...) {
datRaw <- dat
datLog <- transformData(dat, type = c("log10"))$log10
dat01e3 <- transformData(dat, type = c("1e3"))$d01e3


genHTML(iris[,-5], outfile0, truth = iris[,5])
genHTML(datRaw, outfile1)
#system(paste("open", outfile1))
genHTML(datLog, outfile2)
#system(paste("open", outfile2))
genHTML(dat01e3, outfile3)
#system(paste("open", outfile3))
genHTML(dat01e3, outfile3)

