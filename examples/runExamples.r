#! /usr/local/bin/Rscript
require(meda)
load('~/neurodata/synaptome-stats/Code/cleanDataWithAttributes.RData')

outfile0 <- "~/neurodata/GH-pages/meda-gh/examples/FisherIrisData.html"
outfile1 <- "~/neurodata/GH-pages/meda-gh/examples/SynDataRaw.html"
outfile2 <- "~/neurodata/GH-pages/meda-gh/examples/SynDataLog.html"
outfile3 <- "~/neurodata/GH-pages/meda-gh/examples/SynData01e3.html"

#set.seed(2^13)
set.seed(1234)
dat <- data01[sample(nrow(data01), 1e3), 1:24, with = FALSE]

#transformData <- function(x, type = c("1e3"), t98 = FALSE, ...) {
datRaw <- dat
datLog <- transformData(dat, type = c("log10"))$log10
dat01e3 <- transformData(dat, type = c("1e3"))$d01e3


genHTML(iris[,-5], outfile = outfile0, truth = iris[, 5])
#system(paste("open", outfile0))
genHTML(datRaw, outfile1)
#system(paste("open", outfile1))
genHTML(datLog, outfile2)
#system(paste("open", outfile2))
genHTML(dat01e3, outfile3)
#system(paste("open", outfile3))
genHTML(dat01e3, outfile3)

