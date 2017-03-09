#! /usr/local/bin/Rscript
require(meda)
load('~/neurodata/synaptome-stats/Code/cleanDataWithAttributes.RData')

set.seed(317)
dat <- data01[sample(nrow(data01), 1e4), 1:24, with = FALSE]

colnames(dat) <- gsub("_F0", "", colnames(dat))

ccol3 <- c("#197300", "#197300", "#197300", "#197300", "#197300", "#197300", 
  "#197300", "#197300", "#197300", "#197300", "#197300", "#cc0000", 
  "#cc0000", "#cc0000", "#cc0000", "#cc0000", "#cc0000", "#0000cd", 
  "#0000cd", "#0000cd", "#0000cd", "#0000cd", "#0000cd", "#0000cd"
  )
#transformData <- function(x, type = c("1e3"), t98 = FALSE, ...) {
datRaw <- dat
datLog <- transformData(dat, type = c("log10"))$log10
dat01e3 <- transformData(dat, type = c("1e3"))$d01e3

home <- "~/neurodata/GH-pages/meda-gh/examples/K15_1e4/"
outdir1 <- "~/neurodata/GH-pages/meda-gh/examples/K15_1e4/Raw/"
outdir2 <- "~/neurodata/GH-pages/meda-gh/examples/K15_1e4/Log10/"
outdir3 <- "~/neurodata/GH-pages/meda-gh/examples/K15_1e4/01e3/"

outfile1 <- "~/neurodata/GH-pages/meda-gh/examples/K15_1e4/Raw/SynDataRaw.html"
outfile2 <- "~/neurodata/GH-pages/meda-gh/examples/K15_1e4/Log10/SynDataLog10.html"
outfile3 <- "~/neurodata/GH-pages/meda-gh/examples/K15_1e4/01e3/SynData01e3.html"

setwd(outdir3)
tryCatch(genHTML(dat01e3, outfile3, outdir = './', colCol = ccol3, center = TRUE, modelNames = c("VVV")))
tryCatch(system(paste("open", outfile3)))

setwd(outdir1)
tryCatch(genHTML(datRaw, outfile1, outdir ='./', colCol = ccol3, center = TRUE, modelNames = c("VVV")))
tryCatch(system(paste("open", outfile1)))


setwd(outdir2)
tryCatch(genHTML(datLog, outfile2, outdir = './',colCol = ccol3, center = TRUE, modelNames = c("VVV")))
tryCatch(system(paste("open", outfile2)))


