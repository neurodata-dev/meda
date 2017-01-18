#! /usr/local/bin/Rscript
require(devtools)
devtools::document('~/neurodata/meda/R')
devtools::build("~/neurodata/meda/")
devtools::check("~/neurodata/meda/")
devtools::install("~/neurodata/meda/", dependancies = FALSE)

require(meda)
require(rhdf5)

outfile0 <- "~/neurodata/GH-pages/meda-gh/examples/FisherIrisData.html"
colCol <- c("red", "green", "blue", "red")
genHTML(iris[,-5], outfile = outfile0, truth = iris[, 5], colCol = colCol)
system(paste("open", outfile0))

rm(list = ls())

load('~/neurodata/synaptome-stats/Code/cleanDataWithAttributes.RData')

set.seed(1234)
dat <- data01[sample(nrow(data01), 1e3), 1:24, with = FALSE]

ccol3 <- c("#197300", "#197300", "#197300", "#197300", "#197300", "#197300", 
  "#197300", "#197300", "#197300", "#197300", "#197300", "#cc0000", 
  "#cc0000", "#cc0000", "#cc0000", "#cc0000", "#cc0000", "#0000cd", 
  "#0000cd", "#0000cd", "#0000cd", "#0000cd", "#0000cd", "#0000cd"
  )
#transformData <- function(x, type = c("1e3"), t98 = FALSE, ...) {
datRaw <- dat
datLog <- transformData(dat, type = c("log10"))$log10
dat01e3 <- transformData(dat, type = c("1e3"))$d01e3

outfile1 <- "~/neurodata/GH-pages/meda-gh/examples/SynDataRaw.html"
outfile2 <- "~/neurodata/GH-pages/meda-gh/examples/SynDataLog10.html"
outfile3 <- "~/neurodata/GH-pages/meda-gh/examples/SynData01e3.html"

genHTML(datRaw, outfile1, colCol = ccol3)
system(paste("open", outfile1))
genHTML(datLog, outfile2, colCol = ccol3)
system(paste("open", outfile2))
genHTML(dat01e3, outfile3, colCol = ccol3)
system(paste("open", outfile3))

rm(list = ls())

rhdf5::h5ls("~/neurodata/synaptome-stats/Code/Notebooks/Ex10R55_F0.h5")
datH5 <- rhdf5::h5read("~/neurodata/synaptome-stats/Code/Notebooks/Ex10R55_F0.h5",
                       name = "F0")
datEX <- t(datH5[,,1])

namesEx <- read.csv("~/neurodata/synaptome-stats/Code/Python/chan.csv", head=FALSE, stringsAsFactors = FALSE)[,1]

set.seed(317)
datRawEx <- as.data.frame(datEX[sample(nrow(datEX), 1e3), ])
names(datRawEx) <- as.vector(namesEx)

d <- c(3,26,1,25, 27:29)
d <- c(d,setdiff(1:29, d))
dat <- datRawEx[,d]

ccolEx <- 
  c("#197300", "#197300", "#197300", "#cc0000", "#cc0000", "#cc0000", 
    "#cc0000", "#197300", "#0000cd", "#197300", "#197300", "#197300", 
    "#0000cd", "#0000cd", "#197300", "#cc0000", "#cc0000", "#cc0000", 
    "#197300", "#0000cd", "#197300", "#0000cd", "#0000cd", "#0000cd", 
    "#0000cd", "#0000cd", "#0000cd", "#0000cd", "#0000cd")

fac <- factor(ccolEx, levels = c("#197300", "#cc0000", "#0000cd"), 
       ordered = TRUE)

dat <- dat[, order(fac)]
ccolEx <- ccolEx[order(fac)]

datRaw <- dat
datLog <- transformData(dat, type = c("log10"))$log10
dat01e3 <- transformData(dat, type = c("1e3"))$d01e3



outfile4 <- "~/neurodata/GH-pages/meda-gh/examples/Ex10R55_Raw.html"
outfile5 <- "~/neurodata/GH-pages/meda-gh/examples/Ex10R55_Log10.html"
outfile6 <- "~/neurodata/GH-pages/meda-gh/examples/Ex10R55_01e3.html"

genHTML(datRaw, outfile4, colCol = ccolEx)
system(paste("open", outfile4))
genHTML(datLog, outfile5, colCol = ccolEx)
system(paste("open", outfile5))
genHTML(dat01e3, outfile6, colCol = ccolEx)
system(paste("open", outfile6))

