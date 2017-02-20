#! /usr/local/bin/Rscript
require(meda)
require(rhdf5)
rhdf5::h5ls("~/neurodata/synaptome-stats/Code/Notebooks/Ex10R55_F0.h5")
datH5 <- rhdf5::h5read("~/neurodata/synaptome-stats/Code/Notebooks/Ex10R55_F0.h5",
                       name = "F0")
datEX <- t(datH5[,,1])

namesEx <- read.csv("~/neurodata/synaptome-stats/Code/Python/chan.csv", head=FALSE, stringsAsFactors = FALSE)[,1]

set.seed(1234)
datRawEx <- as.data.frame(datEX[sample(nrow(datEX), 1e4), ])
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


outdir4 <- "~/neurodata/GH-pages/meda-gh/examples/W_1e4/Raw/"
outdir5 <- "~/neurodata/GH-pages/meda-gh/examples/W_1e4/Log10/"
outdir6 <- "~/neurodata/GH-pages/meda-gh/examples/W_1e4/01e3/"

outfile4 <- "~/neurodata/GH-pages/meda-gh/examples/W_1e4/Raw/Ex10R55_Raw.html"
outfile5 <- "~/neurodata/GH-pages/meda-gh/examples/W_1e4/Log10/Ex10R55_Log10.html"
outfile6 <- "~/neurodata/GH-pages/meda-gh/examples/W_1e4/01e3/Ex10R55_01e3.html"

#setwd(outdir4)
#tryCatch(genHTML(datRaw, outfile4, outdir = './', colCol = ccolEx, center = TRUE))
#tryCatch(system(paste("open", outfile4)))

setwd(outdir5)
tryCatch(genHTML(datLog, outfile5, outdir = './', colCol = ccolEx, center = TRUE))
tryCatch(system(paste("open", outfile5)))

setwd(outdir6)
tryCatch(genHTML(dat01e3, outfile6, outdir = './', colCol = ccolEx, center = TRUE))
tryCatch(system(paste("open", outfile6)))

