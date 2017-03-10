#! /usr/local/bin/Rscript
require(meda)

load(paste0(Sys.getenv("NEURODATA"), "/synaptome-stats/data/WeilerDAPIpca.RData"))

ccolEx <- 
  c("#197300", "#197300", "#197300", "#cc0000", "#cc0000", "#cc0000", 
    "#cc0000", "#197300", "#0000cd", "#197300", "#197300", "#197300", 
    "#0000cd", "#0000cd", "#197300", "#cc0000", "#cc0000", "#cc0000", 
    "#197300", "#0000cd", "#197300", "#0000cd", "#0000cd", "#0000cd", 
    "#0000cd", "#0000cd", "#0000cd", "#0000cd", "#0000cd")
ccolEx <- ccolEx[1:22]

fac <- factor(ccolEx, levels = c("#197300", "#cc0000", "#0000cd"), 
       ordered = TRUE)

dat <- WeilerDAPIpca
dat <- dat[, order(fac), with = FALSE]
ccolEx <- ccolEx[order(fac)]

set.seed(123456)
s <- sample(nrow(WeilerDAPIpca))
s1 <- s[1:10000]

datRaw <- WeilerDAPIpca[s1,]
dat01e3 <- transformData(WeilerDAPIpca, type = c("1e3"))$d01e3[s1,]
datLog10 <- transformData(WeilerDAPIpca, type = c("log10"))$log10[s1,]

outfile1 <- paste0(Sys.getenv("NEURODATA"), "/GH-pages/meda-gh/examples/W_1e4/Explore/01e3/dapi01e3_PCA.html")
outfile2 <- paste0(Sys.getenv("NEURODATA"), "/GH-pages/meda-gh/examples/W_1e4/Explore/Raw/dapiRaw_PCA.html")
outfile3 <- paste0(Sys.getenv("NEURODATA"), "/GH-pages/meda-gh/examples/W_1e4/Explore/Raw/dapiLog10_PCA.html")

outdir1 <- paste0(Sys.getenv("NEURODATA"), "/GH-pages/meda-gh/examples/W_1e4/Explore/01e3/")
outdir2 <- paste0(Sys.getenv("NEURODATA"), "/GH-pages/meda-gh/examples/W_1e4/Explore/Raw/")
outdir3 <- paste0(Sys.getenv("NEURODATA"), "/GH-pages/meda-gh/examples/W_1e4/Explore/Log10/")

setwd(outdir1)
tryCatch(genHTML(dat01e3, outfile1, outdir = './', colCol = ccolEx, center = TRUE, modelNames = c("VVV")))
tryCatch(system(paste("open", outfile1)))

setwd(outdir2)
tryCatch(genHTML(datRaw, outfile2, outdir = './', colCol = ccolEx, center = TRUE, modelNames = c("VVV")))
tryCatch(system(paste("open", outfile2)))

setwd(outdir3)
tryCatch(genHTML(datLog10, outfile3, outdir = './', colCol = ccolEx, center = TRUE, modelNames = c("VVV")))
tryCatch(system(paste("open", outfile3)))



