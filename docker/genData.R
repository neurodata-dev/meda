#! /usr/local/bin/Rscript
###
### Rscript to run data for meda
### HowToRun:
### Rscript genData.R <infile.h5> <outdir>
###
### Jesse Leigh Patsolic 
### 2017 <jpatsolic@jhu.edu>
### S.D.G 
#
args <- commandArgs(trailingOnly = TRUE)
require(meda)
require(rhdf5)

infile <- args[1]
outdir <- args[2]

outfile <- paste0(outdir, "/meda.RData")

dat <- data.table(h5read(infile, "data/d1"))
ccol <- h5read(infile, "colors/ccol")
H5close()

outL <- list()
outL[[1]] <- mlocation(dat, ccol = ccol)
outL[[2]] <- d1heat(dat, ccol = ccol)
outL[[3]] <- pairhex(dat, colramp = magent, maxd = 8)
outL[[4]] <- cumvar(dat)
outL[[5]] <- outliers(dat)
outL[[6]] <- hmc(scale(dat, center = TRUE, scale = FALSE))
outL[[7]] <- stackM(outL[[6]], centered = TRUE, ccol = ccol)
outL[[8]] <- clusterMeans(outL[[6]], ccol = ccol)


save(outL, file = outfile)


#   Time:
##  Working status:
### Comments:
####Soli Deo Gloria
