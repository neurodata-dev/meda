#!/usr/bin/env Rscript
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
suppressPackageStartupMessages(require(meda))
suppressPackageStartupMessages(require(rhdf5))
suppressPackageStartupMessages(require(heatmaply))
suppressPackageStartupMessages(require(htmlwidgets))

genData <- function(dat, ccol, outdir, basedir){

  outL <- list()
  print("Running mlocation")
  outL[[1]] <- mlocationDat <- mlocation(dat, ccol = ccol)
  saveRDS(mlocationDat, file = paste0(outdir, "mlocation.rds"))
  
  print("Running d1heat")
  outL[[2]] <- d1heatDat <- d1heat(dat, ccol = ccol)
  saveRDS(d1heatDat, file = paste0(outdir, "d1heat.rds"))
  
  print("Running cumvar")
  outL[[3]] <- cumvarDat <- cumvar(dat)
  saveRDS(cumvarDat, file = paste0(outdir, "cumvar.rds"))
  
  print("Running outliers")
  outL[[4]] <- outliersDat <- outliers(dat)
  saveRDS(outliersDat, file = paste0(outdir, "outliers.rds"))
  
  print("Running pairHex")
  outL[[5]] <- pairHexDat <- invisible(pairhex(dat, maxd = 6))
  saveRDS(pairHexDat, file = paste0(outdir, "pairhexDat.rds"))

  print("Running correlation")
  outL[[6]] <- corDat <- medacor(dat, ccol = ccol)
  saveRDS(corDat, file = paste0(outdir, "medacor.rds"))

  print("Running hmc")
  outL[[7]] <- hmcDat <- hmc(scale(dat, center = TRUE, scale = FALSE), 
                             maxDepth = 6, modelNames = "EEE", ccol = ccol)
  saveRDS(hmcDat, file = paste0(outdir, "hmc.rds"))
  
  print("Running Heatmap")
  #setwd(outdir)
  h <- heatmaply(dat, file = "heatmap.html", selfcontained = FALSE)
  #setwd(basedir)

  save(h, file = paste0(outdir, "heatmap.RData"))
  #h %>% saveWidget(file = heatout, selfcontained = FALSE)
  #setwd(basedir)
  
  print("Done")
}

infile <- args[1]
outdir <- args[2]

basedir <- getwd()

dat <- data.table(h5read(infile, name="data/d1"))
ccol <- h5read(infile, "colors/ccol")
H5close()

#system.time(
  genData(dat, ccol, outdir, basedir)
#)

#   Time:
##  Working status:
### Comments:
####Soli Deo Gloria
