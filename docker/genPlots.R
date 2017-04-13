#!/usr/bin/env Rscript
###
### Rscript to run data for meda
### HowToRun:
### Rscript genPlots.R 
###
### Jesse Leigh Patsolic 
### 2017 <jpatsolic@jhu.edu>
### S.D.G 
#
args <- commandArgs(trailingOnly = TRUE)
suppressPackageStartupMessages(require(meda))
suppressPackageStartupMessages(require(htmlwidgets))

indir <- args[1]
outdir <- args[2]

files <- grep(".rds$", dir(indir, full.names = TRUE), value = TRUE)

L <- list()

for(f in 1:length(files)){
  L[[f]] <- readRDS(files[f])
}

for(i in 1:length(L)){
  png(paste0(outdir, i, ".png"), height = 720, width = 720)
  print(plot(L[[i]]))
  dev.off()
}

if(any(grepl("hmc.rds", files))){
  ind <- which(grepl("hmc.rds", files))
  hmcDat <- L[grepl("hmc.rds", files)][[1]]

  png(paste0(outdir, "dend.png"), height = 720, width = 720)
  plotDend(hmcDat)
  dev.off()

  h <-  8 +ifelse(dim(hmcDat$dat$sigma$dat)[3] %% 2==0,
                  dim(hmcDat$dat$sigma$dat)[3]- 2,dim(hmcDat$dat$sigma$dat)[3])

  png(paste0(outdir, "corr.png"), height = h, width = 8, units = "in", res = 72)
  plot(hmcDat$dat$sigma, ccol = L[[ind]]$ccol)
  dev.off()

  png(paste0(outdir, "clusterMeans.png"), height = 8, width = 18, units = "in", res = 300)
  show(clusterMeans(hmcDat, ccol = L[[ind]]$ccol))
  dev.off()

  png(paste0(outdir, "stackM.png"), height = 8, width = 8, units = "in", res = 300)
  show(stackM(hmcDat, centered = TRUE, ccol = L[[ind]]$ccol))
  dev.off()
}

#   Time:
##  Working status:
### Comments:
####Soli Deo Gloria
