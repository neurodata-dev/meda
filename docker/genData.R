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
print("Running mlocation")
outL[[1]] <- mlocationDat <- mlocation(dat, ccol = ccol)
print("Saving")
save(mlocationDat, file = paste0(outdir, "mlocation.RData"))

print("Running d1heat")
outL[[2]] <- d1heatDat <- d1heat(dat, ccol = ccol)
print("Saving")
save(d1heatDat, file = paste0(outdir, "d1heat.RData"))

print("Running pairhex")
outL[[3]] <- pairhexDat <- pairhex(dat, colramp = magent, maxd = 8)
print("Saving")
save(pairhexDat, file = paste0(outdir, "pairhex.RData"))

print("Running cumvar")
outL[[4]] <- cumvarDat <- cumvar(dat)
print("Saving")
save(cumvarDat, file = paste0(outdir, "cumvar.RData"))

print("Running outliers")
outL[[5]] <- outliersDat <- outliers(dat)
print("Saving")
save(outliersDat, file = paste0(outdir, "outliers.RData"))

print("Running hmc")
outL[[6]] <- hmcDat <- hmc(scale(dat, center = TRUE, scale = FALSE))
print("Saving")
save(hmcDat, file = paste0(outdir, "hmc.RData"))


print("Saving")
save(outL, file = outfile)


#   Time:
##  Working status:
### Comments:
####Soli Deo Gloria
