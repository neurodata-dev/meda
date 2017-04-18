#!/usr/bin/env Rscript
###
### Rscript to convert and format data for use in meda
### HowToRun:
### Rscript convertData.R <data.csv> [columnColors.csv]
###
### data.csv: subject in rows, features in columns.
### columnColors.csv: 1 column, with header row, of colors for data
###   features.
###
### Jesse Leigh Patsolic 
### 2017 <jpatsolic@jhu.edu>
### S.D.G 
#
args <- commandArgs(trailingOnly = TRUE)
suppressPackageStartupMessages(require(rhdf5))
suppressPackageStartupMessages(require(data.table))

### get command line arguments
l <- length(args)
infile <- args[1]

### if no color file is given default to black
incolors <- ifelse(l >= 2, args[2], "black")

dat <- fread(infile, showProgress = FALSE)

ccol <- if(l >= 2){
  as.character(read.csv(incolors, header = TRUE)[,1])
  } else {
    rep("black", nrow(dat))
  }

F0 <- dat
base <- infile
fname <- paste0(base,".h5") 
clean <- h5createFile(fname)
clean <- h5createGroup(fname, "data")
clean <- h5createGroup(fname, "colors")

h5write(F0,fname, "data/d1")
h5write(ccol, fname, "colors/ccol")
h5ls(fname)

H5close()

#   Time: 30 minutes
##  Working status: Works for the appriate inputs
### Comments: Not meant to be a fix all for any situation. Make sure
### your data are clean. 
####Soli Deo Gloria
