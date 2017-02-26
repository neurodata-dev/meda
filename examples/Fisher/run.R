#! /usr/local/bin/Rscript
require(devtools)
devtools::document('~/neurodata/meda/R')
devtools::build("~/neurodata/meda/")
devtools::check("~/neurodata/meda/")
devtools::install("~/neurodata/meda/", dependancies = FALSE)

require(meda)

outdir0 <- "./"
outfile0 <- "FisherIrisData.html"
colCol <- c("red", "green", "blue", "red")

dat <- iris[, -5]

genHTML(dat, outfile = outfile0, outdir = outdir0, truth = iris[, 5], colCol = colCol, center = TRUE, modelNames = c("VVV"))
system(paste("open", outfile0))




