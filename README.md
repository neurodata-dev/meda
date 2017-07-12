# README

## R
meda: Matrix Exploratory Data Analysis is an R package for exploring
data in the form of matrices.

### INSTALLATION

To download and install run the following in R:

```R
require(devtools)
devtools::install_github("neurodata/meda")
```

## Run on your own data
To run `meda` on your own data do the following.
Supposing your data is in a csv file with a header:

```R
require(meda)
dat <- read.csv("fileName.csv", header=TRUE)

plot(mlocation(dat))
```




----------------

### Given a new set of n samples in R^d

follow instructions [here](https://github.com/neurodata/checklists/blob/master/matrix.md)

## Python 

[README here](python/README.md)


