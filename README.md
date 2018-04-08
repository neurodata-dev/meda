# README

meda: Matrix Exploratory Data Analysis is an R package for exploring
data in the form of matrices.

for the python version, go here https://github.com/neurodata-nomads/pymeda


### INSTALLATION

Currently there seems to be a problem installing in RStudio.
The best way seems to be installing from R via command line. 

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
```

# Example using Fisher's Iris data

First we center the feature columns and then we proceed with exploring
the data. 

```R
dato <- iris[, -5]
dat <- data.frame(scale(dato, center = TRUE, scale = FALSE))
truth <- iris[, 5]

## optional column classes or feature classes
ccol <- rep(c("darkgreen", "purple"), 2)

plot(mlocation(dato, ccol = ccol))
plot(d1heat(dat, ccol = ccol))
plot(outliers(dat))
plot(medacor(dat, ccol = ccol))
plot(cumvar(dat))
pairhex(dat)

h <- hmc(dat, ccol = ccol)
plot(h, pch = truth)
plotDend(h)
stackM(h, centered = TRUE, ccol = ccol, depth = h$dat$height)
stackM(h, centered = TRUE, ccol = ccol)
clusterMeans(h)
```



----------------

### Given a new set of n samples in R^d

follow instructions [here](https://github.com/neurodata/checklists/blob/master/matrix.md)



