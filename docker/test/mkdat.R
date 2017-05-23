#!/usr/bin/env Rscript
write.csv(iris[,-5], file = "inputs/iris.csv", row.names = FALSE)
