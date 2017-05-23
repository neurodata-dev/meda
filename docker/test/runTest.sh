#!/usr/bin/env bash
mkdir inputs
mkdir outputs
Rscript mkdat.R
docker run -v ~/neurodata/meda/docker/test:/data -t neurodata/meda:v0.1.2.9009 /data/inputs/iris.csv /data/outputs/
docker run -v ~/neurodata/meda/docker/test:/data -t neurodata/meda:v0.1.2.9009 plot /data/outputs/ /data/outputs/plots/
