### building

From **this** directory, run the following:

`docker build -t neurodata/meda:latest .`

### running

We expect the following to be true:

- you have run MrAE's code generating an HDF5 file which you plan to process
- said HDF5 file is assumed to be stored as `/mydata/inputs/myfile.h5`
- you want derivatives to be stored in `/mydata/outputs/`
- you want plots to be stored in `/mydata/plots/`


You may then run:

`docker run -v /mydata/:/data/ -t neurodata/meda data/inputs/myfile.h5 /data/outputs/`

Then to plot the results:

`docker run -v /mydata/:/data/ -t neurodata/meda plot data/outputs/ /data/outputs/plots/`

To run synaptograms on the clustering results:

`docker run -v /mydata/:/data/ -t neurodata/meda synaptograms locationFile.rbin /data/outputs/plots/`

