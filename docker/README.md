### Code
The source code for this project lives [here](https://github.com/neurodata/meda) on GitHub. 

### Pulling the image

`docker pull neurodata/meda`

### Running

The following are expected to be true:

- You have a data matrix/spreadsheet where rows correspond to
observation/subject and columns correspond to features and a separate
csv file with one column of feature colors for plotting.
- you have run [MrAE's code](https://gist.github.com/MrAE/57a4a5a95ad89413d492e5c62c5db497) on your data generating an HDF5 file which you plan to process.
- the resulting HDF5 file is assumed to be stored as `/mydata/inputs/myfile.h5`
- you want derivatives to be stored in `/mydata/outputs/`
- you want plots to be stored in `/mydata/plots/`

You may then run:

`docker run -v /mydata/:/data/ -t neurodata/meda inputs/myfile.h5 outputs/`

Then to plot the results:

`docker run -v /mydata/:/data/ -t neurodata/meda plot data/outputs/ /data/outputs/plots/`

[Issues, bugs, feature requests?](https://github.com/neurodata/meda/issues)

