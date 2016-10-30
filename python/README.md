# TimeSeries MEDA with Python

### What does this do?
Given a new set of T time samples from a d-dimensional time-series, follow [this](https://github.com/neurodata/checklists/blob/master/time-series.md)

Currently unimplemented: change-point detection, power-spectra heatmap

### Instructions
For a quick demo using stock prices, run `python meda.py`

To run on your own data, put it in a [pandas dataframe](http://pandas.pydata.org/pandas-docs/stable/generated/pandas.DataFrame.html) and [pickle it](https://wiki.python.org/moin/UsingPickle). Then call `python meda.py your-file-name.pkl`
