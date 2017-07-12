#!/bin/bash
if [ "${1}" == "plot" ]
then
  mkdir -p ${3}
  ls -R /data
  Rscript /home/meda/genPlots.R ${2} ${3}
elif [ "${1}" == "synaptograms" ]
then 
  echo "Synaptograms are under construction"
elif [ "${1}" == "hdf5" ]
then
  echo "HDF5 script under construction"
else 
  mkdir -p ${2}
  #ls -R /data
  Rscript /home/meda/genData.R ${1} ${2}
fi


