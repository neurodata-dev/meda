#!/usr/bin/env bash
if [ "${1}" == "plot" ]
then
  mkdir -p /data/${3}
  ls -R /data
  Rscript /home/meda/genPlots.R ${2} ${3}
else 
  mkdir -p /data/${2}
  ls -R /data
  Rscript /home/meda/genData.R /data/${1} /data/${2}/
fi

