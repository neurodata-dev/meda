# Commencing 
FROM ubuntu:16.04
MAINTAINER Jesse Patsolic <studiojlp@gmail.com>

RUN apt-get update && apt-get install -y openssh-server 

RUN apt-get update
RUN apt-get install -y git libcurl4-openssl-dev libxml2-dev

RUN apt-get update
RUN apt-get install -y libssl-dev libssh2-1-dev 

####Install and compile R
RUN apt-get -y install r-base \
        r-base-dev

#### Install needed R libraries
RUN R -e "install.packages('devtools', repos = 'http://cran.rstudio.com/', dependencies = TRUE)"
RUN R -e "require(devtools); devtools::install_github('neurodata/meda', ref = 'v0.1.2.7003', dependencies = TRUE)"
RUN R -e "source('https://bioconductor.org/biocLite.R'); biocLite('rhdf5')"

#### Add user
RUN useradd -ms /bin/bash meda 
RUN chown meda:meda /home/meda & addgroup meda staff

COPY ./launcher.sh /launcher.sh
COPY ./genData.R /home/meda/genData.R
RUN chmod +x /launcher.sh & chmod +x /home/meda/genData.R
RUN mkdir /data
RUN chown meda:meda -R /data
USER meda

#ENTRYPOINT ["Rscript", "/home/meda/genData.R"]
ENTRYPOINT ["/launcher.sh"]

