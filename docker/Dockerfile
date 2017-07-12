# Commencing 
FROM ubuntu:16.04
MAINTAINER Jesse Patsolic <studiojlp@gmail.com>

#### Add user
RUN useradd -ms /bin/bash meda 
RUN chown meda:meda /home/meda & addgroup meda staff

RUN apt-get update && apt-get install -y openssh-server \ 
    git libcurl4-openssl-dev libxml2-dev \
    libssl-dev libssh2-1-dev vim wget tmux

####Install and compile R
RUN apt-get update && apt-get -y install r-base \
        r-base-dev

#### Install package dependencies for meda
RUN R -e "install.packages('devtools', repos = 'http://cran.rstudio.com/', dependencies = TRUE)"

RUN R -e "install.packages(c('heatmaply', 'hexbin',\ 
         'htmlwidgets', 'irlba', 'knitr', 'lattice',\ 
         'mclust', 'plotly', 'randomForest'),\ 
          repos = 'http://cran.rstudio.com/',\
          dependencies = c('Depends', 'Imports'))"

RUN R -e "install.packages(c('RColorBrewer', 'rCUR',\ 
         'rmarkdown', 'jsonlite',\ 
         'base64enc', 'png', 'viridis'),\
          repos = 'http://cran.rstudio.com/',\
          dependencies = TRUE)"

RUN R -e "install.packages(c('abind', 'corrplot', 'data.table',\
          'data.tree', 'dendextend', 'energy', 'foreach',\
          'ggplot2', 'gplots', 'graphics', 'gridExtra'),\
          repos = 'http://cran.rstudio.com/',\
          dependencies = c('Depends', 'Imports'))"

RUN R -e "source('https://bioconductor.org/biocLite.R'); biocLite('rhdf5')"


#### Install meda from github
#RUN R -e "require(devtools);\
#         devtools::install_github('neurodata/meda',\
#         ref = '0a438b0ac24599c2194d6b7caf9426101ccfb23e',\
#         dependencies = FALSE)"

#### Install meda from tarball
COPY ./meda_0.1.2.9013.tar.gz /home/meda/meda_0.1.2.9013.tar.gz
RUN R -e "install.packages('/home/meda/meda_0.1.2.9013.tar.gz',\
           type = 'source', repos = NULL)"

#RUN R CMD INSTALL /home/meda/meda_0.1.2.9013.tar.gz

COPY ./launcher.sh /launcher.sh
COPY ./genData.R /home/meda/genData.R 
COPY ./genPlots.R /home/meda/genPlots.R 
RUN chmod +x /launcher.sh & \
    chmod +x /home/meda/genData.R & \
    chmod +x /home/meda/genPlots.R
RUN mkdir /data
RUN chown meda:meda -R /data
USER meda

ENTRYPOINT ["/launcher.sh"]

