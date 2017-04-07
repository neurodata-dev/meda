# Commencing 
FROM ubuntu:16.04
MAINTAINER Jesse Patsolic <studiojlp+p@gmail.com>

RUN apt-get update && apt-get install -y openssh-server 
RUN mkdir /var/run/sshd
RUN echo 'root:screencast' | chpasswd
RUN sed -i 's/PermitRootLogin without-password/PermitRootLogin yes/' /etc/ssh/sshd_config

# SSH login fix. Otherwise user is kicked off after login
RUN sed 's@session\s*required\s*pam_loginuid.so@session optional pam_loginuid.so@g' -i /etc/pam.d/sshd

RUN apt-get update
RUN apt-get install -y git cmake g++ libcurl4-openssl-dev libxml2-dev

RUN apt-get update
RUN apt-get install -y libssl-dev libssh2-1-dev 

####Install and compile R
#https://www.digitalocean.com/community/tutorials/how-to-set-up-r-on-ubuntu-14-04
RUN apt-get -y install r-base \
        r-base-dev

RUN R -e "install.packages('devtools', repos = 'http://cran.rstudio.com/', dependencies = TRUE)"
RUN R -e "require(devtools); devtools::install_github('neurodata/meda', ref = 'split', dependencies = TRUE)"


ENV NOTVISIBLE "in users profile"
RUN echo "export VISIBLE=now" >> /etc/profile

EXPOSE 22
CMD ["/usr/sbin/sshd", "-D"]
