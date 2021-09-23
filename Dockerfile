FROM openanalytics/r-base

MAINTAINER Aroon Chande "achande@ihrc.com, mail@aroonchande.com"


RUN apt-get update && apt-get upgrade -y

RUN apt-get update && apt-get install -y \
    sudo \
    cron \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.0.0 \
    sqlite3 \
    locales \
    git \
    vim-tiny \
    less \
    wget \
    r-base \
    r-base-dev \
    r-recommended \
    fonts-texgyre \
    texinfo \
    locales \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev

RUN locale-gen en_US.utf8 \
    && /usr/sbin/update-locale LANG=en_US.UTF-8
ENV LANG=en_US.UTF-8

RUN apt-get install python3-pip -y


RUN echo 'options(\n\
  repos = c(CRAN = "https://cloud.r-project.org/"),\n\
  download.file.method = "libcurl",\n\
  # Detect number of physical cores\n\
  Ncpus = parallel::detectCores(logical=FALSE)\n\
)' >> /etc/R/Rprofile.site


# This is necessary for non-root users to follow symlinks to /root/.TinyTeX
RUN chmod 755 /root

RUN apt-get install -y \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev

RUN apt-get update && apt-get install -y \
    libxml2-dev

RUN R -e "install.packages('shiny')"

RUN wget https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-1.5.14.948-amd64.deb && \
    yes | sudo gdebi shiny-server-1.5.14.948-amd64.deb

RUN R -e "install.packages('withr')"
RUN R -e "install.packages('ggplot2')"
RUN R -e "install.packages('ggrepel')"
RUN R -e "install.packages('matlab')"
RUN R -e "install.packages('lubridate')"
RUN R -e "install.packages('dplyr')"
RUN R -e "install.packages('ggthemes')"
RUN R -e "install.packages('leaflet')"
RUN R -e "install.packages('mapview')"
RUN R -e "install.packages('shinythemes')"
RUN R -e "install.packages('devtools')"
RUN R -e 'devtools::install_github("andrewsali/shinycssloaders")'
RUN R -e 'install.packages("ggthemes")'
RUN R -e 'devtools::install_github("dreamRs/shinyWidgets")'
RUN R -e 'install.packages("ggpubr")'
RUN R -e 'install.packages("leaflet.extras")'
RUN R -e 'install.packages("RCurl")'
RUN R -e 'install.packages("rtweet")'
RUN R -e 'install.packages("tidyverse")'
RUN R -e "install.packages('shiny')"
RUN R -e "install.packages('vroom')"
RUN R -e "install.packages('RMySQL')"
COPY bin/phantomjs /usr/bin/
RUN R -e 'remotes::install_github("ar0ch/sever")'
RUN R -e 'remotes::install_github("ar0ch/shinypanels")'
# copy the app to the image
COPY Rprofile.site /usr/lib/R/etc/
COPY .rtweet_token.rds /root/.rtweet_token.rds
COPY Renviron /root/.Renviron


ENV TZ=America/New_York
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone
RUN rm -f /shiny-server-1.5.14.948-amd64.deb
#RUN sudo echo -e "1 17 * * * /srv/shiny-server/makeDailyMaps.sh 1 \n\
#1 12 * * * /srv/shiny-server/makeDailyMaps.sh 0 \n\
#1 10 * * * /srv/shiny-server/makeEUMaps.sh \n\
#1 12,20 * * * /srv/shiny-server/makeDailyPlots.sh \n\
#1 * * * * perl -le 'sleep rand 700' && /srv/shiny-server/update_current.sh \n\
#1 */4 * * * perl -le 'sleep rand 700' && /srv/shiny-server/update_daily.sh \n\
#" > /var/spool/cron/crontabs/root

RUN mkdir /root/.ssh
COPY docker_github /root/.ssh/id_rsa
COPY docker_github.pub /root/.ssh/id_rsa.pub
RUN ssh-keyscan -H github.com >> /root/.ssh/known_hosts
RUN git config --global user.email "c19r@atc.io"
RUN git config --global user.name "c19r-bot"
RUN git clone git@github.com:appliedbinf/covid19-event-risk-planner.git /root/repo
RUN R -e 'devtools::install_github("dreamRs/shinyWidgets")'


COPY COVID19-Event-Risk-Planner /srv/shiny-server
COPY Renviron /home/shiny/.Renviron

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
