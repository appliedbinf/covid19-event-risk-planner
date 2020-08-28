FROM openanalytics/r-base as aroon/c19r-base

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
COPY bin/phantomjs /usr/bin/
# copy the app to the image
COPY Rprofile.site /usr/lib/R/etc/


FROM aroon/c19r-base:latest as c19r-swarm-dev

RUN wget https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-1.5.14.948-amd64.deb && \
    yes | sudo gdebi shiny-server-1.5.14.948-amd64.deb

RUN sudo echo "1 17 * * * /srv/shiny-server/makeDailyMaps.sh 1 \n\
1 12 * * * /srv/shiny-server/makeDailyMaps.sh 0 \n\
1 12,20 * * * /srv/shiny-server/makeDailyPlots.sh \n\
1 * * * * perl -le 'sleep rand 700' && /srv/shiny-server/update_current.sh \n\
1 */4 * * * perl -le 'sleep rand 700' && /srv/shiny-server/update_daily.sh \n\
" > /etc/crontabs/root

COPY COVID19-Event-Risk-Planner /srv/shiny-server

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]