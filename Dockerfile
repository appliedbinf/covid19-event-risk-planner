FROM rocker/shiny:4.1.2

MAINTAINER Aroon Chande "achande@ihrc.com, mail@aroonchande.com"

ENV LANG=en_US.UTF-8
ENV TZ=America/New_York

RUN apt-get update && apt-get install -y --no-install-recommends \
    sudo \
    cron \
    pandoc \
    pandoc-citeproc \
    git \
    wget \
    curl \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    gdebi-core \
    libxml2-dev \
    && apt clean \
    &&  locale-gen en_US.utf8 \
    && /usr/sbin/update-locale LANG=en_US.UTF-8 &&\
    ln -snf /usr/share/zoneinfo/$TZ /etc/localtime &&\
    echo $TZ > /etc/timezone\
    && mkdir /root/.ssh


RUN install2.r -n 4 withr ggrepel matlab lubridate \
    dplyr ggthemes leaflet mapview shinythemes \
    devtools ggthemes ggpubr leaflet.extras \
    RCurl rtweet tidyverse vroom RMySQL remotes

run R -e "remotes::install_github('ar0ch/sever', 'ar0ch/shinypanels', 'dreamRs/shinyWidgets', 'andrewsali/shinycssloaders')"

COPY bin/phantomjs /usr/bin/
COPY Rprofile.site /usr/lib/R/etc/
COPY .rtweet_token.rds /root/.rtweet_token.rds


COPY docker_github /root/.ssh/id_rsa
COPY docker_github.pub /root/.ssh/id_rsa.pub
RUN ssh-keyscan -H github.com >> /root/.ssh/known_hosts \
  && git config --global user.email "c19r@atc.io" \
  && git config --global user.name "c19r-bot" \
  && git clone git@github.com:appliedbinf/covid19-event-risk-planner.git /root/repo


COPY COVID19-Event-Risk-Planner /srv/shiny-server
COPY Renviron /home/shiny/.Renviron

EXPOSE 3838

