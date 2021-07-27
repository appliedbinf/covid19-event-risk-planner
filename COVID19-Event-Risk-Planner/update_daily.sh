#!/bin/bash
base="/srv/shiny-server/"
fname=$(date +%Y%m%d_%H%M%S)
wget https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv  -O "${base}/states_daily/${fname}.csv" -a "${base}/daily.log";
[ ! -s "${base}/states_daily/${fname}.csv" ] && rm -f "${base}/states_daily/${fname}.csv"

