#!/bin/bash
base="/srv/shiny-server/"
fname=$(date +%Y%m%d_%H%M%S)
wget https://covidtracking.com/api/v1/states/daily.csv  -O "${base}/states_daily/${fname}.csv" -a "${base}/daily.log";
[ ! -s "${base}/states_daily/${fname}.csv" ] && rm -f "${base}/states_daily/${fname}.csv"

