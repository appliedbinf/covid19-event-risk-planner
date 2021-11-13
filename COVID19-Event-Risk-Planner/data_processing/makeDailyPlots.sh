#!/bin/bash
fname=$(date +%Y%m%d_%H%M%S)
base="/srv/shiny-server/"
wget https://covidtracking.com/api/v1/states/current.csv -O ${base}/states_current/${fname}.csv -a ${base}/current.log;
wget https://covidtracking.com/api/v1/states/daily.csv  -O  ${base}/states_daily/${fname}.csv -a  ${base}/daily.log;

cd ${base}/
Rscript ${base}/makeDaily.R
