#!/bin/bash
base="/projects/covid19/COVID19-Event-Risk-Planner-dev"
fname=$(date +%Y%m%d_%H%M%S)
wget https://covidtracking.com/api/v1/states/daily.csv  -O "${base}/states_daily/${fname}.csv" -a "${base}/daily.log";
[ ! -s "${base}/states_daily/${fname}.csv" ] && rm -f "${base}/states_daily/${fname}.csv"

