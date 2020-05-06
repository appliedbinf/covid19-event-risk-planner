#!/bin/bash
fname=$(date +%Y%m%d_%H%M%S)
wget https://covidtracking.com/api/v1/states/daily.csv  -O /projects/covid19/COVID19-Event-Risk-Planner/states_daily/${fname}.csv -a /projects/covid19/COVID19-Event-Risk-Planner/daily.log;

