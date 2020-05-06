#!/bin/bash
set -x
fname=$(date +%Y%m%d_%H%M%S) 
wget https://covidtracking.com/api/v1/states/current.csv -O /projects/covid19/COVID19-Event-Risk-Planner/states_current/${fname}.csv -a /projects/covid19/COVID19-Event-Risk-Planner/current.log; 
[ ! -s "/projects/covid19/COVID19-Event-Risk-Planner/states_current/${fname}.csv" ] && rm -f "/projects/covid19/COVID19-Event-Risk-Planner/states_current/${fname}.csv"
