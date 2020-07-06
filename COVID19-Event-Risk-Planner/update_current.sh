#!/bin/bash
#set -x
base="/projects/covid19/COVID19-Event-Risk-Planner-dev"
fname=$(date +%Y%m%d_%H%M%S) 
wget https://covidtracking.com/api/v1/states/current.csv \
	-O "${base}/states_current/${fname}.csv" \
	-a "${base}/current.log"; 
[ ! -s "${base}/states_current/${fname}.csv" ] && rm -f "${base}/states_current/${fname}.csv"
