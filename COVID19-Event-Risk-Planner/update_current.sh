#!/bin/bash
#set -x
base="/srv/shiny-server/"
fname=$(date +%Y%m%d_%H%M%S) 
wget https://covidtracking.com/api/v1/states/current.csv \
	-O "${base}/states_current/${fname}.csv" \
	-a "${base}/current.log"; 
[ ! -s "${base}/states_current/${fname}.csv" ] && rm -f "${base}/states_current/${fname}.csv"
