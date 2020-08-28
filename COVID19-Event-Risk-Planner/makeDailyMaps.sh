#!/bin/bash
fname=$(date +%Y%m%d_%H%M%S)
base="/srv/shiny-server/"
mkdir -p ${base}/daily_risk_map/${fname}
cd ${base}/
Rscript ${base}/makeDailyMaps.R $fname $1
