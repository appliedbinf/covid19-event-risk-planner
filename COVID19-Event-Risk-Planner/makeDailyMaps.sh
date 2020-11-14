#!/bin/bash
fname=$(date +%Y%m%d_%H%M%S)
base="/srv/shiny-server/"
mkdir -p ${base}/daily_risk_map/${fname}
cd ${base}/
Rscript ${base}/makeDailyMaps.R $fname $1
Rscript ${base}/makeDailyMapsEU.R $fname
cd /root/repo/
git pull
cp /${base}/www/usa_risk_counties.csv usa_risk_counties.csv
git add usa_risk_counties.csv
git commit -m "Data update: $fname"
git push
