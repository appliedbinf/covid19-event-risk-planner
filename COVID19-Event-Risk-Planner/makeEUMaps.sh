#!/bin/bash
set -eou
fname=$(date +%Y%m%d_%H%M%S)
base="/srv/shiny-server/"
mkdir -p ${base}/{daily_risk_map_italy,daily_risk_map_swiss,daily_risk_map_uk}/${fname}
cd ${base}/
Rscript ${base}/makeDailyMapsSwiss.R $fname
Rscript ${base}/makeDailyMapsUK.R $fname
Rscript ${base}/makeDailyMapsItaly.R $fname
