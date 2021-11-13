#!/bin/bash
set -eou
fname=$(date +%Y%m%d_%H%M%S)
base="/srv/shiny-server/"
mkdir -p ${base}/daily_risk_map_{italy,_swiss,uk,france,austria}/${fname}
cd ${base}/
Rscript ${base}/makeDailyMapsSwiss.R $fname
Rscript ${base}/makeDailyMapsUK.R $fname
Rscript ${base}/makeDailyMapsItaly.R $fname
Rscript ${base}/makeDailyMapsFrance.R $fname
Rscript ${base}/makeDailyMapsAustria.R $fname
Rscript ${base}/makeDailyMapsSweden.R $fname
Rscript ${base}/makeDailyMapsIreland.R $fname
Rscript ${base}/makeDailyMapsDenmark.R $fname
