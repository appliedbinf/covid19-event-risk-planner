#!/bin/bash
fname=$(date +%Y%m%d_%H%M%S)
base="/projects/covid19/COVID19-Event-Risk-Planner"

cd ${base}/
Rscript ${base}/makeDailyMaps.R $fname
