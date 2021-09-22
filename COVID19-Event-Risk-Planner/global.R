library(shiny)
library(withr)
library(ggplot2)
library(ggrepel)
library(matlab)
library(lubridate)
library(dplyr)
library(ggthemes)
library(leaflet)
library(mapview)
library(sever)
library(DBI)
library(dbplyr)
library(DT)
# library(shinycookie)
options(shiny.reactlog = TRUE)
# Master username
# admin
# Master password
# taofrhsXnl031nko7pB6
# Endpoint
# weitz-c19r.cluster-cnoy4sgleu5u.us-east-1.rds.amazonaws.com

db <- dbConnect(
  drv      = RMySQL::MySQL(),
  username = "admin",
  password = "taofrhsXnl031nko7pB6",
  host     = "weitz-c19r.cluster-cnoy4sgleu5u.us-east-1.rds.amazonaws.com",
  port     = 3306,
  dbname   = "c19r"
)

county_geom <- sf::st_read("map_data/geomUnitedStates.geojson")
stateline <- sf::st_read("map_data/US_stateLines.geojson")[,c('STUSPS','NAME', 'geometry')]
names(stateline) <- c('stname','name', 'geometry')

addResourcePath("www", "www")

source('./leaflet_inplace.R', local=T)
