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


db <- dbConnect(
  drv      = RMySQL::MySQL(),
  username = Sys.getenv("MYSQL_USERNAME"),
  password = Sys.getenv("MYSQL_PASSWORD"),
  host     = Sys.getenv("MYSQL_HOST"),
  port     = 3306,
  dbname   = "c19r"
)

county_geom <- sf::st_read("map_data/geomUnitedStates.geojson")
stateline <- sf::st_read("map_data/US_stateLines.geojson")[,c('STUSPS','NAME', 'geometry')]
names(stateline) <- c('stname','name', 'geometry')

addResourcePath("www", "www")

source('./leaflet_inplace.R', local=T)
