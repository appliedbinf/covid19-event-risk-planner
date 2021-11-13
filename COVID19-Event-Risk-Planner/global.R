readRenviron('~/.Renviron')
library(shiny)
library(withr)
library(ggplot2)
library(ggrepel)
library(matlab)
library(lubridate)
library(dplyr)
library(ggthemes)
library(leaflet)
library(sever)
library(DBI)
library(dbplyr)
library(glue)
options(shiny.reactlog = FALSE)

db <- dbConnect(
  drv      = RMySQL::MySQL(),
  username = Sys.getenv("MYSQL_USERNAME"),
  password = Sys.getenv("MYSQL_PASSWORD"),
  host     = Sys.getenv("MYSQL_HOST"),
  port     = 3306,
  dbname   = "c19r"
)


source("R/constants.R", local = T)
source("R/helpers.R", local = T)
source('R/leaflet_inplace.R', local=T)


addResourcePath("www", "www")


