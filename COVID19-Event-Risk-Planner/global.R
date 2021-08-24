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

# db <- dbConnect(
#   drv      = RMySQL::MySQL(),
#   username = "riskmaster",
#   password = "iH8j4DRKNPoVeDaxpXzd",
#   host     = "weitz-covid-dev.cnoy4sgleu5u.us-east-1.rds.amazonaws.com",
#   port     = 3306,
#   dbname   = "usa_counties"
# )

usa_counties <- vroom::vroom('www/usa_risk_counties.csv') %>%
  select(-NAME, -stname) %>%
  mutate_at(vars(-GEOID, -state, -updated), as.numeric)

county_geom <- sf::st_read("map_data/geomUnitedStates.geojson")

usa_counties = county_geom %>% left_join(usa_counties, by = c("GEOID" = "GEOID"))
