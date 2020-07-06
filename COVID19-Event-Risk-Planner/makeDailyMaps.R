#####################################################################
# COVID-19 Event Risk Assessment Planning Tool 
# - preparing daily maps for the fixed event sizes
# Maps by Seolha Lee (seolha.lee@gatehc.edu)
# Aroon Chande <mail@aroonchande.com> <achande@ihrc.com>
#####################################################################
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(ggthemes)
library(jsonlite)
library(leaflet)
library(leaflet.extras)
library(lubridate)
library(mapview)
library(matlab)
library(RCurl)
library(rtweet)
library(sf)
library(withr)
Sys.setenv(PATH = with_path('/projects/covid19/bin', Sys.getenv("PATH")))


get_token()

args = commandArgs(trailingOnly=TRUE)
current_time <- args[1]

getData <- function() {
  dataurl <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
  data <- read.csv( dataurl, stringsAsFactors = FALSE) %>% mutate(date = as_date(date))
  county <<- st_read("map_data/tl_2017_us_county.geojson") 
  stateline <<- st_read("map_data/tl_2017_us_state.geojson")
  pop <- read.csv("map_data/county-population.csv", stringsAsFactors = FALSE)
  cur_date <- ymd(gsub("-", "", Sys.Date())) - 1
  past_date <- ymd(cur_date) - 14
  data_cur <- data %>% filter(date == cur_date) %>% select(c(fips, cases, deaths))
  data_past <- data %>%
    filter(date == past_date) %>%
    select(fips = fips, cases_past = cases)
  data_Nr <<- data_cur %>%
    inner_join(data_past, by = "fips") %>%
    inner_join(pop, by = "fips") %>%
    mutate(Nr = (cases - cases_past) * 10)
  pal <<- colorBin("YlOrRd", bins = c(0, 1, 25, 50, 75, 99, 100))
  legendlabs <<- c("< 1", " 1-25", "25-50", "50-75", "75-99", "> 99" , "No or missing data")
}

# Create mouse-over labels
maplabs <- function(riskData) {
  riskData <- riskData %>%
    mutate(risk = case_when(
      risk == 100 ~ '> 99',
      risk == 0 ~ '< 1',
      is.na(risk) ~ 'No data',
      TRUE ~ as.character(risk)
    ))
  labels <- paste0(
    "<strong>", paste0(riskData$NAME, " County, ", riskData$stname), "</strong><br/>",
    "Current Risk Level: <b>",riskData$risk, ifelse(riskData$risk == "No data", "", " &#37;"),"</b>"
  ) %>% lapply(htmltools::HTML)
  return(labels)
}



# Calculate risk
calc_risk <- function(I, g, pop) {
  p_I <- I / pop
  r <- 1 - (1 - p_I)**g
  return(round(r*100, 1))
}


######## create and save daily map widgets ########
event_size <<- c(10, 100, 1000, 10000, 100000)

getData()

if (dim(data_Nr)[1] > 2600){
  dir.create(file.path('daily_risk_map', current_time), recursive = T)

  maps = list()
  for ( size in event_size){

    # riskdt_map <-  data_Nr %>%  
    #     mutate(risk = if_else(Nr > 0, round(calc_risk(Nr, size, pop)), 0)) %>%
    #     right_join(county, by = c("fips" = "GEOID"))
    riskdt <- data_Nr %>% 
       mutate(risk = if_else(Nr > 0, round(calc_risk(Nr, size, pop)), 0))
    
    riskdt_map <- county %>% left_join(riskdt, by = c("GEOID" = "fips"))
    
    map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lat = 37.1, lng = -95.7, zoom = 4) %>%
      addPolygons(
        data = riskdt_map,
        color = "#444444", weight = 0.2, smoothFactor = 0.1,
        opacity = 1.0, fillOpacity = 0.5,
        fillColor = ~ pal(risk),
        highlight = highlightOptions(weight = 1),
        label = maplabs(riskdt_map)
      ) %>%
      addPolygons(
        data = stateline,
        fill = FALSE, color = "#943b29", weight = 1, smoothFactor = 0.5,
        opacity = 1.0) %>%
      addLegend(
        data = riskdt_map,
        position = "topright", pal = pal, values = ~risk,
        title = "Risk Level (%)",
        opacity = 1,
        labFormat = function(type, cuts, p) {
          paste0(legendlabs)
        }) 
    maps[[size]] = map
    maps[[size]]$dependencies[[1]]$src[1] = "/projects/covid19/COVID19-Event-Risk-Planner/map_data/"
  } 
  saveRDS(object = maps, file = file.path('daily_risk_map', current_time, 'riskmaps.rds'))
  saveRDS(object = maps, file = file.path('daily_risk_map','riskmaps.rds'))
  print(file.path('/projects/covid19/COVID19-Event-Risk-Planner/daily_risk_map', current_time, 'size_100.png'))
  mapshot(maps[[100]], file = file.path('/projects/covid19/COVID19-Event-Risk-Planner/daily_risk_map', current_time, 'size_100.png'))
  # post_tweet(status=paste0("County-level risk estimate update for ", ymd_hms(current_time), ".  Estimated risk for events or other areas where 100 individuals are in close contact"), media=file.path('daily_risk_map', current_time, 'size_100.png'))

}