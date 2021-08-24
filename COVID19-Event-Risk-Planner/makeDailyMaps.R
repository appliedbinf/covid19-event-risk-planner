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
library(glue)

get_token()

args <- commandArgs(trailingOnly = TRUE)
current_time <- args[1]
print(current_time)

getData <- function() {
    dataurl <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
  data <- read.csv(dataurl, stringsAsFactors = FALSE) %>% mutate(date = as_date(date))
  county <<- st_read("map_data/geomUnitedStates.geojson")
  stateline <<- st_read("map_data/US_stateLines.geojson")[,c('STUSPS','NAME')]
  names(stateline) <- c('stname','name')
  pop <- read.csv("map_data/county-population.csv", stringsAsFactors = FALSE)
  cur_date <- ymd(gsub("-", "", Sys.Date())) - 1
  past_date <- ymd(cur_date) - 14
  vacc_data <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/us_state_vaccinations.csv",stringsAsFactors = FALSE)
  VaccImm <<- vacc_data[which(vacc_data$date==past_date),] %>% select(location,pct_partially_vacc = people_vaccinated_per_hundred,pct_fully_vacc = people_fully_vaccinated_per_hundred)
  VaccImm$location[which(VaccImm$location=="New York State")] <<- "New York"
  data_cur <- data %>%
    filter(date == cur_date) %>%
    mutate(fips = case_when(
      county == "New York City" ~ 99999,
      TRUE ~ as.numeric(fips)
    )) %>%
    select(c(state, fips, cases, deaths))
  data_past <- data %>%
    filter(date == past_date) %>%
    mutate(fips = case_when(
      county == "New York City" ~ 99999,
      TRUE ~ as.numeric(fips)
    )) %>%
    select(fips = fips, cases_past = cases)
  data_join <<- data_cur %>%
    inner_join(data_past, by = "fips") %>%
    inner_join(pop, by = "fips")
  pal <<- colorBin("YlOrRd", bins = c(0, 1, 25, 50, 75, 99, 100))
  legendlabs <<- c("< 1", " 1-25", "25-50", "50-75", "75-99", "> 99", "No or missing data")
}

# Create mouse-over labels
maplabs <- function(riskData) {
  riskData <- riskData %>%
    mutate(risk = case_when(
      risk == 100 ~ "> 99",
      risk < 1 ~ "< 1",
      risk == 0 ~ "No data",
      is.na(risk) ~ "No data",
      TRUE ~ as.character(risk)
    ))
  labels <- paste0(
    "<strong>", paste0(riskData$NAME, ", ", riskData$stname), "</strong><br/>",
    "Current Risk Level: <b>", riskData$risk, ifelse(riskData$risk == "No data", "", "&#37;"), "</b><br/>",
    "Updated: ", now("America/New_York"), " Eastern time"
  ) %>% lapply(htmltools::HTML)
  return(labels)
}



# Calculate risk
calc_risk <- function(I, g, pop) {
  p_I <- I / pop
  r <- 1 - (1 - p_I)**g
  return(round(r * 100, 1))
}


######## create and save daily map widgets ########
event_size = c(10, 15, 20, 25, 50, 100, 500, 1000, 5000)
asc_bias_list <<- c(3,4,5)
scale_factor = 10/14


getData()
risk_data = list()

for (asc_bias in asc_bias_list) {
  data_Nr <- data_join %>%
    mutate(Nr = (cases - cases_past) * asc_bias * scale_factor)

  if (dim(data_Nr)[1] > 2000) {
    # dir.create(file.path('daily_risk_map', current_time), recursive = T)

    maps <- list()
    for (size in event_size) {

      cn = glue("{asc_bias}_{size}")
      riskdt <- data_Nr %>%
        mutate(risk = if_else(Nr > 10, round(calc_risk(Nr, size, pop)), 0), "asc_bias" = asc_bias, "event_size" = size)
      risk_data[[cn]] = riskdt %>% select(state, fips, "{cn}" := risk)
      # riskdt_map <- county %>% left_join(riskdt, by = c("GEOID" = "fips"))
      # id = paste(asc_bias, size, sep="_")
      # risk_data[[id]] =  st_drop_geometry(riskdt_map)

      # map <- leaflet() %>%
      #   addProviderTiles(providers$CartoDB.Positron) %>%
      #   setView(lat = 37.1, lng = -95.7, zoom = 4) %>%
      #   addPolygons(
      #     data = riskdt_map,
      #     color = "#444444", weight = 0.2, smoothFactor = 0.1,
      #     opacity = 1.0, fillOpacity = 0.7,
      #     fillColor = ~ pal(risk),
      #     highlight = highlightOptions(weight = 1),
      #     label = maplabs(riskdt_map)
      #   ) %>%
      #   addPolygons(
      #     data = stateline,
      #     fill = FALSE, color = "#943b29", weight = 1, smoothFactor = 0.5,
      #     opacity = 1.0
      #   ) %>%
      #   addLegend(
      #     data = riskdt_map,
      #     position = "topright", pal = pal, values = ~risk,
      #     title = "Risk Level (%)",
      #     opacity = .7,
      #     labFormat = function(type, cuts, p) {
      #       paste0(legendlabs)
      #     }
      #   ) %>%
      #   addEasyButton(easyButton(
      #     icon = "fa-crosshairs fa-lg", title = "Locate Me",
      #     onClick = JS("function(btn, map){ map.locate({setView: true, maxZoom: 7});}")
      #   ))
      # maps[[size]] <- map
      # maps[[size]]$dependencies[[1]]$src[1] <- "/srv/shiny-server/map_data/"
      # mapshot(map, url = file.path(getwd(), "www", paste0(asc_bias, "_", size, ".html")))
    }

    # saveRDS(object = maps, file = file.path('daily_risk_map', current_time, paste0('riskmaps_',asc_bias,'.rds')))
    # saveRDS(object = maps, file = file.path('daily_risk_map', paste0('riskmaps_',asc_bias,'.rds')))
    # print(file.path('/srv/shiny-server/daily_risk_map', current_time, 'asc_10_size_100.png'))
  }
  if (asc_bias == 5 & args[2] == "1") {
    for (size in c(25, 50)) {
      riskdt <- data_Nr %>%
        mutate(risk = if_else(Nr > 10, round(calc_risk(Nr, size, pop)), 0))

      riskdt_map <- county %>% left_join(riskdt, by = c("GEOID" = "fips"))

      map <- leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lat = 37.1, lng = -95.7, zoom = 4) %>%
        addPolygons(
          data = riskdt_map,
          color = "#444444", weight = 0.2, smoothFactor = 0.1,
          opacity = 1.0, fillOpacity = 1.0,
          fillColor = ~ pal(risk),
          highlight = highlightOptions(weight = 1),
          label = maplabs(riskdt_map)
        ) %>%
        addPolygons(
          data = stateline,
          fill = FALSE, color = "#943b29", weight = 1, smoothFactor = 0.5,
          opacity = 1.0
        ) %>%
        addLegend(
          data = riskdt_map,
          position = "topright", pal = pal, values = ~risk,
          title = "Risk Level (%)",
          opacity = 1,
          labFormat = function(type, cuts, p) {
            paste0(legendlabs)
          }
        )

      map$dependencies[[1]]$src[1] <- "/srv/shiny-server/map_data/"
      print("Map to png")
      mapshot(map, file = file.path("/srv/shiny-server/daily_risk_map", current_time, paste0("asc_5_size_", size, ".png")))
      post_tweet(status = paste0("County-level risk estimate update for ", ymd_hms(current_time), ".  Estimated risk that at least 1 person is #COVID19 positive for events or other areas where ", size, " individuals are in close contact [Assuming 3:1 ascertainment bias]"), media = file.path("daily_risk_map", current_time, paste0("asc_5_size_", size, ".png")))
      # print(paste0("County-level risk estimate update for ", ymd_hms(current_time), ".  Estimated risk that at least 1 person is #COVID19 positive for events or other areas where " ,size,  " individuals are in close contact. [Assuming 10:1 ascertainment bias]"))
    }
  }
}

risk_data = county %>%
  left_join(plyr::join_all(risk_data, by=c("fips", "state")), by=c("GEOID" = "fips")) %>%
  left_join(VaccImm, by=c("state" = "location")) %>% st_drop_geometry() %>%
  mutate(imOp = case_when(pct_fully_vacc < 50 ~ 0.0, pct_fully_vacc > 50 ~ pct_fully_vacc/100)) %>%
  mutate(updated = ymd(gsub("-", "", Sys.Date())))
write.csv(risk_data, "www/usa_risk_counties.csv", quote=F, row.names=F)