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
library(tidyr)
library(withr)
library(vroom)
library(glue)

get_token()

args <- commandArgs(trailingOnly = TRUE)
current_time <- args[1]
print(current_time)

getData <- function() {
    dataurl <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
  data <- vroom(dataurl)
  county <<- st_read("map_data/geomUnitedStates.geojson")
  stateline <<- st_read("map_data/US_stateLines.geojson")[,c('STUSPS','NAME')]
  names(stateline) <- c('stname','name')
  pop <- vroom("map_data/county_population.csv")
  
  cur_date <- ymd(gsub("-", "", Sys.Date())) - 1
  past_date <- ymd(cur_date) - 14
  # vaccine
  nyc <- c(36005, 36047, 36061, 36085, 36081)
  vacc_data <- vroom("https://raw.githubusercontent.com/bansallab/vaccinetracking/main/vacc_data/data_county_timeseries.csv")
  vacc_data <- vacc_data%>%
      # filter(CASE_TYPE %in% c("Complete","Partial"))%>%
      filter(CASE_TYPE %in% c("Complete"))%>%
      pivot_wider(
          names_from = CASE_TYPE,
          values_from = CASES
      )%>%
      select(
          county = COUNTY,
          cnt_fully_vacc = Complete,
          # cnt_partially_vacc = Partial, # removing partials
          date = DATE
      )%>%
      mutate(
          
          date = as_date(date),
          county = case_when(
              as.numeric(county) %in% c(2164, 2060) ~ 2997,
              as.numeric(county) %in% c(2282, 2105) ~ 2998,
              as.numeric(county) %in% nyc ~ 99999,
              TRUE ~ as.numeric(county)
          )
      )%>%
      group_by(
          date, county
      )%>%
      summarise(
          cnt_fully_vacc = sum(cnt_fully_vacc),
          # cnt_partially_vacc = sum(cnt_partially_vacc)
      )%>%
      ungroup()
  
  ex_dates <- c(
      vacc_data$date%>%
          sort()%>%
          first(),
      vacc_data$date%>%
          sort()%>%
          last()
  )
  
  all_dates <- ex_dates[1]+0:as.numeric(ex_dates[2]-ex_dates[1])
  all_county <- c(vacc_data$county, c(29991, 29992))%>% # add Joplin and KC
      unique()
  
  add_dates <- purrr::map_df(all_county,function(x){
      tibble(
          county = x,
          date = all_dates
      )
  })
  
  vacc_data_fill <- vacc_data%>%
      mutate(
          last_date = date
      )%>%
      full_join(
          add_dates
      )%>%
      group_by(county)%>%
      arrange(date)%>%
      mutate(
          cnt_fully_vacc = zoo::na.approx(cnt_fully_vacc, na.rm=F),
          # cnt_partially_vacc = na.approx(cnt_partially_vacc, na.rm=F),
          # the 'is_last' variable is telling us that it could not interpolate cnt_fully_vacc because the date is after the last value in the bansal data set.
            # Therefore, when is_last == T, we can display the "last date" in the mouseover UI
          is_last = case_when(
              is.na(cnt_fully_vacc)==T ~ TRUE,
              TRUE ~ FALSE
          )
      )%>%
      fill(
          cnt_fully_vacc,
          # cnt_partially_vacc,
          last_date
      )%>%
      ungroup()
  
  VaccImm <<- vacc_data_fill%>%
      filter(date==past_date)%>%
      inner_join(pop, by = c("county"="fips"))%>%
      select(
          -date,
          # location,
          # pct_partially_vacc = people_vaccinated_per_hundred,
          # pct_fully_vacc = people_fully_vaccinated_per_hundred
          )%>%
      mutate( # Creating a new column to group by so we can give the same vaccination rates to areas surrounding Joplin and KC
          v = case_when(
            county %in% c(29095, 29047, 29165, 29037, 29991) ~ 29993,
            county %in% c(29097, 29145, 29992) ~ 29994,
            TRUE ~ county
          )
      )%>%
      group_by(v)%>%
      mutate(
          pct_fully_vacc = sum(cnt_fully_vacc, na.rm=T)/sum(pop, na.rm=T)*100
          )%>%
      select(-v)
  # VaccImm$location[which(VaccImm$location=="New York State")] <<- "New York"
  
  data_cur <- data %>%
    filter(date == cur_date) %>%
    mutate(fips = case_when(
      county == "New York City" ~ 99999,
      county == "Kansas City" ~ 29991,
      county == "Joplin" ~ 29992,
      TRUE ~ as.numeric(fips)
    )) %>%
    select(c(state, fips, cases, deaths))
  data_past <- data %>%
    filter(date == past_date) %>%
    mutate(fips = case_when(
      county == "New York City" ~ 99999,
      county == "Kansas City" ~ 29991,
      county == "Joplin" ~ 29992,
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

# Risk and Vaccination layer: includes Joplin and KC
risk_data = county %>%
  left_join(plyr::join_all(risk_data, by=c("fips", "state")), by=c("GEOID" = "fips")) %>%
  left_join(VaccImm, by=c("GEOID" = "county")) %>% st_drop_geometry() %>%
  mutate(
      imOp = case_when(pct_fully_vacc < 50 ~ 0.0, pct_fully_vacc > 50 ~ 0.7)) %>% # binary filter
  mutate(updated = ymd(gsub("-", "", Sys.Date())))


write.csv(risk_data, "www/usa_risk_counties.csv", quote=F, row.names=F)