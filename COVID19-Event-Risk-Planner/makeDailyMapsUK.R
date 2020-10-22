#####################################################################
# COVID-19 Event Risk Assessment Planning Tool
# - preparing daily maps for the fixed event sizes
# Maps by Seolha Lee (seolha.lee@gatehc.edu)
# Aroon Chande <mail@aroonchande.com> <achande@ihrc.com>
#####################################################################
source("libraries.R")


get_token()

args <- commandArgs(trailingOnly = TRUE)
current_time <- args[1]

dataQueryUK <- function(date) {
  dataURL <- paste0("https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=utla;date=", date, '&structure={"date":"date","code":"areaCode","cases":"cumCasesBySpecimenDate"}')
  response <- httr::GET(
    url = dataURL,
    timeout(10)
  )
  if (response$status_code >= 400) {
    err_msg <- httr::http_status(response)
    stop(err_msg)
  } else if (response$status_code >= 204){
    cur_date <<- date - 1 
    dataURL <- paste0("https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=utla;date=", cur_date, '&structure={"date":"date","code":"areaCode","cases":"cumCasesBySpecimenDate"}')
  response <- httr::GET(
    url = dataURL,
    timeout(10)
  )

  }
  # Convert response from binary to JSON:
  json_text <- content(response, "text")
  data <- jsonlite::fromJSON(json_text)$data %>%
    mutate(date = as_date(date))
  return(data)
}

getDataUK <- function() {
  cur_date <<- ymd(gsub("-", "", Sys.Date())) - 1

  data_cur <- dataQueryUK(cur_date)
  past_date <- ymd(cur_date) - 14
  data_past <- dataQueryUK(past_date)
  for (i in c(1:13)) {
    data_cur <- data_cur %>% rbind(dataQueryUK(cur_date - i))
  }
  data_cur <- data_cur %>%
    group_by(code) %>%
    dplyr::summarise(date = first(date), cases = first(cases), n = n())

  geom <<- st_read("https://opendata.arcgis.com/datasets/b216b4c8a4e74f6fb692a1785255d777_0.geojson", stringsAsFactors = FALSE) %>%
    rename(code = ctyua19cd, name = ctyua19nm)
  pop <- read.csv("map_data/uk_pop.csv", stringsAsFactors = FALSE) %>% select(-c("name"))

  data_join <<- data_cur %>%
    inner_join(data_past, by = "code", suffix = c("", "_past")) %>%
    inner_join(pop, by = c("code"))
  pal <<- colorBin("YlOrRd", bins = c(0, 1, 25, 50, 75, 99, 100))
  legendlabs <<- c("< 1", " 1-25", "25-50", "50-75", "75-99", "> 99", "No or missing data")
}

# Create mouse-over labels
maplabsUK <- function(riskData) {
  riskData <- riskData %>%
    mutate(risk = case_when(
      risk == 100 ~ "> 99",
      risk == 0 ~ "< 1",
      is.na(risk) ~ "No data",
      TRUE ~ as.character(risk)
    )) %>%
    mutate(country = case_when(
      startsWith(code, "E") ~ "England",
      startsWith(code, "N") ~ "Northern Ireland",
      startsWith(code, "W") ~ "Wales",
      startsWith(code, "S") ~ "Scotland",
      TRUE ~ ""
    )) %>%
    mutate(name = case_when(
      name == "Kingston upon Hull, City of" ~ "Kingston upon Hull",
      name == "Herefordshire, County of" ~ "Herefordshire",
      name == "Bristol, City of" ~ "Bristol",
      TRUE ~ name
    ))
  labels <- paste0(
    "<strong>", paste0(riskData$name, ", ", riskData$country), "</strong><br/>",
    "Current Risk Level: <b>", riskData$risk, ifelse(riskData$risk == "No data", "", " &#37;"), "</b><br/>",
    "Latest Update: ", riskData$date
  ) %>% lapply(htmltools::HTML)
  return(labels)
}



# Calculate risk
calc_risk <- function(I, g, pop) {
  p_I <- I / pop
  r <- 1 - (1 - p_I)**g
  return(round(r * 100, 1))
}


######## Create and save daily map widgets ########
size <- 50
asc_bias <- 10

getDataUK()

data_Nr <- data_join %>%
  mutate(Nr = (cases - cases_past) * asc_bias)
riskdt <- data_Nr %>%
  mutate(risk = if_else(Nr > 10, round(calc_risk(Nr, size, pop)), 0))

riskdt_map <- geom %>% left_join(riskdt, by = c("code"))

# riskdt_hatch <- HatchedPolygons::hatched.SpatialPolygons(riskdt_map, density = c(6,4), angle = c(45, 135))

map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  # setView(lat = 37.1, lng = -95.7, zoom = 4) %>%
  fitBounds(-8.2, 60, 0, 49.5) %>%
  addPolygons(
    data = riskdt_map,
    color = "#444444", weight = 0.2, smoothFactor = 0.1,
    opacity = 1.0, fillOpacity = 0.7,
    fillColor = ~ pal(risk),
    highlight = highlightOptions(weight = 1.5, bringToFront = T),
    label = maplabsUK(riskdt_map)
  ) %>%
  addLegend(
    data = riskdt_map,
    position = "topright", pal = pal, values = ~risk,
    title = "Risk Level (%)",
    opacity = 0.7,
    labFormat = function(type, cuts, p) {
      paste0(legendlabs)
    }
  )
mapshot(map, file = file.path(getwd(), "daily_risk_map_uk", current_time, paste0(current_time,"_", asc_bias, "_", size, ".png")))
post_tweet(status = paste0("UK county-level risk estimate update for ",  now("Europe/London"), " ", tz("Europe/London"), ".  Estimated risk that at least 1 person is #COVID19 positive for events or other areas where ", size, " individuals are in close contact [Assuming 10:1 ascertainment bias]"),
 media = file.path("daily_risk_map_uk", current_time, paste0(current_time,"_", asc_bias, "_", size, ".png")))
