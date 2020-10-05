#####################################################################
# COVID-19 Event Risk Assessment Planning Tool
# - preparing daily maps for the fixed event sizes
# Maps by Seolha Lee (seolha.lee@gatehc.edu)
# Aroon Chande <mail@aroonchande.com> <achande@ihrc.com>
#####################################################################
source("libraries.R")


args <- commandArgs(trailingOnly = TRUE)
current_time <- args[1]

dataQueryItaly <- function(date) {
  data <- read.csv(text = getURL(paste0("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province-", str_replace_all(as.character(date), "-", ""), ".csv")), stringsAsFactors = FALSE) %>%
    select(date = data, region = denominazione_regione, province = denominazione_provincia, code = codice_provincia, cases = totale_casi)
  return(data)
}

getDataItaly <- function() {
  # italy: need to download data_cur and data_past respectively
  cur_date <- ymd(gsub("-", "", Sys.Date())) - 1
  past_date <- ymd(cur_date) - 14

  data_past <- dataQueryItaly(past_date) %>%
    select(date, code, cases) # date, abbreviation_canton_and_fl, ncumul_conf
  data_cur <- dataQueryItaly(cur_date)
  for (i in c(1:13)) {
    data_cur <- data_cur %>% rbind(dataQueryItaly(cur_date - i))
  }
  data_cur <- data_cur %>%
    group_by(code) %>%
    dplyr::summarise(date = first(date), cases = first(cases), region = first(region), province = first(province), n = n())

  geom <<- st_read("map_data/italy_simpler.geojson")
  pop <- read.csv("map_data/italy_pop.csv", stringsAsFactors = FALSE)

  data_join <<- data_cur %>%
    inner_join(data_past, by = "code", suffix = c("", "_past")) %>%
    inner_join(pop, by = c("code"))

  pal <<- colorBin("YlOrRd", bins = c(0, 1, 25, 50, 75, 99, 100))
  legendlabs <<- c("< 1", " 1-25", "25-50", "50-75", "75-99", "> 99", "No or missing data")
}

# Create mouse-over labels
maplabsItaly <- function(riskData) {
  riskData <- riskData %>%
    mutate(risk = case_when(
      risk == 100 ~ "> 99",
      risk == 0 ~ "< 1",
      is.na(risk) ~ "No data",
      TRUE ~ as.character(risk)
    ))
  labels <- paste0(
    "<strong>", paste0(riskData$name, ", ", riskData$region), "</strong><br/>",
    "Current Risk Level: <b>", riskData$risk, ifelse(riskData$risk == "No data", "", " &#37;"), "</b><br/>",
    "Latest Update: ", substr(riskData$date, 1, 10)
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

getDataItaly()

data_Nr <- data_join %>%
  mutate(Nr = (cases - cases_past) * asc_bias)

riskdt <- data_Nr %>%
  mutate(risk = if_else(Nr > 10, round(calc_risk(Nr, size, pop)), 0))

riskdt_map <- geom %>% left_join(riskdt, by = c("prov_istat_code_num" = "code"))

map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  # setView(lat = 37.1, lng = -95.7, zoom = 4) %>%
  fitBounds(5, 37, 20, 48) %>%
  addPolygons(
    data = riskdt_map,
    color = "#444444", weight = 0.2, smoothFactor = 0.1,
    opacity = 1.0, fillOpacity = 0.7,
    fillColor = ~ pal(risk),
    highlight = highlightOptions(weight = 1),
    label = maplabsItaly(riskdt_map)
  ) %>%
  addLegend(
    data = riskdt_map,
    position = "topright", pal = pal, values = ~risk,
    title = "Risk Level (%)",
    opacity = 0.7,
    labFormat = function(type, cuts, p) {
      paste0(legendlabs)
    }
  ) %>%
  addEasyButton(easyButton(
    icon = "fa-crosshairs fa-lg", title = "Locate Me",
    onClick = JS("function(btn, map){ map.locate({setView: true, maxZoom: 7});}")
  ))
# map$dependencies[[1]]$src[1] <- "/srv/shiny-server/map_data/"
mapshot(map, file = file.path(getwd(), "daily_risk_map_italy",current_time, paste0(current_time,"_", asc_bias, "_", size, ".png")))
post_tweet(status = paste0("Italy province-level risk estimate update for ",  now("Europe/Rome"), " ", tz("Europe/Rome"), ".  Estimated risk that at least 1 person is #COVID19 positive for events or other areas where ", size, " individuals are in close contact [Assuming 10:1 ascertainment bias]"),
 media = file.path("daily_risk_map_italy", current_time, paste0(current_time,"_", asc_bias, "_", size, ".png")))
