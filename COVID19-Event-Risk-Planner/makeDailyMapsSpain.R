#####################################################################
# COVID-19 Event Risk Assessment Planning Tool
# - preparing daily maps for the fixed event sizes
# Quan Nguyen &Stephen Beckett
# Maps by Seolha Lee (seolha.lee@gatehc.edu)
# Aroon Chande <mail@aroonchande.com> <achande@ihrc.com>
#####################################################################
source("libraries.R")



get_token()

args <- commandArgs(trailingOnly = TRUE)
current_time <- args[1]

#TODO: finish port
getDataSpain <- function() {
  dataurl <- getURL("https://raw.githubusercontent.com/openZH/covid_19/master/COVID19_Fallzahlen_CH_total_v2.csv") # date, abbreviation_canton_and_fl, ncumul_conf
  data <- read.csv(text = dataurl, stringsAsFactors = FALSE) %>%
    mutate(date = as_date(date)) %>%
    arrange(desc(date)) %>%
    filter(!is.na(ncumul_conf)) %>%
    select(date = date, code = abbreviation_canton_and_fl, cases = ncumul_conf)
  geom <<- st_read("map_data/spain-provinces.geojson")
  pop <- read.csv("map_data/spain_pop.csv", stringsAsFactors = FALSE)

  cur_date <- ymd(gsub("-", "", Sys.Date())) - 1
  past_date <- ymd(cur_date) - 14
  data_cur <<- data %>%
    group_by(code) %>%
    summarise(code = first(code), cases = first(cases), date = first(date)) %>%
    as.data.frame()
  data_past <- data %>%
    filter(date <= past_date) %>%
    group_by(code) %>%
    summarise(code = first(code), cases = first(cases), date = first(date)) %>%
    as.data.frame()
  data_join <<- data_cur %>%
    inner_join(data_past, by = "code", suffix = c("", "_past")) %>%
    inner_join(pop, by = c("code")) %>%
    mutate(n = date - date_past) %>%
    select(-c("name"))
  pal <<- colorBin("YlOrRd", bins = c(0, 1, 25, 50, 75, 99, 100))
  legendlabs <<- c("< 1", " 1-25", "25-50", "50-75", "75-99", "> 99", "No or missing data")
}

# Create mouse-over labels
maplabsSwiss <- function(riskData) {
  riskData <- riskData %>%
    mutate(risk = case_when(
      risk == 100 ~ "> 99",
      risk == 0 ~ "< 1",
      is.na(risk) ~ "No data",
      TRUE ~ as.character(risk)
    ))
  labels <- paste0(
    "<strong>", paste0("Canton of ", riskData$name), "</strong><br/>",
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
asc_bias = 5

getDataSwiss()


data_Nr <- data_join %>%
  mutate(Nr = (cases - cases_past) * asc_bias  * 10/14)
riskdt <- data_Nr %>%
  mutate(risk = if_else(Nr > 10, round(calc_risk(Nr, size, pop)), 0))

riskdt_map <- geom %>% left_join(riskdt, by = c("id" = "code"))

map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  # setView(lat = 37.1, lng = -95.7, zoom = 4) %>%
  fitBounds(7.5, 47.5, 9, 46) %>%
  addPolygons(
    data = riskdt_map,
    color = "#444444", weight = 0.2, smoothFactor = 0.1,
    opacity = 1.0, fillOpacity = 0.7,
    fillColor = ~ pal(risk),
    highlight = highlightOptions(weight = 1),
    label = maplabsSwiss(riskdt_map)
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

mapshot(map, file = file.path(getwd(), "daily_risk_map_swiss", current_time, paste0(current_time,"_", asc_bias, "_", size, ".png")))
post_tweet(status = paste0("Swiss canton-level risk estimate update for ",  now("Europe/Zurich"), ".  Estimated risk that at least 1 person is #COVID19 positive for events or other areas where ", size, " individuals are in close contact [Assuming 5:1 ascertainment bias]"),
 media = file.path("daily_risk_map_swiss", current_time, paste0(current_time,"_", asc_bias, "_", size, ".png")))
