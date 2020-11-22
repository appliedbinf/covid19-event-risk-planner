#####################################################################
# COVID-19 Event Risk Assessment Planning Tool
# - preparing daily maps for the fixed event sizes
# Quan Nguyen & Stephen Beckett
# Seolha Lee (seolha.lee@gatehc.edu)
# Aroon Chande <mail@aroonchande.com> <achande@ihrc.com>
#####################################################################
source("libraries.R")



get_token()

args <- commandArgs(trailingOnly = TRUE)
current_time <- args[1]

getDataSweden <- function() {
  temp <- tempfile()
  download.file(url = "https://fohm.maps.arcgis.com/sharing/rest/content/items/b5e7488e117749c19881cce45db13f7e/data", destfile = temp, mode="wb")
  swedenResource <- as.data.frame(readxl::read_excel(temp,col_names =T))
  unlink(temp)
  names(swedenResource)[1] <- 'date'
  swedenResource$date <- as.Date(swedenResource$date)
  SwedenCounty <- names(swedenResource)[3:length(names(swedenResource))]
  SwedenCounty[SwedenCounty == "Jämtland_Härjedalen"] <- "Jämtland"
  SwedenCounty[SwedenCounty == "Sörmland"] <- "Södermanland"
  SwedenCounty[SwedenCounty == "Västra_Götaland"] <- "Västra Götaland"
  names(swedenResource) = c(names(swedenResource)[1:2],SwedenCounty)

  data = swedenResource %>% 
    pivot_longer(3:23, names_to="County", values_to="cases") %>%
    select(-Totalt_antal_fall) %>%
    arrange(desc(date))

  geom <<- st_read("map_data/sweden-counties.geojson")
  pop <- read.csv("map_data/sweden_pop.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8")

  data_cur <<- data %>%
    group_by(County) %>%
    summarise(County = first(County), cases = sum(cases), date = first(date)) %>%
    as.data.frame()
  past_date <- data_cur$date[1] - 14
  data_past <- data %>%
    filter(date <= past_date) %>%
    group_by(County) %>%
    summarise(County = first(County), cases = sum(cases), date = first(date)) %>%
    as.data.frame()
  data_join <<- data_cur %>%
    inner_join(data_past, by = "County", suffix = c("", "_past")) %>%
    inner_join(pop, by = c("County")) 
  pal <<- colorBin("YlOrRd", bins = c(0, 1, 25, 50, 75, 99, 100))
  legendlabs <<- c("< 1", " 1-25", "25-50", "50-75", "75-99", "> 99", "No or missing data")
}

# Create mouse-over labels
maplabsSweden <- function(riskData) {
  riskData <- riskData %>%
    mutate(risk = case_when(
      risk == 100 ~ "> 99",
      risk == 0 ~ "< 1",
      is.na(risk) ~ "No data",
      TRUE ~ as.character(risk)
    ))
  labels <- paste0(
    "<strong>", paste0(riskData$name), "</strong><br/>",
    "Current Risk Level: <b>", riskData$risk, ifelse(riskData$risk == "No data", "", "&#37;"), "</b><br/>",
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

getDataSweden()


data_Nr <- data_join %>%
  mutate(Nr = (cases - cases_past) * asc_bias  * 10/14)
riskdt <- data_Nr %>%
  mutate(risk = if_else(Nr > 10, round(calc_risk(Nr, size, Population)), 0))

riskdt_map <- geom %>% left_join(riskdt, by = c("name" = "County"))

map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(17, 63, zoom=5) %>%
  addPolygons(
    data = riskdt_map,
    color = "#444444", weight = 0.2, smoothFactor = 0.1,
    opacity = 1.0, fillOpacity = 0.7,
    fillColor = ~ pal(risk),
    highlight = highlightOptions(weight = 1),
    label = maplabsSweden(riskdt_map)
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

mapshot(map, file = file.path(getwd(), "daily_risk_map_sweden", current_time, paste0(current_time,"_", asc_bias, "_", size, ".png")))
post_tweet(status = paste0("Swiss canton-level risk estimate update for ",  now("Europe/Zurich"), ".  Estimated risk that at least 1 person is #COVID19 positive for events or other areas where ", size, " individuals are in close contact [Assuming 5:1 ascertainment bias]"),
 media = file.path("daily_risk_map_swiss", current_time, paste0(current_time,"_", asc_bias, "_", size, ".png")))
