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


getDataSpain <- function(){
  spain_geom <<- st_read('map_data/spain-provinces.geojson')
  #Main COVID-19 hub page: https://cnecovid.isciii.es/covid19/#distribuci%C3%B3n-geogr%C3%A1fica
  SPAIN<- read.csv("https://cnecovid.isciii.es/covid19/resources/casos_tecnica_provincia.csv", na.strings=FALSE) 
  #code link file
  SPAINcode = read.csv("map_data/spain_codenames.csv",encoding="UTF-8",na.strings=FALSE)
  #Population data comes from  Instituto Nacional de Estadística: https://www.ine.es/jaxiT3/Datos.htm?t=2852#!tabs-tabla
  
  DataJoin = c()
  Counties <- unique(SPAIN$provincia_iso)
  DataJoin$ProvinceName = Counties
  for(aa in 1:length(Counties)){
    Subset = SPAIN[SPAIN$provincia_iso==Counties[aa],] 
    Dates = as.Date(Subset$fecha)
    LL = length(Dates)
    ConfirmedCovidCases = cumsum(Subset$num_casos)
    #CaseDiff = 10*( Subset$ConfirmedCovidCases[LL] - Subset$ConfirmedCovidCases[LL - 14])/ 14
    cases = ConfirmedCovidCases[LL]
    cases_past = ConfirmedCovidCases[LL - 14]
    n  = ConfirmedCovidCases[LL] - ConfirmedCovidCases[LL - 14]
    #Make sure difference in cases is positive. If not set to NA.
    if(n<0){
      CaseDiff = NA
    }
    DataJoin$date[aa] = as.character(Dates[LL])
    DataJoin$n[aa] = n
    DataJoin$cases[aa] = cases
    DataJoin$cases_past[aa] = cases_past
  }
  
  SPAINdata = as.data.frame(DataJoin)
  SPAINcode = as.data.frame(SPAINcode)
  spain_data_join <<- inner_join(SPAINdata,SPAINcode,by=c("ProvinceName"="code")) %>% mutate(pop = population2019)
}
  

# Create mouse-over labels
maplabsSpain <- function(riskData) {
  riskData <- riskData %>%
    mutate(risk = case_when(
      risk == 100 ~ '> 99',
      risk == 0 ~ '< 1',
      is.na(risk) ~ 'No data',
      TRUE ~ as.character(risk)
    ))
  labels <- paste0(
    "<strong>", riskData$name, "</strong><br/>",
    "Current Risk Level: <b>",riskData$risk, ifelse(riskData$risk == "No data", "", "&#37;"),"</b><br/>",
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
asc_bias = 3

getDataSpain()


data_Nr <- data_join %>%
  mutate(Nr = (cases - cases_past) * asc_bias  * 10/14)
riskdt <- data_Nr %>%
  mutate(risk = if_else(Nr > 10, round(calc_risk(Nr, size, pop)), 0))

riskdt_map <- geom %>% left_join(riskdt, by = c("id" = "code"))

map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  # setView(lat = 37.1, lng = -95.7, zoom = 4) %>%
  fitBounds(-20, 44, 5, 26) %>%
  addPolygons(
    data = riskdt_map,
    color = "#444444", weight = 0.2, smoothFactor = 0.1,
    opacity = 1.0, fillOpacity = 0.7,
    fillColor = ~ pal(risk),
    highlight = highlightOptions(weight = 1),
    label = maplabsSpain(riskdt_map)
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

mapshot(map, file = file.path(getwd(), "daily_risk_map_spain", current_time, paste0(current_time,"_", asc_bias, "_", size, ".png")))
post_tweet(status = paste0("Spain province-level risk estimate update for ",  now("Europe/Madrid"), ".  Estimated risk that at least 1 person is #COVID19 positive for events or other areas where ", size, " individuals are in close contact [Assuming 3:1 ascertainment bias]"),
 media = file.path("daily_risk_map_spain", current_time, paste0(current_time,"_", asc_bias, "_", size, ".png")))
