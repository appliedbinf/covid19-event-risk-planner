#####################################################################
# COVID-19 Event Risk Assessment Planning Tool 
# - preparing daily maps for the fixed event sizes
# Maps by Seolha Lee (seolha.lee@gatehc.edu)
# Aroon Chande <mail@aroonchande.com> <achande@ihrc.com>
#####################################################################
source("libraries.R")

get_token()

args = commandArgs(trailingOnly=TRUE)
current_time <- args[1]

getDataIreland <- function() {
    
    geom <<- st_read('map_data/Ireland_Counties.geojson')

    #Main COVID-19 hub page: https://covid-19.geohive.ie/datasets/d9be85b30d7748b5b7c09450b8aede63_0
    data <- read.csv("https://opendata.arcgis.com/datasets/d9be85b30d7748b5b7c09450b8aede63_0.csv") %>%
        mutate(date = as.Date(TimeStamp)) %>%
        select(CountyName, date, cases=ConfirmedCovidCases, pop = PopulationCensus16) %>%
        arrange(desc(date))
    data_cur <<- data %>%
        group_by(CountyName) %>%
        summarise(CountyName = first(CountyName), cases = first(cases), date = first(date), pop = first(pop)) %>%
        as.data.frame()
    past_date <- data_cur$date[1] - 14
    data_past <- data %>%
        filter(date == past_date) %>%
        group_by(CountyName) %>%
        summarise(CountyName = first(CountyName), cases = first(cases), date = first(date)) %>%
        as.data.frame()

    data_join <<- inner_join(data_cur, data_past, by = "CountyName", suffix=c('', '_past'))
    pal <<- colorBin("YlOrRd", bins = c(0, 1, 25, 50, 75, 99, 100))
    legendlabs <<- c("< 1", " 1-25", "25-50", "50-75", "75-99", "> 99" , "No or missing data")
}

# Create mouse-over labels
maplabsIreland <- function(riskData) {
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
    return(round(r*100, 1))
}


######## Create and save daily map widgets ########
size <- 50
asc_bias <- 5


getDataIreland()

data_Nr <- data_join %>%
mutate(Nr = (cases - cases_past) * asc_bias * 10/14) 

riskdt <- data_Nr %>% 
    mutate(risk = if_else(Nr > 10, round(calc_risk(Nr, size, pop)), 0))

riskdt_map <- geom %>% left_join(riskdt, by = c("id" = "CountyName"))
map <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    setView(lat = 53, lng = -7.5, zoom = 6) %>%
    addPolygons(
        data = riskdt_map,
        color = "#444444", weight = 0.2, smoothFactor = 0.1,
        opacity = 1.0, fillOpacity = 0.7,
        fillColor = ~ pal(risk),
        highlight = highlightOptions(weight = 1),
        label = maplabsIreland(riskdt_map)
    ) %>%
    addLegend(
        data = riskdt_map,
        position = "topright", pal = pal, values = ~risk,
        title = "Risk Level (%)",
        opacity = 0.7,
        labFormat = function(type, cuts, p) {
            paste0(legendlabs)
        })
mapshot(map, file = file.path(getwd(), "daily_risk_map_ireland", current_time, paste0(current_time,"_", asc_bias, "_", size, ".png")))
post_tweet(status = paste0("Ireland county-level risk estimate update for ",  now("Europe/Dublin"), " ", tz("Europe/Vienna"), ".  Estimated risk that at least 1 person is #COVID19 positive for events or other areas where ", size, " individuals are in close contact [Assuming 5:1 ascertainment bias]"),
 media = file.path("daily_risk_map_austria", current_time, paste0(current_time,"_", asc_bias, "_", size, ".png")))
