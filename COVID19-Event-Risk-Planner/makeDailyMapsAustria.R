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

getDataAustria <- function() {
    
    data <- read.csv('https://covid19-dashboard.ages.at/data/CovidFaelle_Timeline_GKZ.csv',sep = ';',encoding = 'UTF-8', stringsAsFactors = FALSE) %>% 
        select(date = Time, name = Bezirk, code = GKZ, population = AnzEinwohner, cases = AnzahlFaelleSum)
    # format the date 
    for (i in 1:length(data$date)){
        data$date[i] <- unlist(strsplit(data$date[i],' '))[1]
    }
    data <- data %>% mutate(date = as.Date(format(strptime(as.character(date),"%d.%m.%Y"), "%Y-%m-%d")), 
               code = as.character(code)) %>% 
        arrange(desc(date)) %>% filter(!is.na(cases)) 
    geom <<- st_read('https://raw.githubusercontent.com/ginseng666/GeoJSON-TopoJSON-Austria/master/2017/simplified-99.9/bezirke_999_geo.json')
    
    cur_date <- ymd(gsub("-", "", Sys.Date()))-1 
    past_date <- ymd(cur_date) - 14
    data_cur <<- data %>% group_by(code) %>% 
        summarise(code = first(code), cases = first(cases), date = first(date), pop = first(population)) %>% 
        as.data.frame()
    data_past <- data %>% 
        filter(date <= past_date) %>% 
        group_by(code) %>% 
        summarise(code = first(code), cases = first(cases), date = first(date)) %>% 
        as.data.frame()
    data_join <<- data_cur %>%
        inner_join(data_past, by = "code", suffix=c('', '_past')) %>%
        mutate(n = date-date_past) 
    pal <<- colorBin("YlOrRd", bins = c(0, 1, 25, 50, 75, 99, 100))
    legendlabs <<- c("< 1", " 1-25", "25-50", "50-75", "75-99", "> 99" , "No or missing data")
}

# Create mouse-over labels
maplabsAustria <- function(riskData) {
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
asc_bias <- 3


getDataAustria()

data_Nr <- data_join %>%
mutate(Nr = (cases - cases_past) * asc_bias * 10/14) 

riskdt <- data_Nr %>% 
    mutate(risk = if_else(Nr > 10, round(calc_risk(Nr, size, pop)), 0))

riskdt_map <- geom %>% left_join(riskdt, by = c("iso" = "code"))
map <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    # setView(lat = 47, lng = -8.2, zoom = 4) %>%
    fitBounds(10, 48, 16.2, 46.4) %>% 
    addPolygons(
        data = riskdt_map,
        color = "#444444", weight = 0.2, smoothFactor = 0.1,
        opacity = 1.0, fillOpacity = 0.7,
        fillColor = ~ pal(risk),
        highlight = highlightOptions(weight = 1),
        label = maplabsAustria(riskdt_map)
    ) %>%
    addLegend(
        data = riskdt_map,
        position = "topright", pal = pal, values = ~risk,
        title = "Risk Level (%)",
        opacity = 0.7,
        labFormat = function(type, cuts, p) {
            paste0(legendlabs)
        })
mapshot(map, file = file.path(getwd(), "daily_risk_map_austria", current_time, paste0(current_time,"_", asc_bias, "_", size, ".png")))
post_tweet(status = paste0("Austrian state-level risk estimate update for ",  now("Europe/Vienna"), " ", tz("Europe/Vienna"), ".  Estimated risk that at least 1 person is #COVID19 positive for events or other areas where ", size, " individuals are in close contact [Assuming 3:1 ascertainment bias]"),
 media = file.path("daily_risk_map_austria", current_time, paste0(current_time,"_", asc_bias, "_", size, ".png")))
