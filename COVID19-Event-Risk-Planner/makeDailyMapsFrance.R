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

getDataFrance <- function() {
    
    data <- read.csv('https://www.data.gouv.fr/fr/datasets/r/406c6a23-e283-4300-9484-54e78c8ae675',sep = ';', stringsAsFactors = FALSE) %>%
        filter(cl_age90 == 0) %>% 
        select(code = dep, date = jour, cases = P) %>%
        mutate(date = as.Date(date)) %>% 
        filter(!is.na(cases)) 
    geom <<- st_read('https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/departements-version-simplifiee.geojson')
    pop <- read.csv("map_data/france_pop.csv", stringsAsFactors = FALSE) %>% select(code = Code, name = Department, pop = Population)
    
    depList <- unique(data$code) # get the list of all department codes
    
    # sort out and calculate the number of cases during two recent weeks
    # depList[code] = corresponding code of a department
    sortFunc <- function(code){
        deptCode <- depList[code] 
        department <- data %>% filter(code == deptCode) %>% distinct(date, .keep_all = TRUE) 
        latestDate <- department$date[length(department$date)]
        pastDate <- latestDate - 14
        difference <- sum(department[1:which(department$date == latestDate),'cases']) - sum(department[1:which(department$date == pastDate),'cases'])
        vec <- data.frame(code = depList[code], date = latestDate, n = difference)
        return(vec)
    }
    
    # get the data table that includes department codes, last updated date, difference between 14 days
    frenchTable <- data.frame()
    for (i in 1:length(depList)){
        vec <- sortFunc(i)
        frenchTable <- rbind(frenchTable,vec)
    }
    
    data_join <<- frenchTable %>%
        inner_join(pop, by = c("code")) %>%
        select(-c('name'))
    
    pal <<- colorBin("YlOrRd", bins = c(0, 1, 25, 50, 75, 99, 100))
    legendlabs <<- c("< 1", " 1-25", "25-50", "50-75", "75-99", "> 99" , "No or missing data")
}

# Create mouse-over labels
maplabsFrance <- function(riskData) {
    riskData <- riskData %>%
        mutate(risk = case_when(
            risk == 100 ~ '> 99',
            risk == 0 ~ '< 1',
            is.na(risk) ~ 'No data',
            TRUE ~ as.character(risk)
        ))
    labels <- paste0(
        "<strong>", paste0('Department of ', riskData$name), "</strong><br/>",
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


getDataFrance()

   
data_Nr <- data_join %>%
    mutate(Nr = n * asc_bias  * 10/14) 
       
riskdt <- data_Nr %>% 
    mutate(risk = if_else(Nr > 10, round(calc_risk(Nr, size, pop)), 0))

riskdt_map <- geom %>% left_join(riskdt, by = "code") %>% mutate(name = nom)

map <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    #setView(lat = 37.1, lng = -95.7, zoom = 4) %>%
    #fitBounds(7.5, 47.5, 9, 46) %>% 
    addPolygons(
        data = riskdt_map,
        color = "#444444", weight = 0.2, smoothFactor = 0.1,
        opacity = 1.0, fillOpacity = 0.7,
        fillColor = ~ pal(risk),
        highlight = highlightOptions(weight = 1),
        label = maplabsFrance(riskdt_map)
    ) %>%
    addLegend(
        data = riskdt_map,
        position = "topright", pal = pal, values = ~risk,
        title = "Risk Level (%)",
        opacity = 0.7,
        labFormat = function(type, cuts, p) {
            paste0(legendlabs)
        }) 
mapshot(map, file = file.path(getwd(), "daily_risk_map_france", current_time, paste0(current_time,"_", asc_bias, "_", size, ".png")))
post_tweet(status = paste0("France department-level risk estimate update for ",  now("Europe/Paris"), " ", tz("Europe/Paris"), ".  Estimated risk that at least 1 person is #COVID19 positive for events or other areas where ", size, " individuals are in close contact [Assuming 5:1 ascertainment bias]"),
 media = file.path("daily_risk_map_france", current_time, paste0(current_time,"_", asc_bias, "_", size, ".png")))
