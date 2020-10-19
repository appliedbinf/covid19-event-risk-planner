#####################################################################
# COVID-19 Event Risk Assessment Planning Tool 
# - preparing daily maps for the fixed event sizes
# Maps by Seolha Lee (seolha.lee@gatehc.edu)
# Aroon Chande <mail@aroonchande.com> <achande@ihrc.com>
#####################################################################
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(lubridate)
library(matlab)
library(RCurl)
library(sf)
library(utils)
Sys.setenv(PATH = with_path('/projects/covid19/bin', Sys.getenv("PATH")))



get_token()

args = commandArgs(trailingOnly=TRUE)
current_time <- args[1]

getDataFrance <- function() {
    
    data <- read.csv('https://www.data.gouv.fr/fr/datasets/r/406c6a23-e283-4300-9484-54e78c8ae675',sep = ';', stringsAsFactors = FALSE) %>%
        filter(cl_age90 == 0) %>% 
        select(code = dep, date = jour, cases = P) %>%
        mutate(date = as.Date(date)) %>% 
        arrange(desc(date)) %>% filter(!is.na(cases)) 
    geom <<- st_read('https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/departements-version-simplifiee.geojson')
    pop <- read.csv("map_data/france_pop.csv", stringsAsFactors = FALSE) %>% select(code = Code, name = Department, pop = Population)
    
    cur_date <- ymd(gsub("-", "", Sys.Date()))-1 
    past_date <- ymd(cur_date) - 14
    data_cur <<- data %>% group_by(code) %>% 
        summarise(code = first(code), cases = first(cases), date = first(date)) %>% 
        as.data.frame()
    data_past <- data %>% 
        filter(date <= past_date) %>% 
        group_by(code) %>% 
        summarise(code = first(code), cases = first(cases), date = first(date)) %>% 
        as.data.frame()
    data_join <<- data_cur %>%
        inner_join(data_past, by = "code", suffix=c('', '_past')) %>%
        inner_join(pop, by = c("code")) %>%
        mutate(n = date-date_past) %>% 
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
event_size <<- c(10, 25, 50, 100, 500, 1000, 5000, 10000)
asc_bias_list <<-c(5, 10)

getDataFrance()

for ( asc_bias in asc_bias_list ){
    
    data_Nr <- data_join %>%
        mutate(Nr = (cases - cases_past) * asc_bias) 
    print(dim(data_Nr)[1])
    if (dim(data_Nr)[1] > 10){
        dir.create(file.path('daily_risk_map_france'), recursive = T)
        
        maps = list()
        for ( size in event_size ){
            
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
            maps[[size]] = map
            maps[[size]]$dependencies[[1]]$src[1] = "/srv/shiny-server/map_data/"
            mapshot(map, url = file.path('www', paste0('france_', asc_bias,'_', size,'.html')))
            
        } 
        
        #saveRDS(object = maps, file = file.path('daily_risk_map', current_time, paste0('riskmaps_',asc_bias,'.rds')))
        #saveRDS(object = maps, file = file.path('daily_risk_map', paste0('riskmaps_',asc_bias,'.rds')))
        #print(file.path('/srv/shiny-server/daily_risk_map', current_time, 'asc_10_size_100.png'))
        
        
    }
    # if (asc_bias == 10 & args[2] == "1" ){
    #       for (size in c(25,50)){
    #           riskdt <- data_Nr %>%
    #               mutate(risk = if_else(Nr > 10, round(calc_risk(Nr, size, pop)), 0))
    #           
    #           riskdt_map <- county %>% left_join(riskdt, by = c("GEOID" = "fips"))
    #           
    #           map <- leaflet() %>%
    #               addProviderTiles(providers$CartoDB.Positron) %>%
    #               setView(lat = 37.1, lng = -95.7, zoom = 4) %>%
    #               addPolygons(
    #                   data = riskdt_map,
    #                   color = "#444444", weight = 0.2, smoothFactor = 0.1,
    #                   opacity = 1.0, fillOpacity = 1.0,
    #                   fillColor = ~ pal(risk),
    #                   highlight = highlightOptions(weight = 1),
    #                   label = maplabs(riskdt_map)
    #               ) %>%
    #               addPolygons(
    #                   data = stateline,
    #                   fill = FALSE, color = "#943b29", weight = 1, smoothFactor = 0.5,
    #                   opacity = 1.0) %>%
    #               addLegend(
    #                   data = riskdt_map,
    #                   position = "topright", pal = pal, values = ~risk,
    #                   title = "Risk Level (%)",
    #                   opacity = 1,
    #                   labFormat = function(type, cuts, p) {
    #                       paste0(legendlabs)
    #                   })
    #           
    #           map$dependencies[[1]]$src[1] = "/srv/shiny-server/map_data/"
    #           print("Map to png" )
    #           mapshot(map, file = file.path('/srv/shiny-server/daily_risk_map', current_time, paste0('asc_10_size_',size,'.png')))
    #           post_tweet(status=paste0("County-level risk estimate update for ", ymd_hms(current_time), ".  Estimated risk that at least 1 person is #COVID19 positive for events or other areas where ",size," individuals are in close contact [Assuming 10:1 ascertainment bias]"), media=file.path('daily_risk_map', current_time, paste0('asc_10_size_',size,'.png')))
    #           #print(paste0("County-level risk estimate update for ", ymd_hms(current_time), ".  Estimated risk that at least 1 person is #COVID19 positive for events or other areas where " ,size,  " individuals are in close contact. [Assuming 10:1 ascertainment bias]"))
    #         }}
}
