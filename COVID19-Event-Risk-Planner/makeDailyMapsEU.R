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
    timeout(60)
  )
  if (response$status_code >= 400) {
    err_msg <- httr::http_status(response)
    stop(err_msg)
  } else if (response$status_code >= 204) {
    cur_date <<- date - 1
    date = cur_date
    dataURL <- paste0("https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=utla;date=", date, '&structure={"date":"date","code":"areaCode","cases":"cumCasesBySpecimenDate"}')
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

  uk_geom <<- st_read("https://opendata.arcgis.com/datasets/b216b4c8a4e74f6fb692a1785255d777_0.geojson", stringsAsFactors = FALSE) %>%
    rename(code = ctyua19cd, name = ctyua19nm)
  pop <- read.csv("map_data/uk_pop.csv", stringsAsFactors = FALSE) %>% select(-c("name"))

  uk_data_join <<- data_cur %>%
    inner_join(data_past, by = "code", suffix = c("", "_past")) %>%
    inner_join(pop, by = c("code"))
  uk_pal <<- colorBin("YlOrRd", bins = c(0, 1, 25, 50, 75, 99, 100))
  uk_legendlabs <<- c("< 1", " 1-25", "25-50", "50-75", "75-99", "> 99", "No or missing data")
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
    "Current Risk Level: <b>", riskData$risk, ifelse(riskData$risk == "No data", "", "&#37;"), "</b><br/>",
    "Latest Update: ", riskData$date
  ) %>% lapply(htmltools::HTML)
  return(labels)
}

getDataSwiss <- function() {
  dataurl <- getURL("https://raw.githubusercontent.com/openZH/covid_19/master/COVID19_Fallzahlen_CH_total_v2.csv") # date, abbreviation_canton_and_fl, ncumul_conf
  liechtenstein <- read.csv(paste0('https://raw.githubusercontent.com/openZH/covid_19/master/fallzahlen_kanton_total_csv/COVID19_Fallzahlen_FL_total.csv')) %>%
    mutate(date = as_date(date)) %>% arrange(desc(date)) %>% filter(!is.na(ncumul_conf)) %>% 
    select(date = date, code = abbreviation_canton_and_fl, cases = ncumul_conf)
  data <- read.csv(text = dataurl, stringsAsFactors = FALSE) %>%
    mutate(date = as_date(date)) %>%
    arrange(desc(date)) %>%
    filter(!is.na(ncumul_conf)) %>%
    select(date = date, code = abbreviation_canton_and_fl, cases = ncumul_conf) %>% 
    rbind(liechtenstein)
  swiss_geom <<- st_read("https://gist.githubusercontent.com/mbostock/4207744/raw/3232c7558742bab53227e242a437f64ae4c58d9e/readme-swiss.json")
  pop <- read.csv("map_data/swiss_canton_pop.csv", stringsAsFactors = FALSE)

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
  swiss_data_join <<- data_cur %>%
    inner_join(data_past, by = "code", suffix = c("", "_past")) %>%
    inner_join(pop, by = c("code")) %>%
    mutate(n = date - date_past) %>%
    select(-c("name"))
  swiss_pal <<- colorBin("YlOrRd", bins = c(0, 1, 25, 50, 75, 99, 100))
  swiss_legendlabs <<- c("< 1", " 1-25", "25-50", "50-75", "75-99", "> 99", "No or missing data")
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
    "Current Risk Level: <b>", riskData$risk, ifelse(riskData$risk == "No data", "", "&#37;"), "</b><br/>",
    "Latest Update: ", substr(riskData$date, 1, 10)
  ) %>% lapply(htmltools::HTML)
  return(labels)
}

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

  italy_geom <<- st_read("map_data/italy_simpler.geojson")
  pop <- read.csv("map_data/italy_pop.csv", stringsAsFactors = FALSE)

  italy_data_join <<- data_cur %>%
    inner_join(data_past, by = "code", suffix = c("", "_past")) %>%
    inner_join(pop, by = c("code"))

  italy_pal <<- colorBin("YlOrRd", bins = c(0, 1, 25, 50, 75, 99, 100))
  italy_legendlabs <<- c("< 1", " 1-25", "25-50", "50-75", "75-99", "> 99", "No or missing data")
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
    "Current Risk Level: <b>", riskData$risk, ifelse(riskData$risk == "No data", "", "&#37;"), "</b><br/>",
    "Latest Update: ", substr(riskData$date, 1, 10)
  ) %>% lapply(htmltools::HTML)
  return(labels)
}


getDataFrance <- function() {
    
    data <- read.csv('https://www.data.gouv.fr/fr/datasets/r/406c6a23-e283-4300-9484-54e78c8ae675',sep = ';', stringsAsFactors = FALSE) %>%
        filter(cl_age90 == 0) %>% 
        select(code = dep, date = jour, cases = P) %>%
        mutate(date = as.Date(date)) %>% 
        arrange(desc(date)) %>% filter(!is.na(cases)) 
    france_geom <<- st_read('map_data/france.geojson')
    pop <- read.csv("map_data/france_pop.csv", stringsAsFactors = FALSE) %>% select(code = Code, name = Department, pop = Population)
    
    cur_date <- ymd(gsub("-", "", Sys.Date()))-1 
    past_date <- ymd(cur_date) - 14
    data_cur <<- data %>% group_by(code) %>% 
        summarise(code = first(code), cases = sum(cases), date = first(date)) %>% 
        as.data.frame()
    data_past <- data %>% 
        filter(date <= past_date) %>% 
        group_by(code) %>% 
        summarise(code = first(code), cases = sum(cases), date = first(date)) %>% 
        as.data.frame()
    france_data_join <<- data_cur %>%
        inner_join(data_past, by = "code", suffix=c('', '_past')) %>%
        inner_join(pop, by = c("code")) %>%
        mutate(n = date-date_past) %>% 
        select(-c('name'))
    france_pal <<- colorBin("YlOrRd", bins = c(0, 1, 25, 50, 75, 99, 100))
    france_legendlabs <<- c("< 1", " 1-25", "25-50", "50-75", "75-99", "> 99" , "No or missing data")
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
        "<strong>", paste0('Department of ', riskData$nom), "</strong><br/>",
        "Current Risk Level: <b>",riskData$risk, ifelse(riskData$risk == "No data", "", "&#37;"),"</b><br/>",
        "Latest Update: ", substr(riskData$date, 1, 10)
    ) %>% lapply(htmltools::HTML)
    return(labels)
}

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
    austria_geom <<- st_read('https://raw.githubusercontent.com/ginseng666/GeoJSON-TopoJSON-Austria/master/2017/simplified-99.9/bezirke_999_geo.json')
    
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
    austria_data_join <<- data_cur %>%
        inner_join(data_past, by = "code", suffix=c('', '_past')) %>%
        mutate(n = date-date_past) 
    austria_pal <<- colorBin("YlOrRd", bins = c(0, 1, 25, 50, 75, 99, 100))
    austria_legendlabs <<- c("< 1", " 1-25", "25-50", "50-75", "75-99", "> 99" , "No or missing data")
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

getDataCzech <- function(){
  czech_geom <<- st_read('map_data/distictsCzechiaLow.json') 
  czech_geom <- czech_geom %>% select(name, geometry) %>% mutate(name = as.character(name))
  czech_pop <- read.csv('map_data/czech_pop.csv',stringsAsFactors = F) %>%
   select(code = Code, name = District, pop = Population)
   
  czechData <- read.csv('https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/kraj-okres-nakazeni-vyleceni-umrti.csv')
  
  names(czechData) <- c('Date','Code','District','Confirmed','Cure','Death')
  czechData$Date <- as.Date(czechData$Date)
  czechData = czechData %>% 
    group_by(District) %>% 
    slice(c(n()-14, n())) %>% 
    summarize(cases = Confirmed[2]-Confirmed[1], code = first(District), date = last(Date)) %>% 
    ungroup
  
  czech_data_join <<- czechData %>%  inner_join(czech_pop, by = c("code")) 
}


maplabsCzech <- function(riskData) {
  riskData <- riskData %>%
    mutate(risk = case_when(
      risk == 100 ~ '> 99',
      risk == 0 ~ '< 1',
      is.na(risk) ~ 'No data',
      TRUE ~ as.character(risk)
    ))
  labels <- paste0(
    "<strong>", paste0('District of ', riskData$name), "</strong><br/>",
    "Current Risk Level: <b>",riskData$risk, ifelse(riskData$risk == "No data", "", "&#37;"),"</b><br/>",
    "Latest Update: ", substr(riskData$date, 1, 10)
  ) %>% lapply(htmltools::HTML)
  return(labels)
}

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

  swedenData = swedenResource %>% 
    pivot_longer(3:23, names_to="County", values_to="cases") %>%
    select(-Totalt_antal_fall) %>%
    arrange(desc(date))

  sweden_geom <<- st_read("map_data/sweden-counties.geojson")
  sweden_pop <<- read.csv("map_data/sweden_pop.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8")

  data_cur <- swedenData %>%
    group_by(County) %>%
    summarise(County = first(County), cases = sum(cases), date = first(date)) %>%
    as.data.frame()
  past_date <- data_cur$date[1] - 14
  data_past <- swedenData %>%
    filter(date <= past_date) %>%
    group_by(County) %>%
    summarise(County = first(County), cases = sum(cases), date = first(date)) %>%
    as.data.frame()
  sweden_data_join <<- data_cur %>%
    inner_join(data_past, by = "County", suffix = c("", "_past")) %>%
    inner_join(sweden_pop, by = c("County")) 
  sweden_pal <<- colorBin("YlOrRd", bins = c(0, 1, 25, 50, 75, 99, 100))
  sweden_legendlabs <<- c("< 1", " 1-25", "25-50", "50-75", "75-99", "> 99", "No or missing data")
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


getDataDenmark <- function(){
  geomDanish <- st_read('map_data/denmark-municipalities.geojson')
  
  geomDanish$name <- as.character(gsub(" Kommune","",geomDanish$name))
  
  # adjust the names of municipalities in geojson file so that they'll match with out CURRENT variables
  geomDanish[which(geomDanish$name == "Bornholms Regionskommune"),'name'] <- "Bornholm"
  geomDanish[which(geomDanish$name == "Københavns"),'name'] <- "Copenhagen"
  
  denmark_geom <<- geomDanish

  webpages<-read_html("https://covid19.ssi.dk/overvagningsdata/download-fil-med-overvaagningdata")
  #extract the html blocks which are h5 and contain links
  JAM = webpages %>% html_nodes("h5") %>% html_nodes("a")
  #subset this list by looking for items which are in the directory of interest
  INDEX = which(grepl("https://files.ssi.dk/covid19/overvagning/data/",JAM,fixed=TRUE))
  
  #JAM[2] should be the download link -- unless the website changes...Not sure if there is an easy way to double check this is the right code block?
  #split the string to find the link using \"
  DOWNLOADLINK = strsplit(as.character(JAM[INDEX[1]]),"\"")[[1]][2]
  DOWNLOADLINK = paste0(DOWNLOADLINK,".zip")  #need to add .zip extension in order for the download/extraction process to perform correctly in R.
  #Have the download link!
  
  # 2.) download and extract data:
  temp <- tempfile() #temporary file for download
  temp2 <- tempfile()#temporary file for extraction
  download.file(DOWNLOADLINK,temp)
  unzip(zipfile = temp, exdir = temp2)
  DanishData  <- read.csv(file.path(temp2, "Municipality_cases_time_series.csv"),sep=";",encoding="UTF-8", stringsAsFactors = F)
  unlink(temp)
  unlink(temp2)
  
  DanishCounty <- names(DanishData)[2:length(names(DanishData))]
  DanishData$date_sample <- as.Date(DanishData$SampleDate)
  getDanishData <- function(code){
    subdata <- DanishData[,c("date_sample",DanishCounty[code])]
    subdata$CumCases <- cumsum(subdata[,DanishCounty[code]])
    x <- length(subdata$date_sample)
    difference <- round((subdata[x,'CumCases'] - subdata[x-14,'CumCases'])*10/14)
    vec <- data.frame(Municipality = DanishCounty[code], Date = subdata$date_sample[x], Difference = difference)
    return(vec)
  }
  
  dataTable <- data.frame(Municipality = as.character(), Date = as.character(), Difference = as.numeric())
  for (i in 1:length(DanishCounty)){
    vec <- getDanishData(i)
    dataTable <- rbind(dataTable,vec)
  }
  dataTable <- dataTable %>% mutate(Municipality = as.character(Municipality), Date = as.Date(Date))
  DanishPop <- as.data.frame(read.csv('map_data/denmark_pop.csv', encoding="UTF-8", stringsAsFactors = F)) ## get from Statistics Denmark: https://www.statbank.dk/statbank5a/SelectVarVal/saveselections.asp
  names(DanishPop) <- c("Municipality",'Population')
  
  # make the population column as numeric
  DanishPop$Population <- as.numeric(gsub(" ","",DanishPop$Population))
  
  # adjust some municipalities' names so that they match with population file
  dataTable$Municipality[which(dataTable$Municipality == "Høje.Taastrup")] <-  "Høje-Taastrup"
  dataTable$Municipality[which(dataTable$Municipality == "Faaborg.Midtfyn")] <-  "Faaborg-Midtfyn"
  dataTable$Municipality[which(dataTable$Municipality == "Lyngby.Taarbæk")] <-  "Lyngby-Taarbæk"
  dataTable$Municipality[which(dataTable$Municipality == "Ringkøbing.Skjern")] <-  "Ringkøbing-Skjern"
  dataTable$Municipality[which(dataTable$Municipality == "Ikast.Brande")] <-  "Ikast-Brande"
  
  denmark_data_join <<- inner_join(dataTable,DanishPop, by = 'Municipality') %>% rename(name = Municipality, date = Date, difference = Difference, pop = Population)
}

maplabsDenmark <- function(riskData) {
  riskData <- riskData %>%
    mutate(risk = case_when(
      risk == 100 ~ '> 99',
      risk == 0 ~ '< 1',
      is.na(risk) ~ 'No data',
      TRUE ~ as.character(risk)
    ))
  labels <- paste0(
    "<strong>", paste0('Municipality of ', riskData$name), "</strong><br/>",
    "Current Risk Level: <b>",riskData$risk, ifelse(riskData$risk == "No data", "", "&#37;"),"</b><br/>",
    "Latest Update: ", substr(riskData$date, 1, 10)
  ) %>% lapply(htmltools::HTML)
  return(labels)
}

getDataIreland <- function() {
    
    ireland_geom <<- st_read('map_data/Ireland_Counties.geojson')

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

    ireland_data_join <<- inner_join(data_cur, data_past, by = "CountyName", suffix=c('', '_past'))
    ireland_pal <<- colorBin("YlOrRd", bins = c(0, 1, 25, 50, 75, 99, 100))
    ireland_legendlabs <<- c("< 1", " 1-25", "25-50", "50-75", "75-99", "> 99" , "No or missing data")
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
        "<strong>County ", riskData$id, "</strong><br/>",
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
event_size = c(10, 15, 20, 25, 50, 100, 500, 1000, 5000)
asc_bias_list <<- c(3, 5)
europe <<- st_read('map_data/european-selected-countries.geojson') 
getDataUK()
getDataSwiss()
getDataItaly()
getDataAustria()
getDataFrance()
getDataSpain()
getDataCzech()
getDataSweden()
getDataDenmark()
getDataIreland()

scale_factor = 10/14

for (asc_bias in asc_bias_list) {


  uk_data_Nr <- uk_data_join %>% mutate(Nr = (cases - cases_past) * asc_bias * scale_factor)
  italy_data_Nr <- italy_data_join %>% mutate(Nr = (cases - cases_past) * asc_bias * scale_factor)
  swiss_data_Nr <- swiss_data_join %>% mutate(Nr = (cases - cases_past) * asc_bias * scale_factor)
  france_data_Nr <- france_data_join %>% mutate(Nr = (cases - cases_past) * asc_bias * scale_factor)
  austria_data_Nr <- austria_data_join %>% mutate(Nr = (cases - cases_past) * asc_bias * scale_factor)
  spain_data_Nr <- spain_data_join %>% mutate(Nr = (cases - cases_past) * asc_bias * scale_factor) 
  czech_data_Nr <- czech_data_join %>% mutate(Nr = cases * asc_bias * scale_factor) 
  sweden_data_Nr <- sweden_data_join %>% mutate(Nr = (cases - cases_past) * asc_bias * scale_factor) 
  denmark_data_Nr <- denmark_data_join %>% mutate(Nr = difference * asc_bias * scale_factor) 
  ireland_data_Nr <- ireland_data_join %>% mutate(Nr = (cases - cases_past) * asc_bias * scale_factor) 

  for (size in event_size){
    uk_riskdt <- uk_data_Nr %>%
      mutate(risk = if_else(Nr > 10, round(calc_risk(Nr, size, pop)), 0))

    uk_riskdt_map <- uk_geom %>% left_join(uk_riskdt, by = c("code"))


    italy_riskdt <- italy_data_Nr %>%
      mutate(risk = if_else(Nr > 10, round(calc_risk(Nr, size, pop)), 0))

    italy_riskdt_map <- italy_geom %>% left_join(italy_riskdt, by = c("prov_istat_code_num" = "code"))


    swiss_riskdt <- swiss_data_Nr %>%
      mutate(risk = if_else(Nr > 10, round(calc_risk(Nr, size, pop)), 0))

    swiss_riskdt_map <- swiss_geom %>% left_join(swiss_riskdt, by = c("id" = "code"))

    france_riskdt <- france_data_Nr %>%
      mutate(risk = if_else(Nr > 10, round(calc_risk(Nr, size, pop)), 0))

    france_riskdt_map <- france_geom %>% left_join(france_riskdt, by = c("code"))

    austria_riskdt <- austria_data_Nr %>%
      mutate(risk = if_else(Nr > 10, round(calc_risk(Nr, size, pop)), 0))

    austria_riskdt_map <- austria_geom %>% left_join(austria_riskdt, by = c("iso" = "code"))
    
    spain_riskdt <- spain_data_Nr %>% 
      mutate(risk = if_else(Nr > 10, round(calc_risk(Nr, size, pop)), 0))
    
    spain_riskdt_map <- spain_geom %>% left_join(spain_riskdt, by = "name") 
    
    czech_riskdt <- czech_data_Nr %>% 
      mutate(risk = if_else(Nr > 10, round(calc_risk(Nr, size, pop)), 0))
    
    czech_riskdt_map <- czech_geom %>% left_join(czech_riskdt, by = "name")

    sweden_riskdt <- sweden_data_Nr %>% 
      mutate(risk = if_else(Nr > 10, round(calc_risk(Nr, size, Population)), 0))
    
    sweden_riskdt_map <- sweden_geom %>% left_join(sweden_riskdt, by = c("name" = "County")) 

    denmark_riskdt <- denmark_data_Nr %>%
      mutate(risk = if_else(Nr > 10, round(calc_risk(Nr, size, pop)), 0))
    
    denmark_riskdt_map <- denmark_geom %>% left_join(denmark_riskdt, by = "name")

    ireland_riskdt <- ireland_data_Nr %>%
      mutate(risk = if_else(Nr > 10, round(calc_risk(Nr, size, pop)), 0))
    
    ireland_riskdt_map <- ireland_geom %>% left_join(ireland_riskdt, by = c("id" = "CountyName")) 




    map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lat = 48.6, lng = 7.17, zoom = 4) %>%
      # fitBounds(7.5, 47.5, 9, 46) %>%
      addPolygons(
        data = europe, 
        fill = FALSE, color = "#943b29", weight = 2.5, smoothFactor = 0.5,
        opacity = 1.0
      ) %>%
      addPolygons(
        data = swiss_riskdt_map,
        color = "#444444", weight = 0.2, smoothFactor = 0.1,
        opacity = 1.0, fillOpacity = 0.7,
        fillColor = ~ swiss_pal(risk),
        highlight = highlightOptions(weight = 1),
        label = maplabsSwiss(swiss_riskdt_map)
      ) %>%
      addPolygons(
        data = uk_riskdt_map,
        color = "#444444", weight = 0.2, smoothFactor = 0.1,
        opacity = 1.0, fillOpacity = 0.7,
        fillColor = ~ uk_pal(risk),
        highlight = highlightOptions(weight = 1),
        label = maplabsUK(uk_riskdt_map)
      ) %>%
      addPolygons(
        data = italy_riskdt_map,
        color = "#444444", weight = 0.2, smoothFactor = 0.1,
        opacity = 1.0, fillOpacity = 0.7,
        fillColor = ~ italy_pal(risk),
        highlight = highlightOptions(weight = 1),
        label = maplabsItaly(italy_riskdt_map)
      ) %>%
      addPolygons(
        data = france_riskdt_map,
        color = "#444444", weight = 0.2, smoothFactor = 0.1,
        opacity = 1.0, fillOpacity = 0.7,
        fillColor = ~ france_pal(risk),
        highlight = highlightOptions(weight = 1),
        label = maplabsFrance(france_riskdt_map)
      ) %>%
      addPolygons(
        data = austria_riskdt_map,
        color = "#444444", weight = 0.2, smoothFactor = 0.1,
        opacity = 1.0, fillOpacity = 0.7,
        fillColor = ~ austria_pal(risk),
        highlight = highlightOptions(weight = 1),
        label = maplabsAustria(austria_riskdt_map)
      ) %>%
      addPolygons(
        data = spain_riskdt_map,
        color = "#444444", weight = 0.2, smoothFactor = 0.1,
        opacity = 1.0, fillOpacity = 0.7,
        fillColor = ~ austria_pal(risk),
        highlight = highlightOptions(weight = 1),
        label = maplabsSpain(spain_riskdt_map)
      ) %>%
      addPolygons(
        data = czech_riskdt_map,
        color = "#444444", weight = 0.2, smoothFactor = 0.1,
        opacity = 1.0, fillOpacity = 0.7,
        fillColor = ~ austria_pal(risk),
        highlight = highlightOptions(weight = 1),
        label = maplabsCzech(czech_riskdt_map)
      ) %>%
      addPolygons(
        data = sweden_riskdt_map,
        color = "#444444", weight = 0.2, smoothFactor = 0.1,
        opacity = 1.0, fillOpacity = 0.7,
        fillColor = ~ austria_pal(risk),
        highlight = highlightOptions(weight = 1),
        label = maplabsSweden(sweden_riskdt_map)
      ) %>%
      addPolygons(
        data = denmark_riskdt_map,
        color = "#444444", weight = 0.2, smoothFactor = 0.1,
        opacity = 1.0, fillOpacity = 0.7,
        fillColor = ~ austria_pal(risk),
        highlight = highlightOptions(weight = 1),
        label = maplabsDenmark(denmark_riskdt_map)
      )  %>%
      addPolygons(
        data = ireland_riskdt_map,
        color = "#444444", weight = 0.2, smoothFactor = 0.1,
        opacity = 1.0, fillOpacity = 0.7,
        fillColor = ~ austria_pal(risk),
        highlight = highlightOptions(weight = 1),
        label = maplabsIreland(ireland_riskdt_map)
      ) %>%
      addEasyButton(easyButton(
        icon = "fa-crosshairs fa-lg", title = "Locate Me",
        onClick = JS("function(btn, map){ map.locate({setView: true, maxZoom: 7});}")
      )) %>%
      addLegend(
        data = uk_riskdt_map,
        position = "topright", pal = uk_pal, values = ~risk,
        title = "Risk Level (%)",
        opacity = 0.7,
        labFormat = function(type, cuts, p) {
          paste0(uk_legendlabs)
        }
      )
    map$dependencies[[1]]$src[1] <- "/srv/shiny-server/map_data/"
    mapshot(map, url = file.path(getwd(), "www", paste0("eu_", asc_bias, "_", size, ".html")))
  }
}
