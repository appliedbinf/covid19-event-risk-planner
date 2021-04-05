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

getDataDenmark <- function(){
  geomDanish <- st_read('map_data/denmark-municipalities.geojson')
  
  geomDanish$name <- as.character(gsub(" Kommune","",geomDanish$name))
  
  # adjust the names of municipalities in geojson file so that they'll match with out CURRENT variables
  geomDanish[which(geomDanish$name == "Bornholms Regionskommune"),'name'] <- "Bornholm"
  geomDanish[which(geomDanish$name == "Københavns"),'name'] <- "Copenhagen"
  
  geom <<- geomDanish

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
  DanishData$date_sample <- as.Date(DanishData$date_sample)
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
  pal <<- colorBin("YlOrRd", bins = c(0, 1, 25, 50, 75, 99, 100))
  legendlabs <<- c("< 1", " 1-25", "25-50", "50-75", "75-99", "> 99", "No or missing data")
  data_join <<- inner_join(dataTable,DanishPop, by = 'Municipality') %>% rename(name = Municipality, date = Date, difference = Difference, pop = Population)
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


# Calculate risk
calc_risk <- function(I, g, pop) {
    p_I <- I / pop
    r <- 1 - (1 - p_I)**g
    return(round(r*100, 1))
}


######## Create and save daily map widgets ########
size <- 50
asc_bias <- 5


getDataDenmark()

data_Nr <- data_join %>%
mutate(Nr = difference * asc_bias * 10/14) 

riskdt <- data_Nr %>% 
    mutate(risk = if_else(Nr > 10, round(calc_risk(Nr, size, pop)), 0))

riskdt_map <- geom %>% left_join(riskdt, by = "name")
map <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    setView(lat = 56, lng = 9.3, zoom = 7) %>%
    addPolygons(
        data = riskdt_map,
        color = "#444444", weight = 0.2, smoothFactor = 0.1,
        opacity = 1.0, fillOpacity = 0.7,
        fillColor = ~ pal(risk),
        highlight = highlightOptions(weight = 1),
        label = maplabsDenmark(riskdt_map)
    ) %>%
    addLegend(
        data = riskdt_map,
        position = "topright", pal = pal, values = ~risk,
        title = "Risk Level (%)",
        opacity = 0.7,
        labFormat = function(type, cuts, p) {
            paste0(legendlabs)
        })
mapshot(map, file = file.path(getwd(), "daily_risk_map_denmark", current_time, paste0(current_time,"_", asc_bias, "_", size, ".png")))
post_tweet(status = paste0("Danish county-level risk estimate update for ",  now("Europe/Copenhagen"), " ", tz("Europe/Copenhagen"), ".  Estimated risk that at least 1 person is #COVID19 positive for events or other areas where ", size, " individuals are in close contact [Assuming 5:1 ascertainment bias]"),
 media = file.path(getwd(), "daily_risk_map_denmark", current_time, paste0(current_time,"_", asc_bias, "_", size, ".png")))
