## ---------------------------
##
## COVID19 Event Risk Assessment Planning tool
##
## Aroon Chande <mail@aroonchande.com> <achande@ihrc.com>
## Lavanya Rishsishwar <lrishishwar@ihrc.com>
## Model from Joshua Weitz
## See: https://github.com/jsweitz/covid-19-ga-summer-2020
## ---------------------------
# options(shiny.reactlog = TRUE)
options(scipen = 999)
# library(mapview, lib.loc = "/projects/covid19/covid19/R/x86_64-redhat-linux-gnu-library/3.6/")

Sys.setenv(PATH = with_path("/projects/covid19/bin", Sys.getenv("PATH")))

regions <- c(
  "USA, Alphabetical" = "states-alpha.png",
  "USA, By Rank" = "states-rank.png",
  "AK" = "AK.png", "AL" = "AL.png", "AR" = "AR.png", "AZ" = "AZ.png",
  "CA" = "CA.png", "CO" = "CO.png", "CT" = "CT.png", "DC" = "DC.png",
  "DE" = "DE.png", "FL" = "FL.png", "GA" = "GA.png", "HI" = "HI.png",
  "IA" = "IA.png", "ID" = "ID.png", "IL" = "IL.png", "IN" = "IN.png",
  "KS" = "KS.png", "KY" = "KY.png", "LA" = "LA.png", "MA" = "MA.png",
  "MD" = "MD.png", "ME" = "ME.png", "MI" = "MI.png", "MN" = "MN.png",
  "MO" = "MO.png", "MS" = "MS.png", "MT" = "MT.png", "NC" = "NC.png",
  "ND" = "ND.png", "NE" = "NE.png", "NH" = "NH.png", "NJ" = "NJ.png",
  "NM" = "NM.png", "NV" = "NV.png", "NY" = "NY.png", "OH" = "OH.png",
  "OK" = "OK.png", "OR" = "OR.png", "PA" = "PA.png", "PR" = "PR.png",
  "RI" = "RI.png", "SC" = "SC.png", "SD" = "SD.png", "TN" = "TN.png",
  "TX" = "TX.png", "UT" = "UT.png", "VA" = "VA.png", "VT" = "VT.png",
  "WA" = "WA.png", "WI" = "WI.png", "WV" = "WV.png", "WY" = "WY.png"
)

pcrit <- function(x) {
  0.01 / x
}

calc_risk <- function(I, n, USpop, scaling_factor=10/14) {
  p_I <- (I / USpop) * scaling_factor
  r <- 1 - (1 - p_I)**n
  round(100 * r, 1)
}

roundUpNice <- function(x, nice = c(1, 2, 4, 5, 6, 8, 10)) {
  if (length(x) != 1) stop("'x' must be of length 1")
  10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}

name2abbr = setNames(state.abb, state.name)
abbr2name = setNames(state.name, state.abb)

get_data <- function() {
  current_fh <- tail(list.files("states_current/", full.names = TRUE), 1)
  current_time <<- gsub(".csv", "", basename(current_fh))
  daily_fh <- tail(list.files("states_daily/", full.names = TRUE), 1)
  daily_time <<- gsub(".csv", "", basename(daily_fh))
  state_data <<- read.csv(current_fh, stringsAsFactors = F)
  states <<- unique(state_data$state)
  current_time <<- daily_time <<- Sys.Date()
  cur_date <- ymd(Sys.Date())-1
  past_date <- ymd(cur_date) - 14
  states_current <<- subset(state_data, ymd(date) == cur_date) %>% arrange(state)
  states_historic <<- subset(state_data, ymd(date) == past_date) %>% arrange(state)
  state_pops <<- read.delim("state_pops.tsv", header = T, sep = "\t", stringsAsFactors = F)
  state_data <<- states_current %>%
    select(state, cases) %>%
    arrange(state)
  state_data$C_i <<- round((states_current$cases - states_historic$cases)  * 10 / 14)
  state_data$state <<- name2abbr[state_data$state]
  state_data <<- state_data %>% tidyr::drop_na()
}


disconnected <- sever_default(title = "Session disconnected",
    subtitle = "Your session disconnected for some reason :(",
    button = "Reconnect",
    button_class = "warning"
    )
timeout <- sever_default(title = "Session timeout reached",
    subtitle = "Your session ended due to inactivity",
    button = "Reconnect",
    button_class = "warning"
    )


shinyServer(function(input, output, session) {
  output$lat <- renderPrint({
    input$lat
  })

  output$long <- renderPrint({
    input$long
  })

  output$geolocation <- renderPrint({
    input$geolocation
  })
  rupture(ms = 600000, html=timeout)
  observeEvent(input$ruptured, {
    session$close()
    })

  source('server/navigation.R', local=T)
  source('server/usa-map-reactivity.R', local=T)
  source('server/global-map-reactivity.R', local=T)
  source('server/usa-daily-plots.R', local=T)
  source('server/usa-real-time-plots.R', local=T)
  # source('server/risk-game-reactivity.R', local=T)
  pal <- colorBin("YlOrRd", bins = c(0.001, 1, 25, 50, 75, 99, 100))
  legendlabs <- c("< 1", " 1-25", "25-50", "50-75", "75-99", "> 99", "No or missing data")
  output$usa_map <- renderLeaflet({

    risk_data = usa_counties %>%
      select(
        GEOID,
        NAME,
        stname,
        pct_fully_vacc,
        updated,
        risk := glue::glue("{input$asc_bias}_{input$event_size_map}"),
        imOp,
        geometry
      )


    basemap = leaflet(options = leafletOptions(worldCopyJump = F, preferCanvas = TRUE)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lat = 37.1,
              lng = -95.7,
              zoom = 4)

    if (input$immShow) {
      basemap %>%
        addPolygons(
          data = risk_data,
          color = "#444444",
          weight = 0.2,
          smoothFactor = 0.1,
          opacity = 1.0,
          fillOpacity = 0.7,
          fillColor = ~ pal(risk),
          highlight = highlightOptions(weight = 1)
        ) %>%
        addPolygons(
          data = risk_data,
          weight = 0,
          fillColor = "white",
          fillOpacity = ~ imOp,
          smoothFactor = 0.1,
          label = maplabs(risk_data)
        ) %>%
        addLegend(
          data = risk_data,
          position = "topright",
          pal = pal,
          values = ~ risk,
          title = "Risk Level (%)",
          opacity = .7,
          labFormat = function(type, cuts, p) {
            paste0(legendlabs)
          }
        ) %>%
        addEasyButton(easyButton(
          icon = "fa-crosshairs fa-lg",
          title = "Locate Me",
          onClick = JS(
            "function(btn, map){ map.locate({setView: true, maxZoom: 7});}"
          )
        ))

    } else {
      basemap %>%
        addPolygons(
          data = risk_data,
          color = "#444444",
          weight = 0.2,
          smoothFactor = 0.1,
          opacity = 1.0,
          fillOpacity = 0.7,
          fillColor = ~ pal(risk),
          highlight = highlightOptions(weight = 1),
          label = maplabs(risk_data)
        ) %>%
        addLegend(
          data = risk_data,
          position = "topright",
          pal = pal,
          values = ~ risk,
          title = "Risk Level (%)",
          opacity = .7,
          labFormat = function(type, cuts, p) {
            paste0(legendlabs)
          }
        ) %>%
        addEasyButton(easyButton(
          icon = "fa-crosshairs fa-lg",
          title = "Locate Me",
          onClick = JS(
            "function(btn, map){ map.locate({setView: true, maxZoom: 7});}"
          )
        ))

    }

  })





})
