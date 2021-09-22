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
  usa_counties <<- vroom::vroom('www/usa_risk_counties.csv') %>%
    select(-NAME, -stname) %>%
    mutate_at(vars(-GEOID, -state, -updated), as.numeric)
  usa_counties <<- county_geom %>% left_join(usa_counties, by = c("GEOID" = "GEOID"))
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

  rupture(ms = 600000, html=timeout)

  observeEvent(input$ruptured, {
    session$close()
    })

  source('server/navigation.R', local=T)
  source('server/usa-map-reactivity.R', local=T)
  source('server/global-map-reactivity.R', local=T)
  source('server/usa-daily-plots.R', local=T)
  source('server/usa-real-time-plots.R', local=T)
  source('server/risk-game-reactivity.R', local=T)

})
