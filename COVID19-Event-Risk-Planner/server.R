## ---------------------------
##
## COVID19 Event Risk Assessment Planning tool
##
## Aroon Chande <mail@aroonchande.com> <achande@ihrc.com>
## Lavanya Rishsishwar <lrishishwar@ihrc.com>
## Model from Joshua Weitz
## See: https://github.com/jsweitz/covid-19-ga-summer-2020
## ---------------------------
options(scipen = 999)

get_data()
shinyServer(function(input, output, session) {

  rupture(ms = 600000, html = timeout)

  observeEvent(input$ruptured, {
    session$close()
  })

  source("R/server/navigation.R", local = T)
  source("R/server/usa-map-reactivity.R", local = T)
  source("R/server/global-map-reactivity.R", local = T)
  source("R/server/usa-daily-plots.R", local = T)
  source("R/server/usa-real-time-plots.R", local = T)
  source("R/server/risk-game-reactivity.R", local = T)
})
