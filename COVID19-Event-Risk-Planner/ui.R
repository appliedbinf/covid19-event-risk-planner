## ---------------------------
##
## COVID19 Event Risk Assessment Planning tool
##
## Aroon Chande <mail@aroonchande.com> <achande@ihrc.com>
## Lavanya Rishsishwar <lrishishwar@ihrc.com>
## Model from Joshua Weitz
## See: https://github.com/jsweitz/covid-19-ga-summer-2020
## ---------------------------
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(leaflet)
library(sever)
library(shinypanels)
options(scipen = 999)

source("R/ui/usa-map-tab.R", local = T)
source("R/ui/risk-game-tab.R", local = T)
source("R/ui/global-map-tab.R", local = T)
source("R/ui/usa-real-time-tab.R", local = T)
source("R/ui/tutorial-tab.R", local = T)
source("R/ui/about-tabset.R", local = T)


shinyUI(
  navbarPage(
    theme = shinytheme("sandstone"),
    shinyjs::useShinyjs(),
    use_sever(),
    selected = "usa",
    collapsible = TRUE,
    id = "nav-page",
    windowTitle = "COVID-19 Event Risk Assessment Planning Tool",
    title = "COVID-19 Event Risk Assessment Planning Tool",
    header = NAVPAGE_HEADER,
    footer = NAVPAGE_FOOTER,
    usa_map_tab,
    risk_game_tab,
    global_map_tab,
    usa_real_time_tab,
    tutorial_tab,
    about_tabset
  )
)
