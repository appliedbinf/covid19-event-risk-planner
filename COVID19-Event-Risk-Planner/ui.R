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
library(shinyWidgets)
options(scipen=999)
# Define UI for application that draws a histogram
shinyUI(fluidPage(
    tags$head(tags$script(src="format_numbers.js")),
    # Application title
    titlePanel("COVID-19 Event Risk Assessment Planning tool"),

    sidebarLayout(
        sidebarPanel(
            textInput("event_size",
                        "Event size:",
                        value = 275),
            textInput("infect",
                        "Active infections in the US:",
                        value = 200000),
            actionButton("calc", label = "What is the risk?")
        ),

        mainPanel(
            plotOutput("plot", width = "800px",height = "800px")
            
        )
    )
))
