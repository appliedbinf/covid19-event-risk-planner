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

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("COVID-19 Event Risk Assessment Planning tool"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("event_size",
                        "Event size:",
                        min = 1,
                        max = 100000,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            
        )
    )
))
