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
options(scipen = 999)
shinyUI(fluidPage(
    # tags$head(tags$script(src="format_numbers.js")),
    # Application title
    titlePanel( "COVID-19 Event Risk Assessment Planning tool"),
    tabsetPanel(tabPanel(id="Prediction", "Explore US and State-level prediction",
        fluid = TRUE,
        sidebarLayout(
            sidebarPanel(
                textInput("event_size_us",
                          "Event size:",
                          value = 275),
                textInput("infect_us",
                          "Number of circulating infections:",
                          value = 200000),
                checkboxInput("use_state", label = "Limit prediction to state level?"),
                conditionalPanel(condition = "input.use_state",
                                 selectizeInput("us_states", "Select state", c())
                                 ),
                actionButton("calc_us", label = "What is the risk?")
            ),
            
            mainPanel(
                # verbatimTextOutput("values"),br(),
                plotOutput(
                "plot_us", width = "800px", height = "800px"
            ))
        )
    ),
    tabPanel(id="Data-driven", "Real-time US and State predictions",
        fluid = TRUE,
        sidebarLayout(
            sidebarPanel(
                p("The horizontal dotted lines with risk predictions are based on real-time COVID19 surveilance data.  
                  They represent, from bottom up, predictions given the current reported incidence, 5 times the current incidence, and 10 times the current incidence.  
                  These predictions help understand the effects of potential under-testing and reporting of COVID19 incidence"),
                checkboxInput("use_state_dd", label = "Limit prediction to state level?", value = TRUE),
                conditionalPanel(condition = "input.use_state_dd",
                                 selectizeInput("states_dd", "Select state", c())
                ),
                textInput("event_dd",
                          "Event size:",
                          value = 275),
                textInput("infect_dd",
                          "Number of circulating infection:",
                          value = 200000),
                actionButton("calc_dd", label = "What is the risk?")
            ),
            
            mainPanel(
                # verbatimTextOutput("values_dd"),br(),
                plotOutput(
                "plot_dd", width = "800px", height = "800px"
            ))
        )
    )
    )
    
))
