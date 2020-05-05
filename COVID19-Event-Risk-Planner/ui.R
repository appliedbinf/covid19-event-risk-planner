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
options(scipen = 999)
shinyUI(fluidPage(
    tags$head(includeHTML(("www/ga.html"))),
    # Application title
    titlePanel( titlePanel( div(column(width = 9, h2("COVID-19 Event Risk Assessment Planning tool")), 
        column(width = 3, tags$img(src = "scaled_plot.jpg")))), windowTitle = "COVID-19 Event Risk Assessment Planning tool"),
    tabsetPanel(tabPanel(id="Prediction", "Explore US and State-level prediction",
        fluid = TRUE,
        sidebarLayout(
            sidebarPanel(
                textInput("event_size_us",
                          "Event size:",
                          value = 275),
                textInput("infect_us",
                          "Number of circulating infections:",
                          value = 800000),
                checkboxInput("use_state", label = "Limit prediction to state level?"),
                conditionalPanel(condition = "input.use_state",
                                 selectizeInput("us_states", "Select state", c())
                                 ),
                actionButton("calc_us", label = "What is the risk?"),
                downloadButton('dl_pred', "Download plot")
            ),
            
            mainPanel(
                # verbatimTextOutput("values"),br(),
                plotOutput(
                "plot_us", width = "900px", height = "900px")
                )
        )
    ),
    tabPanel(id="Data-driven", "Real-time US and State estimates",
        fluid = TRUE,
        sidebarLayout(
            sidebarPanel(
                p("The horizontal dotted lines with risk estimates are based on real-time COVID19 surveilance data.  
                  They represent, estimates given the current reported incidence (circle), 5 times the current incidence (triangle), and 10 times the current incidence (square).  
                  These estimates help understand the effects of potential under-testing and reporting of COVID19 incidence"),
                checkboxInput("use_state_dd", label = "Limit prediction to state level?", value = TRUE),
                conditionalPanel(condition = "input.use_state_dd",
                                 selectizeInput("states_dd", "Select state", c())
                ),
                textInput("event_dd",
                          "Event size:",
                          value = 275),
                downloadButton('dl_dd', "Download plot"),
                htmlOutput("dd_text")
            ),
            
            mainPanel(
                # verbatimTextOutput("values_dd"),br(),
                plotOutput(
                "plot_dd", width = "900px", height = "900px")
                )
        )
    ),
    tabPanel(id="previous", "Previously Released Charts",
            fluid = TRUE,
            mainPanel(
              tags$img(src = "twitter_image_031020.jpg"),
              tags$br(),tags$br(),
              tags$img(src = "figevent_checker_apr30.png"),
              tags$br(),tags$br(),
              tags$img(src = "figevent_checker_georgia_042720.jpg  ")    
            )   
            ),
    tabPanel(id="about", "About",
             fluid = TRUE,
             mainPanel(
                 includeMarkdown('About.md')    
             )
    ),
    tabPanel(id="data", "Data source",
               fluid = TRUE,
               mainPanel(
                   includeMarkdown('Data.md')    
               )
    ),
    tabPanel(id="press", "Press",
            fluid = TRUE,
            mainPanel(
                includeMarkdown('Press.md')    
            )
    ),
    tags$div(class="footer",
      align = "center",
      style = "margin-top: 20px;",
      column(width = 5), 
      column(width = 2, tags$img(src = "gt-logo-gold.png")), 
      column(width = 2, tags$img(src = "ABiL-Logo.png")), 
      column(width = 3)
      )
    # ),
    # div(class = "footer",
    #     column(width = 3, tags$img(src = "gt-logo-gold.png")), column(width = 3, tags$img(src = "ABiL-Logo.png"))
    )  
  )
)
