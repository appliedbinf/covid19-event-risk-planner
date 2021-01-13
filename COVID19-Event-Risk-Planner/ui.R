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
options(scipen = 999)


event_size = c(10, 15, 20, 25, 50, 100, 500, 1000, 5000)

shinyUI(fluidPage(
  theme = shinytheme("sandstone"),
  use_sever(),
  tags$head(
    includeHTML(("www/ga.html")),
    includeHTML(("www/mt.html")),
    tags$style(
      ".map-container {
          height: 600px;
          width: 100%;
          position: relative;
        }",
      ".map-loading {
          position: absolute;
          display: flex;
          justify-content: center;
          align-items: center;
          width: 100%;
          height: 600px;
          background-color: #bdbdbd;
          text-align: center;
          color: #FFFFFF;
          font-size: 2em;
        }"
    ),
    HTML('<meta property="og:title" content="COVID-19 Event Risk Assessment Planning Tool" />
<meta property="og:type" content="website" />
<meta property="og:url" content="https://covid19risk.biosci.gatech.edu/" />
<meta property="og:image" content="https://covid19risk.biosci.gatech.edu/og.png" />')
  ),
  # Application title
  titlePanel(titlePanel(
    h2(
      "COVID-19 Event Risk Assessment Planning Tool"
    )
    # column(width = 3, tags$img(src = "scaled_plot.jpg"))
  ), windowTitle = "COVID-19 Event Risk Assessment Planning Tool"),
  tabsetPanel(
    id="maps",
    tabPanel(
      value = "usa",
      title = "USA Risk estimates by county",
      fluid = TRUE,
      sidebarLayout(
        sidebarPanel(
          width = 3,
          HTML(
            paste0(
              "<p>This map shows the risk level of attending an event, given the event size and location.",
              "<br/><br/>You can reduce the risk that one case becomes many by wearing a mask, distancing, and gathering outdoors in smaller groups<br/><br/>",
              "The risk level is the estimated chance (0-100%) that at least 1 COVID-19 positive individual will be present at an event in a county, given the size of the event.",
              "<br/><br/>", "Based on seroprevalence data and increases in testing, by default we assume there are three times more cases than are being reported (3:1 ascertainment bias). In places with less testing availability, that bias may be higher. We are evaluating the inclusion of lower ascertainment biases based on increased testing.",
              "<br/><br/>",
              "Choose an event size and ascertainment bias below.</p>"
            )
          ),
          actionLink("to_data", "See our data sources"),
          shinyWidgets::sliderTextInput(
            "event_size_map",
            "Event Size: ",
            choices = event_size,
            selected = 50,
            grid = T
          ),
          shinyWidgets::awesomeRadio(
            inputId = "asc_bias",
            label = "Select Ascertainment Bias",
            choices = c("3", "5"),
            selected = "3",
            status = "warning",
            inline = T
          )
        ),
        mainPanel(
          fluidRow(column(
            10,
            htmlOutput("map_static")
            # ),
          )),
          HTML(
            "<p>(Note: This map uses a Web Mercator projection that inflates the area of states in northern latitudes. County boundaries are generalized for faster drawing.)</p>"
          ),
          fluidRow(
            align="center",
            column(10,
            shinyWidgets::actionBttn("to_global", label="Explore global risk estimates", style="jelly", color="success", size="sm")
            ))
        )
      )
    ),
    tabPanel(
      value = "global",
      title = "Global Risk Estimates",
      fluid = TRUE,
      sidebarLayout(
        sidebarPanel(
          width = 3,
          HTML(
            paste0(
              "<p>This map shows the risk level of attending events of different sizes at within-country resolution.",
              "<br/><br/>You can reduce the risk that one case becomes many by wearing a mask, distancing, and gathering outdoors in smaller groups<br/><br/>",
              "The risk level is the estimated chance (0-100%) that at least 1 COVID-19 positive individual will be present at an event in a NUTS-3 level area (County, Local Authority, Council, District), given the size of the event.",
              "<br/><br/>", "Based on seroprevalence data and increases in testing, by default we assume there are three times more cases than are being reported (3:1 ascertainment bias). In places with less testing availability, that bias may be higher. We are evaluating the inclusion of lower ascertainment biases based on increased testing.",
              "<br/><br/>",
              "Choose an event size and ascertainment bias below.</p>"
            )
          ),
          actionLink("to_data_global", "See our data sources"),
          shinyWidgets::sliderTextInput(
            "global_event_size_map",
            "Event Size: ",
            choices = event_size,
            selected = 50,
            grid = T
          ),
          shinyWidgets::awesomeRadio(
            inputId = "global_asc_bias",
            label = "Select Ascertainment Bias",
            choices = c("3", "5"),
            selected = "3",
            status = "warning",
            inline = T
          )
        ),
        mainPanel(
          fluidRow(column(
            10,
            htmlOutput("eu_map_static", width = "331px", height = "744px")
          )),
           HTML(
            "<p>(Note: This map uses a Web Mercator projection that inflates the area of states in northern latitudes. County boundaries are generalized for faster drawing.)</p>"
          ),
          fluidRow(
            align="center",
            column(10,
            shinyWidgets::actionBttn("to_usa", label="Explore US risk estimates", style="jelly", color="success", size="sm")
            ))
        )
      )
    ),
    tabPanel(
      value = "usa-real-time",
      "Real-time US and State-level estimates ",
      # 
      fluid = TRUE,
      sidebarLayout(
        sidebarPanel(
          width=3,
          HTML(
            "<p>The horizontal dotted lines with risk estimates are based on real-time COVID19 surveillance data.
                  They represent estimates given the current reported incidence [C<sub>I</sub>] (<span title='circle' style='color: red'>&#11044;</span>), 5 times the current incidence (<span title='triangle' style='color: red'>&#9650;</span>), and 10 times the current incidence (<span title='square' style='color: red'>&#9632;</span>).
                  These estimates help understand the effects of potential under-testing and reporting of COVID19 incidence.</p>"
          ),
          htmlOutput("dd_current_data"),
          checkboxInput("use_state_dd", label = "Limit prediction to state level?", value = TRUE),
          conditionalPanel(
            condition = "input.use_state_dd",
            selectizeInput("states_dd", "Select state", c())
          ),
          textInput("event_dd",
            "Event size:",
            placeholder = 275
          ),
          downloadButton("dl_dd", "Download plot"),
          htmlOutput("dd_text")
        ),

        mainPanel( # verbatimTextOutput("values_dd"),br(),
          plotOutput(
            "plot_dd",
            width = "900px", height = "900px"
          )
        )
      )
    ),
    tabPanel(
      value = "usa-continuous",
      "USA Continuous risk estimates",
      fluid = TRUE,
      sidebarLayout(
        sidebarPanel(
          width = 3,
          HTML(
            "<p>The curved lines (risk estimates) are based on real-time COVID19 surveillance data.
                  They represent estimates given the current reported incidence (dashed line) [C<sub>I</sub>]: 5x the current incidence (blue), 10x (yellow), and 20x (red).
                  These estimates help understand the effects of potential under-testing and reporting of COVID19 incidence.</p>
                  <p>Select from a mosiac of all 50 states, ordered alphabetically or by their population-adjusted incidence, or zoom in to individual states.</p>"
          ),
          selectizeInput("regions", "Select region", c()),
          selectizeInput("date", "Select a date to view", c()),
          p(
            "Estimates are updated every day at midnight and 12:00 (timezone=America/New_York)"
          ),
          downloadButton("dl_risk", "Download plot")
        ),
        mainPanel(plotOutput(
          "risk_plots",
          width = "900px", height = "900px"
        ))
      )
    ),
    tabPanel(
      id = "tuts",
      "Tutorial",
      fluid = TRUE,
      mainPanel(includeMarkdown("Tutorial.md"))
    ),
    tabPanel(
      value = "about",
      "About",
      fluid = TRUE,
      tabsetPanel(
        id="abouttabs",
        tabPanel(
          value = "Aboutcontent",
          "About",
          fluid = TRUE,
          mainPanel(includeMarkdown("About.md"))
        ),
        tabPanel(
          value = "press",
          "Press",
          fluid = TRUE,
          mainPanel(includeMarkdown("Press.md"))
        ),
        tabPanel(
          value = "data",
          "Data source",
          fluid = TRUE,
          mainPanel(includeMarkdown("Data.md"))
        ),
        tabPanel(
          value = "previous",
          "Previously Released Charts",
          fluid = TRUE,
          mainPanel(
            tags$img(src = "twitter_image_031020.jpg"),
            tags$br(),
            tags$br(),
            tags$img(src = "figevent_checker_apr30.png"),
            tags$br(),
            tags$br(),
            tags$img(src = "figevent_checker_georgia_042720.jpg  ")
          )
        )
      )
    )
  ),
  tags$div(
    class = "footer",
    align = "center",
    style = "padding-top: 10px",
    fluidRow(
      column(3), column(
        7,
        HTML('<div class="well"><p>The COVID-19 Event Risk Assessment Planning Tool is a collaborative project led by <a href="https://ecotheory.biosci.gatech.edu/" rel="noopener" target="_blank">Prof. Joshua Weitz</a> and <a href="http://friendlycities.gatech.edu/" rel="noopener" target="_blank">Prof. Clio Andris</a> at the Georgia Institute of Technology, along with researchers at the <a href="https://www.abil.ihrc.com/" rel="noopener" target="_blank">Applied Bioinformatics Laboratory</a> and <a href="https://knight-hennessy.stanford.edu/program/scholars/2019/mallory-harris" rel="noopener" target="_blank">Stanford University</a>, and powered by <a href="https://rstudio.com/" rel="noopener" target="_blank">RStudio</a>.  Description of the method and analyses available at <a href="https://www.nature.com/articles/s41562-020-01000-9/" rel="noopener" target="_blank">Nature Human Behaviour</a>.</p></div>')
      ), column(1)
    )
  ),
  tags$script(HTML(
    "$(document).on('shiny:inputchanged', function(event) {
        _paq.push(['trackEvent', 'input', 
          'userActivity', event.name, event.value]);
     });"
  )),
))
