global_map_tab = tabPanel(
  value = "global",
  title = "Global Risk Estimates",
  fluid = TRUE,
  sidebarLayout(
    sidebarPanel(
      width = 3,
      HTML(
        paste0(
          "<p>This map shows the risk level of attending events of different sizes at within-country resolution.",
          "<br/><br/>You can reduce the risk that one case becomes many by wearing a mask, distancing, and gathering outdoors in smaller groups. For vaccinated individuals, preventative steps can reduce the risk of breakthrough infections that spread to vulnerable individuals. For unvaccinated individuals, preventative steps before vaccination can reduce the risk of breakthrough disease, including potentially severe cases, hospitalizations, and fatalities.<br/><br/>",
          "The risk level is the estimated chance (0-100%) that at least 1 COVID-19 positive individual will be present at an event in a NUTS-3 level area (County, Local Authority, Council, District), given the size of the event.",
          "<br/><br/>", "Based on seroprevalence data and increases in testing, by default we assume there are five times more cases than are being reported (5:1 ascertainment bias). In places with less testing availability, that bias may be higher. We are evaluating the inclusion of lower ascertainment biases based on increased testing.",
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
        selected = "5",
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
)
