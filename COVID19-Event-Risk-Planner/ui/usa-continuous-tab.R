usa_continuous_tab = tabPanel(
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
)