usa_real_time_tab <- tabPanel(
  value = "usa-real-time",
  "Real-time US and State-level estimates ",
  fluid = TRUE,
  sidebarLayout(
    sidebarPanel(
      width = 3,
      HTML(
        "<p>The horizontal dotted lines with risk estimates are based on ",
        "real-time COVID19 surveillance data. They represent estimates ",
        "given the current reported incidence [C<sub>I</sub>] ",
        "(<span title='circle' style='color: red'>&#11044;</span>), 5 ",
        "times the current incidence (<span title='triangle' style='color: ",
        "red'>&#9650;</span>), and 10 times the current incidence ",
        "(<span title='square' style='color: red'>&#9632;</span>).",
        "These estimates help understand the effects of potential ",
        "under-testing and reporting of COVID19 incidence.</p>"
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
    mainPanel(
      plotOutput(
        "plot_dd",
        width = "900px", height = "900px"
      )
    )
  )
)
