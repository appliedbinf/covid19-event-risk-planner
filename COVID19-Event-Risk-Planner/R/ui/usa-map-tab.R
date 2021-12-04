usa_map_tab <- tabPanel(
  value = "usa",
  title = "USA Risk estimates by county",
  fluid = TRUE,
  tags$script(
    '
      $(document).on("shiny:sessioninitialized", function() {
        $.ajaxSetup({
            timeout: 1000
        });
        $.get("https://ipinfo.io?token=15dd4534f98729", function(response) {
            Shiny.setInputValue("ip_data", response.ip, {
                priority: "event"
            });
        }, "json").fail(function() {
            Shiny.setInputValue("ip_data", "Unknown", {
                priority: "event"
            });
        });
       });
    '
  ),
  panelsPage(fluidRow(
    panel(
      title = "USA Risk estimates by county",
      can_collapse = F,
      class = "col-sm-12 col-xs-12 col-md-3 well fake-sidebar",
      body = div(
        HTML(
          paste0(
            "<p>This map shows the risk level of attending an event, given the event size and location.",
            "<br/><br/>You can reduce the risk that one case becomes many by wearing a mask, distancing, and gathering outdoors in smaller groups. For vaccinated individuals, preventative steps can reduce the risk of breakthrough infections that spread to vulnerable individuals. For unvaccinated individuals, preventative steps before vaccination can reduce the risk of breakthrough disease, including potentially severe cases, hospitalizations, and fatalities.<br/><br/>",
            "The risk level is the estimated chance (0-100%) that at least 1 COVID-19 positive individual will be present at an event in a county, given the size of the event.",
            "<br/><br/>",
            "Choose an event size and ascertainment bias below</p>"
          )
        ),
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
          choices = c("3", "4", "5"),
          selected = "4",
          status = "warning",
          inline = T
        ),
        shinyWidgets::awesomeRadio(
          inputId = "imm_lvl",
          label = "Focus on states with immunity via full vaccination less than:",
          choices = c(
            "Off" = 0,
            "45%" = 45,
            "50%" = 50,
            "60%" = 60,
            "65%" = 65
          ),
          selected = 0,
          status = "warning",
          inline = T,
        ),
        actionLink("to_data", "See our data sources"),
        HTML(
          paste0(
            "<br/><br/>",
            "Based on seroprevalence data and increases in testing, by default we assume there are four times more cases than are being reported (4:1 ascertainment bias). In places with less testing availability, that bias may be higher. We are evaluating the inclusion of lower ascertainment biases based on increased testing.",
            "<br/><br/>",
            "Higher vaccination levels reduce the risk that exposure to COVID-19 will lead to severe disease and  onward transmission. We show an optional layer representing state-level population immunity via vaccination (allowing for two weeks for individuals completing a vaccination series)."
          )
        )
      )
    ),
    panel(
      class = "col-sm-12 col-md-2 hidden-sm hidden-xs",
      body = div(
        class = "",
        htmlOutput("risk_context_us"),
        fluidRow(
          align = "center",
          column(
            12,
            HTML(
              "<h3>Can you guess the risk levels in YOUR community?  Try the Risk Quiz and share your score!</h3>"
            ),
          )
        ),
        fluidRow(
          align = "center",
          column(
            12,
            shinyWidgets::actionBttn(
              "to_game",
              label = "Take the quiz",
              style = "jelly",
              color = "success",
              size = "sm"
            )
          )
        ),
        fluidRow(
          align = "center",
          column(
            12,
            div(
              div(style = "height: 10px;"),
              div(
                class = "well fake-sidebar",
                HTML(
                  "<p class='intro-text'><a href='https://duke.qualtrics.com/jfe/form/SV_0SZR4fPxyUAg9Ke', rel='noopener' target='_blank'>Fill out this 5-minute survey</a> for a chance to win a $50 Amazon gift card!</p>"
                )
              )
            )
          )
        )
      ),
      title = "Risk context",
      collapsed = F
    ),
    panel(
      class = "col-md-auto",
      title = "",
      can_collapse = FALSE,
      body = div(
        leafletOutput(outputId = "usa_map", height = "70vh"),
        HTML(
          "<p>(Note: This map uses a Web Mercator projection that inflates the area of states in northern latitudes. County boundaries are generalized for faster drawing.)</p>"
        ),
        fluidRow(
          align = "center",
          column(
            12,
            shinyWidgets::actionBttn(
              "to_global",
              label = "Explore global risk estimates",
              style = "jelly",
              color = "success",
              size = "sm"
            )
          )
        ),
        fluidRow(
          align = "center",
          column(
            width = 12,
            div(
              class = "well fake-sidebar panel-content",
              HTML(
                paste0(
                  "<h3>After viewing this map are you MORE or LESS <strong>",
                  "willing to participate</strong> in an event of this size?</h3>"
                )
              ),
              shinyWidgets::sliderTextInput(
                "risk_followup",
                "",
                choices = c(
                  "1" = "Much less willing",
                  "2" = "A little less willing",
                  "3" = "Same as before",
                  "4" = "A little more willing",
                  "5" = "Much more willing"
                ),
                selected = "Same as before",
                grid = T,
                width = "90%",
                hide_min_max = T
              ),
              shinyWidgets::actionBttn(
                "map_will",
                label = "Submit",
                style = "jelly",
                color = "success",
                size = "sm",
              ),
            )
          )
        )
      )
    )
  ))
)
