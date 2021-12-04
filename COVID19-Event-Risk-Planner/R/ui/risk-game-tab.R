risk_game_tab <- tabPanel(
  value = "game",
  title = "Risk Quiz",
  fluid = TRUE,
  chooseSliderSkin("Round"),
  tags$script(
    '
      $(document).ready(function () {
        navigator.geolocation.getCurrentPosition(onSuccess, onError);

        function onError (err) {
        console.log(err)
          Shiny.onInputChange("geolocation", false);
        }

        function onSuccess (position) {
          setTimeout(function () {
            Shiny.onInputChange("setGeo", true);
            var coords = position.coords;
            console.log(coords.latitude + ", " + coords.longitude);
            Shiny.onInputChange("geolocation", true);
            Shiny.onInputChange("lat", coords.latitude);
            Shiny.onInputChange("long", coords.longitude);
          }, 1100)
        }
      });
              '
  ),
  fluidRow(
    panel(
      title = "Risk Quiz",
      can_collapse = F,
      class = "col-sm-12 col-xs-12 col-md-3",
      body = div(
        div(
          class = "well fake-sidebar",
          HTML(
            "<p class='intro-text'>Can you guess the risk levels in your community?",
            "  Take the quiz to find out, and share your high score.</p>"
          ),
          uiOutput("location_selector"),
          selectizeInput(
            "risk_state",
            choices = RISK_GAME_CHOICES,
            label = "Select state"
          ),
          selectizeInput("risk_county", choices = NULL, label = "Select county")
        ),
        div(style = "height: 25px;"),
        SURVEY_ELEMENT
      )
    ),
    panel(
      class = "col-md-auto",
      title = "",
      can_collapse = FALSE,
      body = div(
        fluidRow(
          align = "center",
          column(
            8,
            HTML(
              "<h3>Imagine a coffee shop in your area with <b><u>20 people</u></b> inside.  What's the probability that <u>at least one</u> of the people is infected with COVID-19?</h3>"
            )
          ),
          column(
            8,
            make_resp_slider("quiz20", "")
          ),
          column(3, )
        ),
        fluidRow(
          align = "center",
          column(
            8,
            HTML(
              "<h3>Imagine a grocery store in your area with <b><u>50 people</u></b> inside.  What's the probability that <u>at least one</u> of the people is infected with COVID-19?</h3>"
            )
          ),
          column(
            8,
            make_resp_slider("quiz50", "")
          ),
          column(3, )
        ),
        fluidRow(
          align = "center",
          column(
            8,
            HTML(
              "<h3>Imagine a movie theater in your area with <b><u>100 people</u></b> inside.  What's the probability that <u>at least one</u> of the people is infected with COVID-19?</h3>"
            )
          ),
          column(
            8,
            make_resp_slider("quiz100", "")
          ),
          column(3, )
        ),
        fluidRow(
          align = "center",
          column(
            8,
            HTML(
              "<h3>Imagine a graduation ceremony in your area with <b><u>1000 people</u></b> inside.  What's the probability that <u>at least one</u> of the people is infected with COVID-19?</h3>"
            )
          ),
          column(
            8,
            make_resp_slider("quiz1000", "")
          ),
          column(3, )
        ),
        fluidRow(
          align = "center",
          column(
            8,
            shinyWidgets::actionBttn(
              "submit_answers",
              label = "I'm done! Show my results",
              style = "jelly",
              color = "success",
              size = "sm"
            )
          )
        )
      )
    )
  )
)
