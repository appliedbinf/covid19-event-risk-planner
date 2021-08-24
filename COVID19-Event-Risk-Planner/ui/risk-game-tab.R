make_resp_slider = function(input, label) {
  sliderInput(
    inputId = input,
    label = label,
    min = 1,
    max = 99,
    step = 1,
    value = 50,
    post = "%",
    width = '100%'
  )
}

risk_game_tab = tabPanel(
  value = "game",
  title = "Risk prediction game",
  fluid = TRUE,
  chooseSliderSkin("Round"),
  tags$script(
    '
      $(document).ready(function () {
        navigator.geolocation.getCurrentPosition(onSuccess, onError);

        function onError (err) {
          Shiny.onInputChange("geolocation", false);
        }

        function onSuccess (position) {
          setTimeout(function () {
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
      title = "Test Your Knowledge!",
      can_collapse = F,
      class = "col-sm-12 col-xs-12 col-md-3 well fake-sidebar",
      body = div(
        # fluidRow(
        HTML(
          "<p class='intro-text'>How well do you know the risk levels in your community?  Take the quiz to find out, and share your high score.</p>"
        ),
        uiOutput("location_selector"),
        selectizeInput(
          "risk_state",
          choices = setNames(state.abb, state.name),
          label = "Select state"
        ),
        selectizeInput("risk_county", choices = NULL, label = "Select county"),
        uiOutput("guessdf")
        # )
      )
    ),
    panel(
      class = "col-md-auto",
      title = "",
      can_collapse = FALSE,
      body = div(
        fluidRow(align = "center",
                 column(
                   8,
                   HTML(
                     "<h3>Imagine a coffee shop in your area with <b><u>20 people</u></b> inside.  What's the probability that <u>at least one</u> of the people is infected with COVID-19</h3>"
                   )
                 ),
                 column(8,
                        make_resp_slider("quiz20", "")),
                 column(3,
                        # HTML('<img src="/example_images/20_1.jpg" alt="Store ">')
                        )),
        fluidRow(align = "center",
                 column(
                   8,
                   HTML(
                     "<h3>Imagine a grocery store in your area with <b><u>50 people</u></b> inside.  What's the probability that <u>at least one</u> of the people is infected with COVID-19</h3>"
                   )
                 ),
                 column(8,
                        make_resp_slider("quiz50", "")),
                 column(3,
                        # HTML('<img src="/example_images/50_2.jpg" alt="Store">')
                 )),
        fluidRow(align = "center",
                 column(
                   8,
                   HTML(
                     "<h3>Imagine a movie theater in your area with <b><u>100 people</u></b> inside.  What's the probability that <u>at least one</u> of the people is infected with COVID-19</h3>"
                   )
                 ),
                 column(8,
                        make_resp_slider("quiz100", "")),
                 column(3,
                        # HTML('<img src="/example_images/100_2.jpg" alt="Store">')
                 )),
        fluidRow(align = "center",
                 column(
                   8,
                   HTML(
                     "<h3>Imagine a gradaution ceremony in your area with <b><u>1000 people</u></b> inside.  What's the probability that <u>at least one</u> of the people is infected with COVID-19</h3>"
                   )
                 ),
                 column(8,
                        make_resp_slider("quiz1000", "")),
                 column(3,
                        # HTML('<img src="/example_images/1000_2.jpg" alt="Store">')
                 )),
        fluidRow(align = "center",
                 column(
                   10,
                   shinyWidgets::actionBttn(
                     "submit_answers",
                     label = "I'm done! Show me my results",
                     style = "jelly",
                     color = "success",
                     size = "sm"
                   )
                 ))
      )
    )
  )

)
