observeEvent(input$risk_state, {
  req(input$risk_state)
  updateSelectizeInput(
    "risk_county",
    choices = usa_counties %>% filter(stname == !!input$risk_state) %>% pull(NAME) %>% sort %>% unique(),
    session = session
  )
})

output$location_selector <- renderUI({
  if (input$geolocation){
    api_url = glue::glue("https://geo.fcc.gov/api/census/block/find?latitude={input$lat}&longitude={input$long}&format=json
")
    location = jsonlite::fromJSON(api_url)
    output$loc = renderPrint({
      location
    })
    updateSelectizeInput("risk_state", session=session, selected = location$State$code)
    updateSelectizeInput("risk_county", session=session,
                         choices = usa_counties %>% filter(stname == !!input$risk_state) %>% pull(NAME) %>% sort %>% unique(),
                         selected = location$County$name)
    HTML("<p class='loc-text success'>Detected your location automatically</p>")
  }
  else{
    HTML("<p class='loc-text'>Please choose your location below</p>")
  }
})
observeEvent(input$submit_answers, {

  state = input$risk_state
  county = input$risk_county
  pred_risk = usa_counties %>%
    filter(stname == state, NAME == county, asc_bias == 3, event_size %in% c(25, 50, 100, 1000)) %>%
    collect()
  ans_20 = input$quiz20
  ans_50 = input$quiz50
  ans_100 = input$quiz100
  ans_1000 = input$quiz1000
  pred_risk = pred_risk %>%
    mutate(guess = c(ans_20, ans_50, ans_100, ans_1000)) %>%
    mutate(diff = (risk - guess), absdiff = abs(risk - guess) )
  results_to_db = data.frame(
    "ip"
  )
  overall_acc = 100 - (sum(pred_risk$absdiff)/4)
  signed_err = (sum(pred_risk$diff)/4)
  acc_text = case_when(
    signed_err >= 25 ~ "Our risk estimates were higher than your guesses.",
    signed_err >= 10 ~ "Our risk estimates were slightly higher than your guesses." ,
    signed_err > -10 ~ "Our risk estimates were close to your guesses!" ,
    signed_err >= -25 ~ "Our risk estimates were slightly lower than your guesses." ,
    signed_err <= 25 ~ "Our risk estimates were lower than your guesses."
  )
  tweet_msg = glue::glue("I scored {overall_acc}% on the @covid19riskusa Risk Prediction Game.  Can you do better? ")
  tweet_url = glue::glue("https://twitter.com/intent/tweet?text={tweet_msg}&url=https://weitz-covid-dev.appliedbinf.com/?game")
  # TODO; accuracy table
  # accuracy_table
  # tweet_url = "https://twitter.com/intent/tweet?button_hashtag=COVID19&ref_src=twsrc%5Etfw"
  # tweet_data = glue::glue('class="twitter-hashtag-button" data-size="large" data-text="{tweet_msg}" data-url="https://weitz-covid-dev.appliedbinf.com/?game" data-related="covid19riskusa" data-dnt="true" data-show-count="false"')
  # tweet_html = glue::glue("<a href=\"{tweet_url}\" {tweet_data}>Share your score<a><script async src=\"https://platform.twitter.com/widgets.js\" charset=\"utf-8\"></script>")
  show_alert(
    title = "Your quiz results",
    text = div(
      h4(glue::glue("Overall Accuracy: {overall_acc}%")),
      p(acc_text),
      # HTML(accuracy_table),
      # HTML(tweet_html)),
      tags$a(href=URLencode(tweet_url), tags$i("Tweet your score", class="fab fa-twitter"), class="twitter-share-button twitter-hashtag-button", target="_blank")),
    html = TRUE,
    closeOnClickOutside = TRUE,
  )

})



