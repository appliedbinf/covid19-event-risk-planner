geo_county = reactiveVal(NULL)

riskParams = function(val){
  case_when(
    val < 1 ~ "< 1",
    val > 99 ~ "> 99",
    TRUE ~ as.character(val)
  )
}

observeEvent(input$`nav-page`, {
  if (input$`nav-page` == "game" && is.null(input$cookies$consent)){
    inputSweetAlert(
      session = session, inputId = "game_consent",
      inputPlaceholder = "I am over 18 and in the US",
      title = "Are you over 18 and in the US?", text = paste0(
        "Users under 18 and/or those who reside outside the US",
        " are encouraged to use the risk prediction tools, ",
        "unfortunately we cannot save your survey feedback.  ",
        "Select 'No' if you are not eligible or would like to ",
        "opt out of having your responses saved for research purposes.  ",
        "Please see the About page for more details"),
      type = "question", reset_input = TRUE, btn_labels = "Confirm", input="radio",
      inputOptions = c("yes" = "Yes", "no" = "No"), inputValue = "yes"
    )
  }
})

observeEvent(input$game_consent,{
  if (input$game_consent == "yes"){
    session$sendCustomMessage("cookie-set", list(
      name = "consent", value = "yes"
    ))
  } else {
    session$sendCustomMessage("cookie-set", list(
      name = "consent", value = "no"
    ))
  }
})

observeEvent(input$risk_state, {

  if (length(geo_county()) > 0){
    geo_county(NULL)
  } else{
    updateSelectizeInput(
      "risk_county",
      choices = usa_counties %>% filter(stname == !!input$risk_state) %>% pull(NAME) %>% sort %>% unique(),
      session = session
    )
  }
}, ignoreNULL = T)

output$location_selector <- renderUI({
  if (input$geolocation) {
    HTML("<p class='loc-text success'>Detected your location automatically</p>")
    # input$geolocation <- FALSE
  }
  else{
    HTML("<p class='loc-text'>Please choose your location below</p>")
  }
})

observeEvent(input$setGeo, {
  if (input$geolocation) {
    api_url = glue::glue(
      "https://geo.fcc.gov/api/census/block/find?latitude={input$lat}&longitude={input$long}&format=json
  "
    )
    location = jsonlite::fromJSON(api_url)
    geo_county(location$County$name)

    if (is.null(geo_county())){
      return(NULL)
    }
    updateSelectizeInput(
      "risk_county",
      session = session,
      choices = usa_counties %>% filter(stname == location$State$code) %>% pull(NAME) %>% sort %>% unique(),
      selected = geo_county()
    )
    updateSelectizeInput("risk_state",
                         session = session,
                         selected = location$State$code)
  }
}, ignoreNULL = T)

observeEvent(input$submit_answers, {
  if (is.null(input$cookies$consent)){
    inputSweetAlert(
      session = session, inputId = "game_consent", input = "checkbox",
      inputPlaceholder = "I am over 18 and in the US",
      title = "Are you over 18 and in the US?",  text = paste0(
        "Users under 18 and/or those who reside outside the US",
        " are encouraged to use the risk prediction tools, ",
        "unfortunately we cannot save your survey feedback.  ",
        "Select 'No' if you are not eligible or would like to ",
        "opt out of having your responses saved for research purposes.  ",
        "Please see the About page for more details"),
      type = "question", reset_input = TRUE
    )
    return(NULL)
  }
  # shinyjs::enable("game_will")
  sel_state = input$risk_state
  sel_county = input$risk_county
  pred_risk = usa_counties %>%
    filter(stname == sel_state, NAME == sel_county) %>%
    sf::st_drop_geometry() %>%
    select(
      GEOID,
      data_ts = updated,
      pred_20 = "4_20",
      pred_50 = "4_50",
      pred_100 = "4_100",
      pred_1000 = "4_1000"
    )
  ans_20 = input$quiz20
  ans_50 = input$quiz50
  ans_100 = input$quiz100
  ans_1000 = input$quiz1000
  pred_risk = pred_risk %>%
    mutate(
      g_20 = ans_20,
      g_50 = ans_50,
      g_100 = ans_100,
      g_1000 = ans_1000
    ) %>%
    rowwise() %>%
    mutate(
      diff_20 = pred_20 - g_20,
      diff_50 = pred_50 - g_50,
      diff_100 = pred_100 - g_100,
      diff_1000 = pred_1000 - g_1000
    )
  results_table = data.table::data.table(
    "Event size" = as.integer(c(20, 50, 100, 1000)),
    "Predicted risk" = paste0(riskParams(
      trunc(
        c(
          pred_risk$pred_20,
          pred_risk$pred_50,
          pred_risk$pred_100,
          pred_risk$pred_1000
        )
      )
    ), "%"),
    "Your guess" =  paste0(round(c(
      ans_20, ans_50, ans_100, ans_1000
    )), "%")
  )
  overall_acc = round(100 - (sum(abs(
    select(pred_risk, starts_with("diff"))
  )) / 4))
  signed_err = (sum(select(pred_risk, starts_with("diff"))) / 4)
  acc_text = case_when(
    signed_err >= 25 ~ "Our risk estimates were higher than your guesses.",
    signed_err >= 10 ~ "Our risk estimates were slightly higher than your guesses." ,
    signed_err > -10 ~ "Our risk estimates were close to your guesses!" ,
    signed_err >= -25 ~ "Our risk estimates were slightly lower than your guesses." ,
    signed_err <= 25 ~ "Our risk estimates were lower than your guesses."
  )
  if (input$cookies$consent == "yes"){
    sql <-
      "INSERT INTO risk_game_results (GEOID, data_ts, pred_20, pred_50, pred_100, pred_1000, g_20, g_50, g_100, g_1000, ip)
          VALUES (?geoid, ?data_ts, ?p20, ?p50, ?p100, ?p1000, ?g20, ?g50, ?g100, ?g1000, ?ip)"

    query <-
      sqlInterpolate(
        ANSI(),
        sql,
        geoid = pred_risk$GEOID,
        data_ts = pred_risk$data_ts,
        p20 = pred_risk$pred_20,
        p50 = pred_risk$pred_50,
        p100 = pred_risk$pred_100,
        p1000 = pred_risk$pred_1000,
        g20 = pred_risk$g_20,
        g50 = pred_risk$g_50,
        g100 = pred_risk$g_100,
        g1000 = pred_risk$g_1000,
        ip = input$ip_data
      )
    dbSendQuery(db, query)
  }
  tweet_msg = glue::glue(
    "I scored {overall_acc}% on the @covid19riskusa Risk Guessing Game. Try it out and guess the risk in your own community!"
  )
  tweet_url = glue::glue(
    "https://twitter.com/intent/tweet?text={tweet_msg}&url=https://covid19risk.biosci.gatech.edu/?game"
  )
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
      renderTable(results_table, align = "c", width = "100%",),
      tags$a(
        href = URLencode(tweet_url),
        tags$i("Tweet your score", class = "fab fa-twitter"),
        class = "twitter-share-button twitter-hashtag-button",
        target = "_blank"
      ),
      br(),
      if (input$cookies$consent == "yes") {
        tagList(
          fluidRow(
            align = "center",
            column(
              width = 12,
              div(
                "After taking this quiz, are you MORE or LESS willing to participate in an events in your area?"
              ),
              shinyWidgets::sliderTextInput(
                "game_followup",
                "",
                choices = c(
                  "1" = "Much less willing",
                  "2" = "A little less willing",
                  "3" = "Same as before",
                  "4" = "A little more willing",
                  "5" = 'Much more willing'
                ),
                selected = "Same as before",
                grid = T,
                width = "85%",
                hide_min_max = T
              )
            )
          ),
          shinyWidgets::actionBttn(
            "game_will",
            label = "Submit",
            style = "jelly",
            color = "success",
            size = "sm"
          )
        )
      }
    ),
    # htmlOutput("thankyou"),
    html = TRUE,
    closeOnClickOutside = TRUE,
    width = "400px%",
    showCloseButton = F,
    btn_labels = NA
  )

})


observeEvent(input$game_will, {
  # shinyjs::disable("game_will")
  sql <-
    "INSERT INTO willingness (source, asc_bias, event_size, answer, ip)
        VALUES (?p1, ?p2, ?p3, ?p4, ?p5)"

  query <- sqlInterpolate(
    ANSI(),
    sql,
    p1 = "game",
    p2 = -1,
    p3 = -1,
    p4 = input$game_followup,
    p5 = input$ip_data
  )
  dbSendQuery(db, query)
  show_alert("Response saved", text = "Thank you for taking the Risk Prediction Quiz",)
})
