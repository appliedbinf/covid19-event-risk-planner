geo_county <- reactiveVal(NULL)

observeEvent(input$`nav-page`, {
  if (input$`nav-page` == "game" && is.null(input$cookies$consent)) {
    inputSweetAlert(
      session = session,
      inputId = "game_consent",
      inputPlaceholder = CONSENT_POPUP_PLACEHOLDER,
      title = CONSENT_POPUP_TITLE,
      text = CONSENT_POPUP_TEXT,
      type = "question",
      reset_input = TRUE,
      btn_labels = "Confirm",
      input = "radio",
      inputOptions = c("yes" = "Yes", "no" = "No"),
      inputValue = "yes"
    )
  }
})

observeEvent(input$game_consent, {
  if (input$game_consent == "yes") {
    session$sendCustomMessage("cookie-set", list(name = "consent", value = "yes"))
  } else {
    session$sendCustomMessage("cookie-set", list(name = "consent", value = "no"))
  }
})

observeEvent(input$risk_state,
  {
    if (length(geo_county()) > 0) {
      geo_county(NULL)
    } else {
      if (input$risk_state == "USA") {
        game_counties <- "Entire US"
      } else {
        game_counties <- usa_counties %>%
          filter(stname == !!input$risk_state) %>%
          pull(NAME) %>%
          sort() %>%
          unique()
      }

      updateSelectizeInput("risk_county",
        choices = game_counties,
        session = session
      )
    }
  },
  ignoreNULL = T
)

output$location_selector <- renderUI({
  if (input$geolocation) {
    HTML("<p class='loc-text success'>Detected your location automatically</p>")
  } else {
    HTML("<p class='loc-text'>Please choose your location below</p>")
  }
})

observeEvent(input$setGeo,
  {
    if (input$geolocation) {
      api_url <- glue::glue(
        "https://geo.fcc.gov/api/census/block/find?",
        "latitude={input$lat}&longitude={input$long}&format=json"
      )
      location <- jsonlite::fromJSON(api_url, )
      geo_county(location$County$name)

      if (is.null(geo_county())) {
        return(NULL)
      }
      updateSelectizeInput(
        "risk_county",
        session = session,
        choices = usa_counties %>%
          filter(stname == location$State$code) %>%
          pull(NAME) %>%
          sort() %>%
          unique(),
        selected = geo_county()
      )
      updateSelectizeInput("risk_state",
        session = session,
        selected = location$State$code
      )
    }
  },
  ignoreNULL = T
)

observeEvent(input$submit_answers, {
  if (is.null(input$cookies$consent)) {
    inputSweetAlert(
      session = session,
      inputId = "game_consent",
      input = "checkbox",
      inputPlaceholder = CONSENT_POPUP_PLACEHOLDER,
      title = CONSENT_POPUP_TITLE,
      text = CONSENT_POPUP_TEXT,
      type = "question",
      reset_input = TRUE
    )
    return(NULL)
  }
  shinyjs::show("game_interactive_elem")
  sel_state <- input$risk_state
  sel_county <- input$risk_county
  ans_20 <- input$quiz20
  ans_50 <- input$quiz50
  ans_100 <- input$quiz100
  ans_1000 <- input$quiz1000
  if (sel_state == "USA") {
    USpop <- 331 * 10^6
    C_i <- sum(as.numeric(state_data$C_i))
    quiz_nvec <- c(20, 50, 100, 1000)
    pred_risk <- as.data.frame(
      as.list(calc_risk(C_i, quiz_nvec, USpop)),
      col.names = c("pred_20", "pred_50", "pred_100", "pred_1000"),
      row.names = NULL
    ) %>%
      mutate(
        GEOID = "0",
        data_ts = usa_counties %>% pull(updated) %>% first()
      )
  } else {
    pred_risk <- usa_counties %>%
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
  }

  pred_risk <- pred_risk %>%
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
  results_table <- data.table::data.table(
    "Event size" = as.integer(c(20, 50, 100, 1000)),
    "Predicted risk" = riskParams(trunc(
      c(
        pred_risk$pred_20,
        pred_risk$pred_50,
        pred_risk$pred_100,
        pred_risk$pred_1000
      )
    )),
    "Your guess" = paste0(round(c(
      ans_20, ans_50, ans_100, ans_1000
    )), "%")
  )
  if (any(select(pred_risk, starts_with("pred_")) < 1)) {
    overall_acc <- "Overall accuracy: Not available"
    acc_text <- "Overall accuracy not available due to data limitations"
    tweet_msg <- glue::glue(
      "I just took the @covid19riskusa Risk Quiz. Try it out and guess the risk in your own community!"
    )
  } else {
    overall_acc_perc <- round(100 - (sum(abs(
      select(pred_risk, starts_with("diff"))
    )) / 4))
    overall_acc <- glue::glue("Overall Accuracy: {overall_acc_perc}%")
    tweet_msg <- glue::glue(
      "I scored {overall_acc_perc}% on the @covid19riskusa Risk Quiz. Try it out and guess the risk in your own community!"
    )
    signed_err <- (sum(select(pred_risk, starts_with("diff"))) / 4)
    acc_text <- case_when(
      signed_err >= 25 ~ "Our risk estimates were higher than your guesses.",
      signed_err >= 10 ~ "Our risk estimates were slightly higher than your guesses.",
      signed_err > -10 ~ "Our risk estimates were close to your guesses!",
      signed_err >= -25 ~ "Our risk estimates were slightly lower than your guesses.",
      signed_err <= 25 ~ "Our risk estimates were lower than your guesses."
    )
  }

  if (input$cookies$consent == "yes") {
    sql <-
      "INSERT INTO risk_game_results
        (
          GEOID, data_ts, pred_20, pred_50,
          pred_100, pred_1000, g_20, g_50,
          g_100, g_1000, ip, latitude,
          longitude, utm_source, utm_medium,
          utm_content, utm_campaign
        )
        VALUES (?geoid, ?data_ts, ?p20,
                ?p50, ?p100, ?p1000, ?g20,
                ?g50, ?g100, ?g1000, ?ip,
                ?lat, ?long, NULLIF(?utm_source, 'NULL'),
                NULLIF(?utm_medium, 'NULL'),
                NULLIF(?utm_content, 'NULL'),
                NULLIF(?utm_campaign, 'NULL')
        )"

    query <-
      sqlInterpolate(
        ANSI(),
        gsub("\\n\\w+", " ", sql),
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
        ip = input$ip_data,
        lat = str_or_unk(input$lat),
        long = str_or_unk(input$long),
        utm_source = ref_content$utm_source,
        utm_medium = ref_content$utm_medium,
        utm_content = ref_content$utm_content,
        utm_campaign = ref_content$utm_campaign
      )
    dbSendQuery(db, query)
  }

  tweet_url <- glue::glue(
    "https://twitter.com/intent/tweet?text={tweet_msg}&url=https://covid19risk.biosci.gatech.edu/?quiz"
  )
  show_alert(
    title = "Your quiz results",
    text = div(
      h4(overall_acc),
      p(acc_text),
      renderTable(results_table, align = "c", width = "100%", ),
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
            id = "game_interactive_elem",
            align = "center",
            column(
              width = 12,
              div(
                "After taking this quiz, are you MORE or LESS willing to participate in an events in your area?",
              ),
              shinyWidgets::sliderTextInput(
                "game_followup",
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
    html = TRUE,
    closeOnClickOutside = FALSE,
    width = "400px%",
    showCloseButton = T,
    btn_labels = NA
  )
})


observeEvent(input$game_will, {
  save_willingness(
    source = "game",
    asc_bias = input$asc_bias,
    event_size = input$event_size_map,
    answer = input$risk_followup,
    ip = input$ip_data,
    vacc_imm = input$imm_lvl,
    latitude = input$lat,
    longitude = input$long,
    utm_source = ref_content$utm_source,
    utm_medium = ref_content$utm_medium,
    utm_content = ref_content$utm_content,
    utm_campaign = ref_content$utm_campaign
  )
  shinyjs::hide("game_interactive_elem")
})
