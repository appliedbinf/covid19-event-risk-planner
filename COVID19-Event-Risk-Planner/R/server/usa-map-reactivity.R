
risk_text <- c(
  "10" = paste0("<b>A dinner party</b>", '<img src="/example_images/10_1.svg" alt="dinner party">', '<b style="float: right">...or a convenience store</b>', '<img src="/example_images/10_2.svg" alt="Store" width="500" height="600">'),
  "15" = HTML("<b>A boutique</b>", '<img src="/example_images/15_1.svg" alt=" boutique">', '<b style="float: right">...or a fitness class</b>', '<img src="/example_images/15_2.svg" alt="fitness class">'),
  "20" = HTML("<b>A coffee shop</b>", '<img src="/example_images/20_1.svg" alt="coffee shop">', '<b style="float: right">...or a neighborhood BBQ</b>', '<img src="/example_images/20_2.svg" alt="neighborhood BBQ">'),
  "25" = HTML("<b>A classroom</b>", '<img src="/example_images/25_1.svg" alt="classroom">', '<b style="float: right">...or small bar</b>', '<img src="/example_images/25_2.svg" alt="small bar">'),
  "50" = HTML("<b>A supermarket</b>", '<img src="/example_images/50_1.svg" alt="supermarket">', '<b style="float: right">...or a restaurant</b>', '<img src="/example_images/50_2.svg" alt="house party">'),
  "100" = HTML("<b>A wedding</b>", '<img src="/example_images/100_1.svg" alt="wedding">', '<b style="float: right">...or a movie theater</b>', '<img src="/example_images/100_2.svg" alt="movie theater">'),
  "500" = HTML("<b>A large airplane</b>", '<img src="/example_images/500_1.svg" alt="large airplane">', '<b style="float: right">...or a high school basketball game</b>', '<img src="/example_images/500_2.svg" alt="basketball game">'),
  "1000" = HTML("<b>A performing arts theater</b>", '<img src="/example_images/1000_1.svg" alt="performing arts theater">', '<b style="float: right">...or a graduation ceremony</b>', '<img src="/example_images/1000_2.svg" alt="graduation ceremony">'),
  "5000" = HTML("<b>A large concert</b>", '<img src="/example_images/5000_1.svg" alt="large concert">', '<b style="float: right">...or a college basketball game</b>', '<img src="/example_images/5000_2.svg" alt="basketball game">')
)



output$risk_context_us <- renderUI({
  div(p(
    glue::glue(
      "You're viewing risk levels for an event with {input$event_size_map} people, which is like:"
    ),
    HTML("<br/>", risk_text[as.character(input$event_size_map)]),
    class = "context-header"
  ))
})


observeEvent(input$map_will, {
  shinyjs::disable("map_will")
  shinyjs::delay(3000, shinyjs::enable("map_will"))
  map_consent <- input$cookies$consent
  if (!is.null(map_consent) && map_consent == "yes") {
    save_willingness(
      source = "map",
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
    show_toast("Response saved", text = "Thank you!", timerProgressBar = F)
  } else {
    inputSweetAlert(
      session = session, inputId = "over18_US",
      inputPlaceholder = CONSENT_POPUP_PLACEHOLDER,
      title = CONSENT_POPUP_TITLE, 
      text = CONSENT_POPUP_TEXT,
      type = "question", reset_input = TRUE, btn_labels = "Confirm", input = "radio",
      inputOptions = c("yes" = "Yes", "no" = "No"), inputValue = "yes"
    )
  }
})

observeEvent(input$over18_US, {
  if (input$over18_US == "yes") {
    session$sendCustomMessage("cookie-set", list(
      name = "consent", value = "yes"
    ))

    save_willingness(
      source = "map",
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
    show_toast("Response saved", text = "Thank you!", timerProgressBar = F)
  }
})


output$usa_map <- renderLeaflet({
  risk_data <- usa_counties %>%
    select(
      GEOID,
      NAME,
      stname,
      pct_fully_vacc,
      updated,
      risk = "4_50",
      imOp,
      geometry
    ) %>%
    mutate(
      imOp = 0.0,
      polyid = paste0("id", GEOID),
      imid = paste0("im", GEOID)
    )

  basemap <- leaflet(options = leafletOptions(worldCopyJump = F, preferCanvas = TRUE)) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    setView(
      lat = 37.1,
      lng = -95.7,
      zoom = 4
    ) %>%
    addPolygons(
      layerId = ~stateline,
      data = stateline,
      fill = FALSE, color = "#943b29", weight = 1, smoothFactor = 0.5,
      opacity = 1.0
    ) %>%
    addPolygons(
      layerId = ~polyid,
      data = risk_data,
      color = "#444444",
      weight = 0.2,
      smoothFactor = 0.1,
      highlight = highlightOptions(weight = 1),
    ) %>%
    addPolygons(
      layerId = ~imid,
      data = risk_data,
      weight = 0,
      smoothFactor = 0.1,
    ) %>%
    addLegend(
      data = risk_data,
      position = "topright",
      pal = pal,
      values = ~risk,
      title = "Risk Level (%)",
      opacity = .7,
      labFormat = function(type, cuts, p) {
        paste0(legendlabs)
      }
    ) %>%
    addEasyButton(easyButton(
      icon = "fa-crosshairs fa-lg",
      title = "Locate Me",
      onClick = JS(
        "function(btn, map){ map.locate({setView: true, maxZoom: 7});}"
      )
    ))
})

map_obs <- reactive(list(input$event_size_map, input$asc_bias, input$imm_lvl))

observeEvent(map_obs(), {
  risk_data <- usa_counties %>%
    select(
      GEOID,
      NAME,
      stname,
      pct_fully_vacc,
      updated,
      risk := glue::glue("{input$asc_bias}_{input$event_size_map}"),
      imOp,
      geometry
    ) %>%
    mutate(
      imOp = case_when(
        input$imm_lvl == 0 ~ 0.0,
        pct_fully_vacc < input$imm_lvl ~ 0.0,
        pct_fully_vacc > input$imm_lvl ~ pct_fully_vacc / 100,
        TRUE ~ 0.0
      ),
      polyid = paste0("id", GEOID),
      imid = paste0("im", GEOID)
    )

  leafletProxy("usa_map", data = risk_data) %>%
    setShapeStyle(layerId = ~polyid, fillColor = pal(risk_data$risk), color = "#444444", fillOpacity = 0.7, ) %>%
    setShapeStyle(layerId = ~imid, fillColor = "white", fillOpacity = ~imOp, ) %>%
    setShapeLabel(layerId = ~imid, label = maplabs(risk_data))
})
