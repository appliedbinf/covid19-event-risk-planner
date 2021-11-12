county_geom = sf::st_read("map_data/geomUnitedStates.geojson")
county_geomV = sf::st_read("map_data/geomUnitedStatesCV.geojson")

pal <- colorBin("YlOrRd", bins = c(0, 1, 25, 50, 75, 99, 100))
maplabs <- function(riskData) {
  riskData <- riskData %>%
    mutate(risk = case_when(
      risk == 100 ~ "> 99",
      risk == 0 ~ "No data",
      risk < 1 ~ "< 1",
      is.na(risk) ~ "No data",
      TRUE ~ as.character(risk)
    ))
  labels <- paste0(
    "<strong>", paste0(riskData$NAME, ", ", riskData$stname), "</strong><br/>",
    "Current Risk Level: <b>", riskData$risk, ifelse(riskData$risk == "No data", "", "&#37;"), "</b><br/>",
    "State-level immunity via vaccination: <strong>", round(riskData$pct_fully_vacc, 1), "%</strong></b><br/>",
    "Updated: ", riskData$updated,
    ""
  ) %>% lapply(htmltools::HTML)
  return(labels)
}



risk_text = c(
  "10" = paste0("<b>A dinner party</b>",'<img src="/example_images/10_1.svg" alt="dinner party">', '<b style="float: right">...or a convenience store</b>','<img src="/example_images/10_2.svg" alt="Store" width="500" height="600">'),
  "15" = HTML("<b>A boutique</b>",'<img src="/example_images/15_1.svg" alt=" boutique">', '<b style="float: right">...or a fitness class</b>', '<img src="/example_images/15_2.svg" alt="fitness class">'),
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
      HTML("<br/>",risk_text[as.character(input$event_size_map)]),
      class = "context-header"
    )
    )
})

output$dl_map <- downloadHandler(
  filename = paste0("County-level COVID risk estimates map - ", today(), ".png"),
  content = function(file) {
    # with_path('/projects/covid19/bin', Sys.getenv("PATH"))
    showModal(modalDialog("This can take 20 seconds", title = "Rendering map image...", footer = NULL))
    map_sel <<- map_sel %>%
      setView(lat = countyCenter(input$county_text)[4], lng = countyCenter(input$county_text)[3], zoom = 7)
    leafletProxy("map_us", session) %>%
      setView(lat = countyCenter(input$county_text)[4], lng = countyCenter(input$county_text)[3], zoom = 7)
    mapshot(map_sel, file = file)
    removeModal()
  },
  contentType = "application/octet-stream"
)




observeEvent(input$map_will, {
  shinyjs::disable("map_will")
  shinyjs::delay(3000, shinyjs::enable("map_will"))
  map_consent = input$cookies$consent
  if (!is.null(map_consent) && map_consent == "yes"){
    sql <- "INSERT INTO willingness (source, asc_bias, event_size, answer, ip, vacc_imm, latitude, longitude)
        VALUES (?p1, ?p2, ?p3, ?p4, ?p5, ?p6, ?p7, ?p8)"

    query <- sqlInterpolate(ANSI(), sql, p1 = "map", p2 = input$asc_bias,
                            p3 = input$event_size_map, p4 = input$risk_followup, p5 = input$ip_data,
                            p6 = input$imm_lvl, p7 = str_or_unk(input$lat), p8=str_or_unk(input$long))
    dbSendQuery(db, query)
    show_toast("Response saved", text = "Thank you!", timerProgressBar = F)
  } else {
    inputSweetAlert(
      session = session, inputId = "over18_US",
      inputPlaceholder = "I am over 18 and in the US",
      title = "Are you over 18 and in the US?", text = paste0(
        "Users under 18 and/or who reside outside the US",
        " are encouraged to use the risk prediction tools, ",
        "but unfortunately we cannot save your survey feedback.  ",
        "Select 'No' if you are not eligible or would like to ",
        "opt out of having your responses saved for research purposes.  ",
        "Please see the About page for more details"),
      type = "question", reset_input = TRUE, btn_labels = "Confirm", input="radio",
      inputOptions = c("yes" = "Yes", "no" = "No"), inputValue = "yes"
    )
  }

})

observeEvent(input$over18_US, {
  if (input$over18_US == "yes"){
    session$sendCustomMessage("cookie-set", list(
      name = "consent", value = "yes"
    ))
    sql <- "INSERT INTO willingness (source, asc_bias, event_size, answer, ip, vacc_imm, latitude, longitude)
        VALUES (?p1, ?p2, ?p3, ?p4, ?p5, ?p6, ?p7, ?p8)"

    query <- sqlInterpolate(ANSI(), sql, p1 = "map", p2 = input$asc_bias,
                            p3 = input$event_size_map, p4 = input$risk_followup, p5 = input$ip_data,
                            p6 = input$imm_lvl, p7 = str_or_unk(input$lat), p8=str_or_unk(input$long))
    dbSendQuery(db, query)
    show_toast("Response saved", text = "Thank you!", timerProgressBar = F)
  }
})

pal <- colorBin("YlOrRd", bins = c(0.001, 1, 25, 50, 75, 99, 100))
legendlabs <- c("< 1", " 1-25", "25-50", "50-75", "75-99", "> 99", "No or missing data")
output$usa_map <- renderLeaflet({



  risk_data = usa_counties %>%
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

  basemap = leaflet(options = leafletOptions(worldCopyJump = F, preferCanvas = TRUE)) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    setView(lat = 37.1,
            lng = -95.7,
            zoom = 4) %>%
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
      # opacity = 1.0,
      # fillOpacity = 0.7,
      # fillColor = ~ pal(risk),
      highlight = highlightOptions(weight = 1),
      # label = maplabs(risk_data)
    ) %>%
    addPolygons(
      layerId = ~imid,
      data = risk_data,
      weight = 0,
      smoothFactor = 0.1,
      # label = maplabs(risk_data)
    ) %>%
    addLegend(
      data = risk_data,
      position = "topright",
      pal = pal,
      values = ~ risk,
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

map_obs = reactive(list(input$event_size_map, input$asc_bias, input$imm_lvl))

observeEvent(map_obs(), {
  risk_data = usa_counties %>%
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
        pct_fully_vacc > input$imm_lvl ~ 0.7,
        TRUE ~ 0.0
      ),
      polyid = paste0("id", GEOID),
      imid = paste0("im", GEOID)
    )

  leafletProxy("usa_map", data = risk_data) %>%
    setShapeStyle(layerId = ~polyid, fillColor = pal(risk_data$risk), color = "#444444", fillOpacity = 0.7, ) %>%
    setShapeStyle(layerId = ~imid, fillColor = "white", fillOpacity = ~ imOp, ) %>%
    setShapeLabel(layerId = ~imid, label = maplabs(risk_data))
})



