# county_geom = sf::st_read("map_data/tl_2017_us_county.geojson")
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

# setShapeLabel <- function( map, data = getMapData(map), layerId,
#                            label = NULL,
#                            options = NULL
# ){
#   cat("in setShapeLabel","\n")
#   options <- c(list(layerId = layerId),
#                options,
#                filterNULL(list(label = label
#                )))
#   # evaluate all options
#   options <- evalFormula(options, data = data)
#   # make them the same length (by building a data.frame)
#   options <- do.call(data.frame, c(options, list(stringsAsFactors=FALSE)))
#
#   layerId <- options[[1]]
#   label <- options[-1] # drop layer column
#
#   # typo fixed in this line
#   leaflet::invokeMethod(map, data, "setLabel", "shape", layerId, label);
# }
#
#

#
# observeEvent(input$event_size_map, {
#
#   risk_data = county_geom %>%
#     left_join(usa_counties %>%
#                filter(asc_bias == !!input$asc_bias, event_size == !!input$event_size_map) %>%
#                collect())
#   # sf::write_sf(risk_data, "data.fgb")
#
#   leafletProxy("usa_map", session) %>%
#     clearGroup(group="risk_overlay") %>%
#     clearControls() %>%
#     addPolygons(
#       data = risk_data, layerId = "riskpoly",
#       color = "#444444", weight = 0.2, smoothFactor = 0.1,
#       opacity = 1.0, fillOpacity = 0.5,
#       fillColor = ~ pal(risk),
#       # highlight = highlightOptions(weight = 1),
#       label = maplabs(risk_data),
#       group = "risk_overlay"
#     )
# })


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

# observeEvent(input$event_size_map, {
#   output$map_static <- renderUI({
#     tags$iframe(
#       src = paste0("https://covid19risk.biosci.gatech.edu/", input$asc_bias, "_", input$event_size_map, ".html"),
#       style="position: relative; height: 60vh; width: -moz-available; width: -webkit-fill-available;  width: fill-available; max-width: 992px; max-height: 500px; min-height: 350px; align: center", frameBorder = "0"
#     )
#   })
# })
#
# observeEvent(input$asc_bias, {
#   output$map_static <- renderUI({
#     tags$iframe(
#       src = paste0("https://covid19risk.biosci.gatech.edu/",input$asc_bias, "_", input$event_size_map, ".html"),
#       style="position: relative; height: 60vh; width: -moz-available; width: -webkit-fill-available;  width: fill-available; max-width: 992px; max-height: 500px; min-height: 350px; align: center", frameBorder = "0"
#     )
#   })
# })

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
