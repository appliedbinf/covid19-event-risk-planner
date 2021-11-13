observeEvent(input$global_event_size_map, {
  output$eu_map_static <- renderUI({
    tags$iframe(
      src = paste0("https://covid19risk.biosci.gatech.edu/", "eu_", input$global_asc_bias, "_", input$global_event_size_map, ".html"),
      style="position: relative; height: 60vh; width: -moz-available; width: -webkit-fill-available;  width: fill-available; max-width: 992px; max-height: 500px; min-height: 350px; align: center", frameBorder = "0"
    )
  })
})

observeEvent(input$global_asc_bias, {
  output$eu_map_static <- renderUI({
    tags$iframe(
      src = paste0("https://covid19risk.biosci.gatech.edu/", "eu_", input$global_asc_bias, "_", input$global_event_size_map, ".html"),
      style="position: relative; height: 60vh; width: 95vw; max-width: 992px; max-height: 500px; min-height: 350px; align: center", frameBorder = "0"
    )
  })
})
