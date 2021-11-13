updateSelectizeInput(session, "states_dd", choices = names(regions), selected = "GA")
updateSelectizeInput(session, "us_states", choices = names(regions), selected = "GA")
updateSelectizeInput(session, "regions", choices = regions, selected = "states-alpha.png")
daily_plots_dir <- list.dirs("www/daily_risk_plots/", full.names = F)
names(daily_plots_dir) <- ymd_hms(daily_plots_dir, tz = "America/New_York")
updateSelectizeInput(session, "date", choices = rev(daily_plots_dir), selected = tail(daily_plots_dir, 1))

output$risk_plots <- renderImage(
  {
    risk_folder <- paste0("www/daily_risk_plots/", input$date)
    region <- input$regions
    if (is.null(region))
      return
    if (region %in% c("states-alpha.png", "states-rank.png")) {
      width <- 837
      height <- 900
    } else {
      width <- 864
      height <- 504
    }
    src_file = paste0(risk_folder, "/", region)
    shiny::validate(need(file.exists(src_file), message = "No historical plot data available"))
    list(
      src = src_file,
      width = width,
      height = height
    )
  },
  deleteFile = FALSE
)

output$dl_risk <- downloadHandler(
  filename = function() {
    paste0("Daily-Risk-Estimate-", input$date, "-", input$regions)
  },
  content <- function(file) {
    file.copy(paste0("www/daily_risk_plots/", input$date, "/", input$regions), file)
  },
  contentType = "image/png"
)