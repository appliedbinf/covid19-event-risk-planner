

dd_inputs <- reactive({
  list(input$states_dd, input$event_dd, input$use_state_dd)
})


observeEvent(dd_inputs(), {
  req(dd_inputs)
  xblock <- c(10, 100, 1000, 10**4, 10**5)
  names(xblock) <- RTR_X_LABS
  use_state <- input$use_state_dd
  state <- input$states_dd
  states_dd <- state
  if (use_state) {
    USpop <- as.numeric(state_pops[state_pops$state == state, "pop"])
    pcrit_label_x <- RTR_PCRIT_STATES
    C_i <- as.numeric(state_data[state_data$state == state, "C_i"])
    yblock <- c(10, 100, 1000, C_i, 5 * C_i, 10 * C_i, 10 * 10^ceiling(log10(10 * C_i)))
    names(yblock) <- format(yblock, big.mark = ",")
    ylimits <- c(10, max(yblock))
  } else {
    states_dd <<- "US"
    USpop <- 331 * 10^6
    pcrit_label_x <- RTR_PCRIT_US
    C_i <- sum(as.numeric(state_data$C_i))
    yblock <- c(10**c(1, 2, 3, 4, 5), 4 * 10**5, 10**6, 2 * 10**6, 8 * 10**6)
    names(yblock) <-
      c(
        "10",
        "100",
        "1,000",
        "10,000",
        "100,000",
        "400,000",
        "1 million",
        "2 million",
        "8 million"
      )
    ylimits <- c(10**4, 3 * 10**7)
  }
  nvec <- c(C_i, 5 * C_i, 10 * C_i)
  event_size <- as.numeric(gsub("[ ,-]", "", isolate(input$event_dd)))
  risk <- calc_risk(nvec, event_size, USpop, 1)
  risk <- case_when(risk < .1 ~ "<0.1", risk > 99 ~ ">99", TRUE ~ as.character(risk))

  output$dd_text <- renderUI({
    HTML(paste0(
      "<p style='font-size: 18px;'><br/><strong>C<sub>I</sub> ",
      "= Current reported incidence</strong><br/>Chance someone is ",
      "COVID19 positive at C<sub>I</sub>  (", 
      format(nvec[1], big.mark = ","), "): ", risk[1], "%<br/>",
      "Chance someone is COVID19 positive at 5x C<sub>I</sub> (",
      format(nvec[2], big.mark = ","), "): ", risk[2], "%<br/>",
      "Chance someone is COVID19 positive at 10x C<sub>I</sub> (",
      format(nvec[3], big.mark = ","), "): ", risk[3], "%</p>"
    ))
  })


  output$plot_dd <- renderPlot({
    req(input$states_dd)
    req(input$event_dd)
    req(USpop)
    shiny::validate(need(event_size > 9, "Please enter an event size >= 10"))
    n <- logspace(0, 6, 100)
    pcrit_val <- pcrit(n)
    numcrit <- pcrit_val * USpop
    sizevec <- c(1, 10, 100, 1000, 10000, 100000, 10**7)
    risk_vals <- c(0.01, 0.02, 0.1, 0.5, 0.9)
    pcrit_risk_list <- list()
    for (i in 1:length(risk_vals)) {
      pcrit_risk <- 1 - (1 - risk_vals[i])**(1 / n)
      pcrit_risk <- pcrit_risk * USpop
      pcrit_risk[is.infinite(pcrit_risk)] <- USpop
      pcrit_risk_list[[i]] <- data.frame("risk" = risk_vals[i], "y" = pcrit_risk, "x" = n)
    }
    ytarget <- 100000
    pcrit_label <- ytarget / USpop
    pcrit_lab_list <- list()
    for (i in 1:length(risk_vals)) {
      nlabel <- log(1 - risk_vals[i]) / log(1 - pcrit_label)
      pcrit_lab_list[[i]] <- data.frame("risk" = risk_vals[i], "x" = nlabel, y = ytarget * 1.4)
    }

    risk_vals_list <- list()
    for (i in 1:length(nvec)) {
      p_equiv <- nvec[i] / USpop
      risk_vals_I <- round(100 * (1 - (1 - p_equiv)**sizevec), 2)
      risk_vals_list[[i]] <- data.frame("nvec" = nvec[i], "svec" = sizevec, "risk" = risk_vals_I)
    }

    pcrit.df <- do.call(rbind.data.frame, pcrit_risk_list)
    pcrit_lab.df <- do.call(rbind.data.frame, pcrit_lab_list)
    risk.df <- do.call(rbind.data.frame, risk_vals_list) %>%
      mutate(risk = case_when(
        risk > 99 ~ ">99",
        risk <= 0.1 ~ "<0.1",
        TRUE ~ as.character(risk)
      ))

    shiny::validate(
      need(is.numeric(event_size), "Event size must be a number"),
      need(event_size >= 5, "Event size must be >=5"),
      need(event_size <= 100000, "Event size must be <= 100,000")
    )
    dd_plot <<- ggplot() +
      geom_area(data = pcrit_risk_list[[1]], aes(x = x, y = y), alpha = .5) +
      geom_hline(yintercept = risk.df$nvec, linetype = 2) +
      geom_path(data = pcrit.df, aes(x = x, y = y, group = risk, color = as.factor(risk * 100)), size = 1) +
      scale_color_manual(values = c("black", "#fcae91", "#fb6a4a", "#de2d26", "#a50f15")) +
      geom_label(data = risk.df, aes(x = svec, y = nvec, label = paste(risk, "% Chance")), nudge_y = .1, size = 5, fill = "blue", alpha = .5, color = "white") +
      geom_vline(xintercept = event_size, linetype = 3) +
      geom_point(aes(x = event_size, y = nvec), size = 4.5, shape = c(16, 17, 15), color = "red") +
      geom_point(data = risk.df, aes(x = svec, y = nvec), size = 3) +
      theme_clean() +
      scale_x_continuous(name = "Number of people at event", breaks = xblock, labels = names(xblock), trans = "log10", expand = c(.1, .1), ) +
      scale_y_continuous(name = paste0("Number of circulating cases in ", states_dd), breaks = yblock, labels = names(yblock), trans = "log10", expand = c(.1, .1)) +
      annotation_logticks(scaled = T) +
      coord_cartesian(ylim = ylimits, xlim = c(10, 100001)) +
      theme(
        axis.title.x = element_text(size = 20),
        axis.text = element_text(size = 16),
        axis.title.y = element_text(size = 20),
        plot.caption = element_text(hjust = 0, face = "italic"),
        plot.caption.position = "plot",
        plot.title = element_text(hjust = 0.5, size = 20),
        plot.subtitle = element_text(hjust = 0.5)
      ) +
      guides(color = guide_legend(title = "% Chance"), override.aes = list(size = 2)) +
      labs(
        caption = paste0("© CC-BY-4.0\tChande, A.T., Gussler, W., Harris, M., Lee, S., Rishishwar, L., Jordan, I.K., Andris, C.M., and Weitz, J.S. 'Interactive COVID-19 Event Risk Assessment Planning Tool'\nhttp://covid19risk.biosci.gatech.edu\nData updated on and risk estimates made:  ", today(), "\nReal-time COVID19 data comes from the COVID Tracking Project: https://covidtracking.com/api/\nUS 2019 population estimate data comes from the US Census: https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-total.html"),
        title = paste0("COVID-19 Event Risk Assessment Planner - ", states_dd, " - ", today()),
        subtitle = "Estimated chance that one or more individuals are COVID-19 positive at an event\ngiven event size (x-axis) and current case prevalence (y-axis)"
      )
    dd_plot
  })
})

output$dl_dd <- downloadHandler(
  filename = function() {
    paste("Predicted-risk-", states_dd, "-Event_size-", input$event_dd, "-", today(), ".png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = dd_plot, width = 12, height = 12, units = "in")
  }
)
