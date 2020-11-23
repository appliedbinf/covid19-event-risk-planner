## ---------------------------
##
## COVID19 Event Risk Assessment Planning tool
##
## Aroon Chande <mail@aroonchande.com> <achande@ihrc.com>
## Lavanya Rishsishwar <lrishishwar@ihrc.com>
## Model from Joshua Weitz
## See: https://github.com/jsweitz/covid-19-ga-summer-2020
## ---------------------------
# options(shiny.reactlog = TRUE)
options(scipen = 999)
# library(mapview, lib.loc = "/projects/covid19/covid19/R/x86_64-redhat-linux-gnu-library/3.6/")

Sys.setenv(PATH = with_path("/projects/covid19/bin", Sys.getenv("PATH")))

pcrit <- function(x) {
  0.01 / x
}

calc_risk <- function(I, n, USpop, scaling_factor=10/14) {
  p_I <- (I / USpop) * scaling_factor
  r <- 1 - (1 - p_I)**n
  round(100 * r, 1)
}

roundUpNice <- function(x, nice = c(1, 2, 4, 5, 6, 8, 10)) {
  if (length(x) != 1) stop("'x' must be of length 1")
  10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}

get_data <- function() {
  current_fh <- tail(list.files("states_current/", full.names = TRUE), 1)
  current_time <<- gsub(".csv", "", basename(current_fh))
  daily_fh <- tail(list.files("states_daily/", full.names = TRUE), 1)
  daily_time <<- gsub(".csv", "", basename(daily_fh))
  state_current <<- read.csv(current_fh, stringsAsFactors = F)
  states <<- state_current$state
  cur_date <- gsub("-", "", Sys.Date())
  past_date <- ymd(cur_date) - 14
  states_historic <<- read.csv(daily_fh, stringsAsFactors = F)
  states_historic <<- subset(states_historic, ymd(date) == past_date) %>% arrange(state)
  state_pops <<- read.delim("state_pops.tsv", header = T, sep = "\t", stringsAsFactors = F)
  state_data <<- state_current %>%
    select(state, positive) %>%
    arrange(state)
  state_data$C_i <<- round((state_data$positive - states_historic$positive)  * 10 / 14)
}


disconnected <- sever_default(title = "Session disconnected", 
    subtitle = "Your session disconnected for some reason :(", 
    button = "Reconnect",
    button_class = "warning"
    )
timeout <- sever_default(title = "Session timeout reached", 
    subtitle = "Your session ended due to inactivity", 
    button = "Reconnect",
    button_class = "warning"
    )
event_size = c("10"=0, "15"=1, "20"=2, "25"=3, "50"=4, "100"=5, "500"=6, "1000"=7, "5000"=8)

shinyServer(function(input, output, session) {
  rupture(ms = 600000, html=timeout)
  sever(html=disconnected)
  observeEvent(input$ruptured, {
    session$close()
    })
    
  observe({
    query = getQueryString()
    if ("global" %in% names(query)){
    print("found")
      updateTabsetPanel(session, "maps", "global")
    }
  })
  observeEvent(input$to_usa, {
    updateTabsetPanel(session, "maps", "usa")
  }) 
  observeEvent(input$to_global, {
    updateTabsetPanel(session, "maps", "global")
  }) 
  observeEvent(input$to_data, {
    updateTabsetPanel(session, "maps", "about")
    updateTabsetPanel(session, "abouttabs", "data")

  })
  observeEvent(input$to_data_global, {
    updateTabsetPanel(session, "maps", "about")
    updateTabsetPanel(session, "abouttabs", "data")

  }) 

   observeEvent(input$event_size_map, {
    print(input$event_size_map)
    js = glue::glue("document.getElementsByClassName('leaflet-control-layers')[0].getElementsByTagName('input')[{event_size[as.character(input$event_size_map)]}].click();")
    print(js)
    shinyjs::runjs(js)
  })

  observeEvent(input$asc_bias, {
    output$map_static <- renderUI({
      tags$iframe(
        id = "leaflet",
        src = paste0(input$asc_bias, ".html"),
        style="position: relative; height: 60vh; width: 95vw; max-width: 992px; max-height: 500px; min-height: 350px; align: center", frameBorder = "0"
      )
    })
  })

  # UK map
  observeEvent(input$global_event_size_map, {
    output$eu_map_static <- renderUI({
      tags$iframe(
        src = paste0("eu_", input$global_asc_bias, "_", input$global_event_size_map, ".html"),
        style="position: relative; height: 60vh; width: 95vw; max-width: 992px; max-height: 500px; min-height: 350px; align: center", frameBorder = "0"
      )
    })
  })

  observeEvent(input$global_asc_bias, {
    output$eu_map_static <- renderUI({
      tags$iframe(
        src = paste0("eu_", input$global_asc_bias, "_", input$global_event_size_map, ".html"),
        style="position: relative; height: 60vh; width: 95vw; max-width: 992px; max-height: 500px; min-height: 350px; align: center", frameBorder = "0"
      )
    })
  })


  regions <- c(
    "USA, Alphabetical" = "states-alpha.png",
    "USA, By Rank" = "states-rank.png",
    "AK" = "AK.png", "AL" = "AL.png", "AR" = "AR.png", "AZ" = "AZ.png",
    "CA" = "CA.png", "CO" = "CO.png", "CT" = "CT.png", "DC" = "DC.png",
    "DE" = "DE.png", "FL" = "FL.png", "GA" = "GA.png", "HI" = "HI.png",
    "IA" = "IA.png", "ID" = "ID.png", "IL" = "IL.png", "IN" = "IN.png",
    "KS" = "KS.png", "KY" = "KY.png", "LA" = "LA.png", "MA" = "MA.png",
    "MD" = "MD.png", "ME" = "ME.png", "MI" = "MI.png", "MN" = "MN.png",
    "MO" = "MO.png", "MS" = "MS.png", "MT" = "MT.png", "NC" = "NC.png",
    "ND" = "ND.png", "NE" = "NE.png", "NH" = "NH.png", "NJ" = "NJ.png",
    "NM" = "NM.png", "NV" = "NV.png", "NY" = "NY.png", "OH" = "OH.png",
    "OK" = "OK.png", "OR" = "OR.png", "PA" = "PA.png", "PR" = "PR.png", 
    "RI" = "RI.png", "SC" = "SC.png", "SD" = "SD.png", "TN" = "TN.png",
    "TX" = "TX.png", "UT" = "UT.png", "VA" = "VA.png", "VT" = "VT.png",
    "WA" = "WA.png", "WI" = "WI.png", "WV" = "WV.png", "WY" = "WY.png"
  )
  updateSelectizeInput(session, "states_dd", choices = names(regions), selected = "GA")
  updateSelectizeInput(session, "us_states", choices = names(regions), selected = "GA")
  updateSelectizeInput(session, "regions", choices = regions, selected = "states-alpha.png")
  daily_plots_dir <- list.dirs("www/daily_risk_plots/", full.names = F)
  names(daily_plots_dir) <- ymd_hms(daily_plots_dir, tz = "America/New_York")
  updateSelectizeInput(session, "date", choices = rev(daily_plots_dir), selected = tail(daily_plots_dir, 1))

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


  output$risk_plots <- renderImage(
    {
      risk_folder <- paste0("www/daily_risk_plots/", input$date)
      region <- input$regions
      # if (is.null(region))
      #     return
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
  values_pred <- reactiveValues(infect = 800000, event_size = 275, pop = 330 * 10^6, state = "US", use_state = FALSE)
  values_dd <- reactiveValues(infect = 200000, event_size = 275, pop = 10617423, state = "GA", use_state = TRUE)
  # observeEvent(input$use_state, {
  #     values_pred$use_state <- input$use_state
  #     })
  pred_inputs <- reactive({
    list(input$state, input$event_size_us, input$use_state, input$infect_us)
  })
  observeEvent(input$calc_us, {
    # cat("calc_us pushed", input$event_size_us, " ", input$infect_us, " ", input$us_states, " ", input$use_state, "\n")
    req(input$event_size_us)
    req(input$infect_us)
    event_size <- isolate(input$event_size_us)
    event_size <- as.numeric(gsub("[ ,_]", "", event_size))

    values_pred$event_size <- event_size
    infect <- isolate(input$infect_us)
    infect <- round(as.numeric(gsub("[ ,_]", "", infect)))

    values_pred$infect <- infect
    values_pred$pop <- 330 * 10^6
    if (input$use_state) {
      values_pred$state <- isolate(input$us_states)
      values_pred$use_state <- isolate(input$use_state)
    } else {
      values_pred$state <- "US"
      values_pred$use_state <- isolate(input$use_state)
    }
  })



  output$values <- renderText({
    outtext <- reactiveValuesToList(values_pred)
    sapply(outtext, paste, collapse = ":")
  })

  output$values_dd <- renderText({
    outtext <- reactiveValuesToList(values_dd)
    paste(outtext, collapse = "\t")
  })
  get_data()
  pred_plot <- ""
  output$plot_us <- renderPlot({
    xblock <- c(10, 100, 1000, 10**4, 10**5)
    yblock <- c(10, 100, 1000, 10000, 10**5, 4 * 10**5, 10**6, 2 * 10**6, 8 * 10**6)

    names(xblock) <- c("10\nDinner party", "100\nWedding reception", "1,000\nSmall concert", "10,000\nSoccer match", "100,000\nNFL game")
    names(yblock) <- c("10", "100", "1,000", "10,000", "100,000", "400,000", "1 million", "2 million", "8 million")
    if (8 * 10**6 < values_pred$event_size) {
      yblock <- yblock + c(values_pred$event_size)
      names(yblock) <- c("10", "100", "1,000", "10,000", "100,000", "400,000", "1 million", "2 million", "8 million", format(values_pred$event_size, big.mark = ","))
    }
    use_state <- values_pred$use_state
    state <- values_pred$state
    if (use_state) {
      USpop <- as.numeric(state_pops[state_pops$state == state, "pop"])
      xblock <- c(10, 100, 1000, 10**4, 10**5)
      nvec <- round(c(.01 * USpop, .05 * USpop, .25 * USpop))
      yblock <- sapply(c(10, 100, 1000, .05 * USpop, .1 * USpop, .25 * USpop, USpop), roundUpNice)
      names(xblock) <- c("10\nDinner party", "100\nWedding reception", "1,000\nSmall concert", "10,000\nSoccer match", "100,000\nNFL game")
      names(yblock) <- format(sapply(c(10, 100, 1000, .05 * USpop, .1 * USpop, .25 * USpop, USpop), roundUpNice), big.mark = ",")
      ylimits <- c(10, max(yblock, 10 * 10^ceiling(log10(max(yblock)))))
    } else {
      USpop <- 330 * 10^6
      stata <- "US"
      nvec <- c(8000000, 400000, 2000000)
      xblock <- c(10, 100, 1000, 10**4, 10**5)
      yblock <- c(10, 100, 1000, 10000, 10**5, 4 * 10**5, 10**6, 2 * 10**6, 8 * 10**6)
      names(xblock) <- c("10\nDinner party", "100\nWedding reception", "1,000\nSmall concert", "10,000\nSoccer match", "100,000\nNFL game")
      names(yblock) <- c("10", "100", "1,000", "10,000", "100,000", "400,000", "1 million", "2 million", "8 million")
      ylimits <- c(10**4, 3 * 10**7)
    }

    # cat(state, "\t", USpop, "\n")
    n <- logspace(0, 6, 100)
    pcrit_val <- pcrit(n)
    numcrit <- pcrit_val * USpop
    sizevec <- c(10, 100, 1000, 10000, 100000, 10**7)
    risk_vals <- c(0.01, 0.02, 0.1, 0.5, 0.9)
    pcrit_risk_list <- list()
    for (i in 1:length(risk_vals)) {
      pcrit_risk <- 1 - (1 - risk_vals[i])**(1 / n)
      pcrit_risk <- pcrit_risk * USpop
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
    risk.df <- do.call(rbind.data.frame, risk_vals_list)

    infect <- values_pred$infect
    event_size <- values_pred$event_size
    shiny::validate(
      need(is.numeric(event_size), "Event size must be a number"),
      need(event_size >= 5, "Event size must be >=5"),
      need(event_size <= 100000, "Event size must be <= 100,000")
    )
    shiny::validate(
      need(is.numeric(infect), "Number of active cases must be a number"),
      need(infect >= 10, "Number of active cases must be >=10"),
      need(infect < 0.5 * USpop, paste("Number of active cases must less than 10% of population <", round(USpop * .5)))
    )
    risk <- calc_risk(infect, event_size, USpop)
    risk <- case_when(risk < .1 ~ "<0.1", risk > 99 ~ ">99", TRUE ~ as.character(risk))

    pred_plot <<- ggplot() +
      geom_area(data = pcrit_risk_list[[1]], aes(x = x, y = y), alpha = .5) +
      # geom_text(data = pcrit_lab.df, aes(x=x, y = y, label=paste(risk * 100, "% Chance")), angle=angle, size=6) +
      geom_hline(yintercept = risk.df$nvec, linetype = 2) +
      geom_path(data = pcrit.df, aes(x = x, y = y, group = risk, color = as.factor(100 * risk)), size = 1) +
      scale_color_manual(values = c("black", "#fcae91", "#fb6a4a", "#de2d26", "#a50f15")) +
      # geom_segment(data=pcrit.df, aes(x=xstart, y=ystart, xend=xend, yend=yend)) +
      geom_label(data = risk.df, aes(x = svec, y = nvec, label = paste(risk, "% Chance")), nudge_y = .1, size = 5, fill = "blue", alpha = .5, color = "white") +
      geom_vline(xintercept = event_size, linetype = 3) +
      geom_hline(yintercept = infect, linetype = 3) +
      geom_point(aes(x = event_size, y = infect), size = 4, color = "red") +
      geom_point(data = risk.df, aes(x = svec, y = nvec), size = 3) +
      geom_label_repel(aes(x = event_size, y = infect, label = paste(risk, "% chance an attendee\n has COVID-19.")), size = 5) +
      # geom_polygon(aes(x=c(0, 0, 100), y=c(pcrit.df[1,]$ystart, 0, 0), group=c(1,1,1)), fill="grey", alpha = 0.5) +
      theme_clean() +
      # coord_cartesianxlim(1, 10**5) + ylim(ylimits)
      scale_x_continuous(name = "Number of people at event", breaks = xblock, labels = names(xblock), trans = "log10", expand = c(.1, .1)) +
      scale_y_continuous(name = paste0("Number of circulating cases in ", state), breaks = yblock, labels = names(yblock), trans = "log10", expand = c(.1, .1)) +
      annotation_logticks(scaled = T) +
      # # geom_vline(xintercept = 10**5, linetype=2) +
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
      guides(color = guide_legend(title = "% Chance"), override.aes = list(size = 2), label.position = "bottom") +
      labs(
        caption = paste0("© CC-BY-4.0\tChande, A.T., Gussler, W., Harris, M., Lee, S., Rishishwar, L., Jordan, I.K., Andris, C.M., and Weitz, J.S. 'Interactive COVID-19 Event Risk Assessment Planning Tool'\nhttp://covid19risk.biosci.gatech.edu\nRisk estimates made:  ", today(), "\nReal-time COVID19 data comes from the COVID Tracking Project: https://covidtracking.com/api/\nUS 2019 population estimate data comes from the US Census: https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-total.html"),
        title = paste0("COVID-19 Event Risk Assessment Planner - ", state, " - Exploratory"),
        subtitle = "Estimated chance that one or more individuals are COVID-19 positive at an event\ngiven event size (x-axis) and current case prevalence (y-axis)"
      )
    pred_plot
  })
dd_inputs <- reactive({
    list(input$states_dd, input$event_dd, input$use_state_dd)
  })
  #
  dd_plot <- ""
  states_dd <- "US"
  observeEvent(dd_inputs(), {
    req(dd_inputs)
    xblock <- c(10, 100, 1000, 10**4, 10**5)
    names(xblock) <- c("10\nDinner party", "100\nWedding reception", "1,000\nSmall concert", "10,000\nSoccer match", "100,000\nNFL game")
    # cat("218 ", values_dd$use_state, "\n")
    use_state <- input$use_state_dd
    state <- input$states_dd
    states_dd <<- state
    # cat("220\t", state, "\t",use_state, "\n")
    if (use_state) {
      USpop <- as.numeric(state_pops[state_pops$state == state, "pop"])
      # cat(USpop)
      pcrit_label_x <- c(-9, -20, -200, -2000, -7000)
      C_i <- as.numeric(state_data[state_data$state == state, "C_i"])
      yblock <- c(10, 100, 1000, C_i, 5 * C_i, 10 * C_i, 10 * 10^ceiling(log10(10 * C_i)))
      names(yblock) <- c("10", "100", "1,000", format(c(C_i, 5 * C_i, 10 * C_i, 10 * 10^ceiling(log10(10 * C_i))), big.mark = ","))
      ylimits <- c(10, max(yblock))
    } else {
      states_dd <<- "US"
      USpop <- 330 * 10^6
      pcrit_label_x <- c(9, 20, 200, 2000, 7000)
      C_i <- sum(as.numeric(state_data$C_i))
      yblock <- c(10, 100, 1000, 10000, 10**5, 4 * 10**5, 10**6, 2 * 10**6, 8 * 10**6)
      names(yblock) <- c("10", "100", "1,000", "10,000", "100,000", "400,000", "1 million", "2 million", "8 million")
      ylimits <- c(10**4, 3 * 10**7)
    }
    nvec <- c(C_i, 5 * C_i, 10 * C_i)
    event_size <- as.numeric(gsub("[ ,-]", "", isolate(input$event_dd)))
    risk <- calc_risk(nvec, event_size, USpop, 1)
    risk <- case_when(risk < .1 ~ "<0.1", risk > 99 ~ ">99", TRUE ~ as.character(risk))

    output$dd_text <- renderUI({
      HTML(paste0(
        "<p style='font-size: 18px;'><br/><strong>C<sub>I</sub> = Current reported incidence</strong><br/>Chance someone is COVID19 positive at C<sub>I</sub>  (", format(nvec[1], big.mark = ","), "): ", risk[1], "%<br/>",
        "Chance someone is COVID19 positive at 5x C<sub>I</sub> (", format(nvec[2], big.mark = ","), "): ", risk[2], "%<br/>",
        "Chance someone is COVID19 positive at 10x C<sub>I</sub> (", format(nvec[3], big.mark = ","), "): ", risk[3], "%</p>"
      ))
    })

    output$dd_current_data <- renderUI({
      HTML(
        paste0(
          "Real-time data last updated at: ", ymd_hms(current_time, tz = ""),
          "<br/>Historic data last updated at: ", ymd_hms(daily_time, tz = "")
        )
      )
    })

    output$plot_dd <- renderPlot({
      req(input$states_dd)
      req(input$event_dd)
      req(USpop)
      # cat("state: ", state, "\tpop: ", USpop, "\n")
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
      # cat("USpop\t", USpop, "\n")
      pcrit_label <- ytarget / USpop
      pcrit_lab_list <- list()
      for (i in 1:length(risk_vals)) {
        # cat("rv\t", risk_vals[i],"pc\t", pcrit_label, "\n")
        nlabel <- log(1 - risk_vals[i]) / log(1 - pcrit_label)
        pcrit_lab_list[[i]] <- data.frame("risk" = risk_vals[i], "x" = nlabel, y = ytarget * 1.4)
      }

      risk_vals_list <- list()
      # cat("before risk_vals\n")
      for (i in 1:length(nvec)) {
        p_equiv <- nvec[i] / USpop
        risk_vals_I <- round(100 * (1 - (1 - p_equiv)**sizevec), 2)
        risk_vals_list[[i]] <- data.frame("nvec" = nvec[i], "svec" = sizevec, "risk" = risk_vals_I)
      }
      # cat("after risk_vals\n")

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
      # ylimits <- c(10**4, 3*10**6)

      # cat(infect, "-", ylimits,"\n")
      dd_plot <<- ggplot() +
        geom_area(data = pcrit_risk_list[[1]], aes(x = x, y = y), alpha = .5) +
        # geom_text(data = pcrit_lab.df, aes(x=x, y = y, label=paste(risk * 100, "% Chance")), angle=angle, size=6) +
        geom_hline(yintercept = risk.df$nvec, linetype = 2) +
        geom_path(data = pcrit.df, aes(x = x, y = y, group = risk, color = as.factor(risk * 100)), size = 1) +
        scale_color_manual(values = c("black", "#fcae91", "#fb6a4a", "#de2d26", "#a50f15")) +
        # geom_segment(data=pcrit.df, aes(x=xstart, y=ystart, xend=xend, yend=yend)) +
        geom_label(data = risk.df, aes(x = svec, y = nvec, label = paste(risk, "% Chance")), nudge_y = .1, size = 5, fill = "blue", alpha = .5, color = "white") +
        geom_vline(xintercept = event_size, linetype = 3) +
        # geom_hline(yintercept = nvec, linetype=3) +
        geom_point(aes(x = event_size, y = nvec), size = 4.5, shape = c(16, 17, 15), color = "red") +
        geom_point(data = risk.df, aes(x = svec, y = nvec), size = 3) +
        # geom_label_repel(aes(x=event_size, y=nvec, lrabel = paste(ifelse(risk > .99,">99", round(100*risk, 1)) , "% Chance someone is \ninfected with COVID19")), size=5) +
        # geom_polygon(aes(x=c(0, 0, 100), y=c(pcrit.df[1,]$ystart, 0, 0), group=c(1,1,1)), fill="grey", alpha = 0.5) +
        theme_clean() +
        # coord_cartesianxlim(1, 10**5) + ylim(ylimits)
        scale_x_continuous(name = "Number of people at event", breaks = xblock, labels = names(xblock), trans = "log10", expand = c(.1, .1), ) +
        scale_y_continuous(name = paste0("Number of circulating cases in ", states_dd), breaks = yblock, labels = names(yblock), trans = "log10", expand = c(.1, .1)) +
        annotation_logticks(scaled = T) +
        # # geom_vline(xintercept = 10**5, linetype=2) +
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


  output$dl_pred <- downloadHandler(
    filename = function() {
      paste("Predicted-risk-Event_size-", values_pred$state, "-Infection_count", values_pred$event_size, "-", values_pred$infect, "-", today(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = pred_plot, width = 12, height = 12, units = "in")
    }
  )
})
