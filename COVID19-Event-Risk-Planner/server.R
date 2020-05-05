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
options(scipen=999)
library(shiny)
library(ggplot2)
library(ggrepel)
library(matlab)
library(lubridate)
library(dplyr)

pcrit = function(x){0.01/x}

calc_risk = function(I,n, USpop){
    p_I = I/USpop
    1-(1-p_I)**n
}


state_current = read.csv("https://covidtracking.com/api/v1/states/current.csv", stringsAsFactors = F)
states = state_current$state
cur_date = gsub("-", "", Sys.Date())
past_date = ymd(cur_date) - 14
states_historic = read.csv("https://covidtracking.com/api/v1/states/daily.csv", stringsAsFactors = F)
states_historic = subset(states_historic, ymd(date) == past_date) %>% arrange(state)


state_pops = read.delim('state_pops.tsv', header=T, sep="\t", stringsAsFactors = F)
state_data = state_current %>%
  select(state, positive) %>%
  arrange(state)

state_data$C_i = state_data$positive - states_historic$positive
rm(list=c("state_current", "states_historic"))
gc()




shinyServer(function(input, output, session) {
    
    updateSelectizeInput(session, "states_dd", choices=states, selected="GA")
    updateSelectizeInput(session, "us_states", choices=states, selected="GA")

    values_pred <- reactiveValues(infect = 800000, event_size = 275, pop = 330*10^6, state= "US", use_state = FALSE)
    values_dd <- reactiveValues(infect = 200000, event_size = 275, pop = 10617423, state= "GA", use_state = TRUE)
    observeEvent(input$calc_us, {
        #cat("calc_us pushed", input$event_size_us, " ", input$infect_us, " ", input$us_states, " ", input$use_state, "\n")
        req(input$event_size_us)
        req(input$infect_us)
        event_size = isolate(input$event_size_us)
        event_size = as.numeric(gsub("[ ,_]", "", event_size))
        
        values_pred$event_size = event_size
        infect = isolate(input$infect_us)
        infect = as.numeric(gsub("[ ,_]", "", infect))
       
        values_pred$infect = infect
        values_pred$pop = 330*10^6
        if (input$use_state){
            values_pred$state = isolate(input$us_states)
            values_pred$use_state = isolate(input$use_state)
        } else{
            values_pred$state = "US"
            values_pred$use_state = isolate(input$use_state)
        }

    })
    
    
    # observeEvent(input$calc_dd, {
    #     #cat("calc_dd pressed\n")
    #     req(input$event_dd)
    #     req(input$infect_dd)
    #     event_size = isolate(input$event_dd)
    #     event_size = as.numeric(gsub("[ ,_]", "", event_size))
    #     
    #     values_dd$event_size = event_size
    #     infect = isolate(input$infect_dd)
    #     infect = as.numeric(gsub("[ ,_]", "", infect))
    #     
    #     values_dd$infect = infect
    #     state = isolate(input$states_dd)
    #     pop = as.numeric(state_pops[state_pops == state,"pop"])
    #     values_dd$pop = pop
    #     values_dd$state = state
    #     values_dd$infect = infect
    #     #cat("Calc_dd state: ", state, "\tCalc_dd pop: ",pop,  "\n")
    #     if (input$use_state_dd){
    #         values_dd$state = isolate(input$states_dd)
    #         values_dd$use_state = isolate(input$use_state_dd)
    #         values_dd$pop = as.numeric(state_pops[state_pops == isolate(input$states_dd),"pop"])
    #         #cat(state_pops[state_pops == isolate(input$states_dd),"pop"])
    #     } else{
    #         values_dd$state = "US"
    #         values_dd$use_state = isolate(input$use_state_dd)
    #         values_dd$pop = 330*10^6
    #         
    #     }
    #     #cat(values_dd$state,"\t", values_dd$even_size, "\t", values_dd$infect, "\n")
    # })
    # 
    # #cat(infect, " - ", event_size)
    output$values <- renderText({
      outtext <- reactiveValuesToList(values_pred)
      sapply(outtext, paste, collapse=":")
      })
    
    output$values_dd <- renderText({
      outtext <- reactiveValuesToList(values_dd)
      paste(outtext,collapse="\t")
    })
    output$plot_us <- renderPlot({
      xblock = c(10, 100, 1000, 10**4, 10**5)
      yblock = c(10, 100, 1000, 10000, 10**5, 4*10**5, 10**6, 2*10**6, 8*10**6)
      names(xblock) <- c("10\nDinner party", "100\nWedding reception", "1,000\nSmall concert", "10,000\nSoccer match", "100,000\nNFL game" )
      names(yblock) <- c("10", "100", "1,000", "10,000", "100,000", "400,000", "1 million", "2 million", "8 million")
      
        use_state = values_pred$use_state
        state = values_pred$state
        if (use_state){
            USpop <- as.numeric(state_pops[state_pops$state == state,"pop"])
            pcrit_label_x = c(-9, -20, -200, -2000, -7000)
        } else{
            USpop <- 330*10^6
            pcrit_label_x = c(9, 20, 200, 2000, 7000)
            C_i = sum(state_data$C_i)
        }
        
        #cat(state, "\t", USpop, "\n")
        n=logspace(0,6,100);
        pcrit_val=pcrit(n);
        numcrit=pcrit_val*USpop;
        sizevec= c(10, 100, 1000, 10000, 100000, 10**7)
        risk_vals = c(0.01, 0.02,  0.1, 0.5, 0.9)
        pcrit_risk_list = list()
        for (i in 1:length(risk_vals)){
            pcrit_risk = 1 - (1-risk_vals[i])**(1/n)
            pcrit_risk = pcrit_risk*USpop
            pcrit_risk_list[[i]] = data.frame("risk" = risk_vals[i], "y" = pcrit_risk, "x" = n)
        }
        
        ytarget = 100000
        pcrit_label=ytarget/USpop
        pcrit_lab_list = list()
        for (i in 1:length(risk_vals)){
            nlabel = log(1-risk_vals[i])/log(1-pcrit_label)
            pcrit_lab_list[[i]] = data.frame("risk" = risk_vals[i], "x" = nlabel, y = ytarget*1.4)
        }
        
        nvec = c(8000000, 400000, 2000000)
        risk_vals_list = list()
        for (i in 1:length(nvec)){
            p_equiv = nvec[i]/USpop;
            risk_vals_I = round(100*(1 - (1-p_equiv)**sizevec), 2)
            risk_vals_list[[i]] = data.frame("nvec" = nvec[i], "svec" = sizevec, "risk" = risk_vals_I)
            
        }
        
        pcrit.df = do.call(rbind.data.frame, pcrit_risk_list)
        pcrit_lab.df = do.call(rbind.data.frame, pcrit_lab_list)
        risk.df = do.call(rbind.data.frame, risk_vals_list)
        
        infect <- values_pred$infect
        event_size <- values_pred$event_size
        validate(need(is.numeric(event_size), "Event size must be a number"),
                 need(event_size >= 5, "Event size must be >=0"),
                 need(event_size <= 100000, "Event size must be <= 100,000")
        )
        validate(need(is.numeric(infect), "Number of active cases must be a number"),
                 need(infect >= 10, "Number of active cases must be >=10"),
                 need(infect < USpop, paste("Number of active cases must less than 10% of population <", USpop * .1))
        )
        risk <- calc_risk(infect, event_size, USpop)
        ylimits <- c(10**4, 3*10**7)
        l.rise = min(pcrit_risk_list[[1]]$y) - max(pcrit_risk_list[[1]]$y)
        l.run = max(pcrit_risk_list[[1]]$x)
        angle = -45
        if (infect < 10**4){
            ylimits <- c(10, 3*10**6)
            angle <- -28
        } 
        
        ggplot() + geom_point(data = risk.df, aes(x=svec, y=nvec)) + 
        # geom_text(data = pcrit_lab.df, aes(x=x, y = y, label=paste(risk * 100, "% Chance")), angle=angle, size=6) + 
        geom_hline(yintercept = risk.df$nvec, linetype=2) + 
          geom_path(data = pcrit.df, aes(x=x, y=y, group=risk, color = as.factor(100*risk)), size=1)+
          scale_color_manual(values=c("#fee5d9", "#fcae91", "#fb6a4a", "#de2d26", "#a50f15")) +
        # geom_segment(data=pcrit.df, aes(x=xstart, y=ystart, xend=xend, yend=yend)) +
        geom_label(data = risk.df, aes(x=svec, y=nvec, label = paste(ifelse(risk > 99,">99", round(risk, 1)), "% Chance")), nudge_y = .1, size=5, fill="blue", alpha=.5, color="white") + 
        geom_vline(xintercept = event_size, linetype=3) + 
        geom_hline(yintercept = infect, linetype=3) + 
        geom_point(aes(x=event_size, y=infect), size=4, color="red") + 
        geom_label_repel(aes(x=event_size, y=infect, label = paste(ifelse(risk > .99,">99", round(100*risk, 1)) , "% Chance someone is \ninfected with COVID19")), size=5) +
        # geom_polygon(aes(x=c(0, 0, 100), y=c(pcrit.df[1,]$ystart, 0, 0), group=c(1,1,1)), fill="grey", alpha = 0.5) +
        ggthemes::theme_clean() +
        # coord_cartesianxlim(1, 10**5) + ylim(ylimits)
        scale_x_continuous(name="Number of people at event", breaks = xblock, labels = names(xblock), trans = "log10", expand = c(.1, .1), ) +
        scale_y_continuous(name=paste("Number of circulating cases in ", state), breaks = yblock, labels = names(yblock), trans = "log10", expand = c(.1, .1)) + annotation_logticks(scaled=T) +
        # # geom_vline(xintercept = 10**5, linetype=2) + 
        coord_cartesian(ylim=ylimits, xlim = c(10,100001)) + 
            theme(
                axis.title.x = element_text(size=20),
                axis.text  = element_text(size=16),
                axis.title.y = element_text(size=20),
            ) + guides(color = guide_legend(title="% Chance"),override.aes = list(size = 2) )
  
    }) 
    dd_inputs <- reactive({
      list(input$states_dd, input$event_dd, input$use_state_dd)
    })
    observeEvent(dd_inputs(), {
    nvec = c(C_i, 5*C_i, 10*C_i)
    event_size <- as.numeric(gsub("[ ,-]","", isolate(input$event_dd)))
    xblock = c(10, 100, 1000, 10**4, 10**5)
    names(xblock) <- c("10\nDinner party", "100\nWedding reception", "1,000\nSmall concert", "10,000\nSoccer match", "100,000\nNFL game" )
    #cat("218 ", values_dd$use_state, "\n")
    use_state = isolate(input$use_state_dd)
    state = input$states_dd
    #cat("220\t", state, "\t",use_state, "\n")
    if (use_state){
      USpop <- as.numeric(state_pops[state_pops$state == state,"pop"])
      #cat(USpop)
      pcrit_label_x = c(-9, -20, -200, -2000, -7000)
      C_i = as.numeric(state_data[state_data$state == state, "C_i"])
      yblock = c(10, 100, 1000, 10000, 10**5, 4*10**5, 10**6, 2*10**6)
      names(yblock) <- c("10", "100", "1,000", "10,000", "100,000", "400,000", "1 million", "2 million")
    } else{
      USpop <- 330*10^6
      pcrit_label_x = c(9, 20, 200, 2000, 7000)
      C_i = sum(as.numeric(state_data$C_i))
      yblock = c(10, 100, 1000, 10000, 10**5, 4*10**5, 10**6, 2*10**6, 8*10**6)
      names(yblock) <- c("10", "100", "1,000", "10,000", "100,000", "400,000", "1 million", "2 million", "8 million")
    }
    risk <- calc_risk(nvec, event_size, USpop)
    
    
    
    output$dd_text <- renderUI({HTML(paste0("Chance someone is COVID19 positive at the current reported incidence (", nvec[1],"): ", round(100*risk[1],1), "%<br/>",
                             "Chance someone is COVID19 positive at 5x the current reported incidence (", nvec[2],"): ", round(100*risk[2],1), "%<br/>",
                             "Risk at 10x the current reported incidence (", nvec[3],"): ",  round(100*risk[3],1), "%"))}
    )
                 
    output$plot_dd <- renderPlot({
      
      #cat("state: ", state, "\tpop: ", USpop, "\n")
      n=logspace(0,6,100);
        pcrit_val=pcrit(n);
        numcrit=pcrit_val*USpop;
        sizevec= c(1, 10, 100, 1000, 10000, 100000, 10**7)
        risk_vals = c(0.01, 0.02,  0.1, 0.5, 0.9)
        pcrit_risk_list = list()
        for (i in 1:length(risk_vals)){
            pcrit_risk = 1 - (1-risk_vals[i])**(1/n)
            pcrit_risk = pcrit_risk*USpop
            pcrit_risk[is.infinite(pcrit_risk)] <- USpop
            pcrit_risk_list[[i]] = data.frame("risk" = risk_vals[i], "y" = pcrit_risk, "x" = n)
        }
        ytarget = 100000
        #cat("USpop\t", USpop, "\n")
        pcrit_label=ytarget/USpop
        pcrit_lab_list = list()
        for (i in 1:length(risk_vals)){
          #cat("rv\t", risk_vals[i],"pc\t", pcrit_label, "\n")
            nlabel = log(1-risk_vals[i])/log(1-pcrit_label)
            pcrit_lab_list[[i]] = data.frame("risk" = risk_vals[i], "x" = nlabel, y = ytarget*1.4)
        }
        
        risk_vals_list = list()
        #cat("before risk_vals\n")
        for (i in 1:length(nvec)){
            p_equiv = nvec[i]/USpop;
            risk_vals_I = round(100*(1 - (1-p_equiv)**sizevec), 2)
            risk_vals_list[[i]] = data.frame("nvec" = nvec[i], "svec" = sizevec, "risk" = risk_vals_I)
            
        }
        #cat("after risk_vals\n")
        
        pcrit.df = do.call(rbind.data.frame, pcrit_risk_list)
        pcrit_lab.df = do.call(rbind.data.frame, pcrit_lab_list)
        risk.df = do.call(rbind.data.frame, risk_vals_list)
        
        validate(need(is.numeric(event_size), "Event size must be a number"),
                 need(event_size >= 5, "Event size must be >=0"),
                 need(event_size <= 100000, "Event size must be <= 100,000")
        )
        ylimits <- c(10**4, 3*10**6)
        angle = -45
        if (infect < 10**4){
            ylimits <- c(10, 3*10**4)
            angle <- -28
        } 
        
        #cat(infect, "-", ylimits,"\n")
        ggplot() + geom_point(data = risk.df, aes(x=svec, y=nvec)) + 
            # geom_text(data = pcrit_lab.df, aes(x=x, y = y, label=paste(risk * 100, "% Chance")), angle=angle, size=6) + 
            geom_hline(yintercept = risk.df$nvec, linetype=2) + 
          geom_path(data = pcrit.df, aes(x=x, y=y, group=risk, color=as.factor(risk*100)), size=1)+
            scale_color_manual(values=c("#fee5d9", "#fcae91", "#fb6a4a", "#de2d26", "#a50f15")) +
            # geom_segment(data=pcrit.df, aes(x=xstart, y=ystart, xend=xend, yend=yend)) +
            geom_label(data = risk.df, aes(x=svec, y=nvec, label = paste(ifelse(risk > 99,">99", round(risk, 1)), "% Chance")), nudge_y = .1, size=5, fill="blue", alpha=.5, color="white") +
            geom_vline(xintercept = event_size, linetype=3) + 
            # geom_hline(yintercept = nvec, linetype=3) + 
            geom_point(aes(x=event_size, y=nvec), size=4, shape=c(16, 17, 15), color="red") + 
            # geom_label_repel(aes(x=event_size, y=nvec, lrabel = paste(ifelse(risk > .99,">99", round(100*risk, 1)) , "% Chance someone is \ninfected with COVID19")), size=5) +
            # geom_polygon(aes(x=c(0, 0, 100), y=c(pcrit.df[1,]$ystart, 0, 0), group=c(1,1,1)), fill="grey", alpha = 0.5) +
            ggthemes::theme_clean() +
            # coord_cartesianxlim(1, 10**5) + ylim(ylimits)
            scale_x_continuous(name="Number of people at event", breaks = xblock, labels = names(xblock), trans = "log10", expand = c(.1, .1), ) +
            scale_y_continuous(name=paste("Number of circulating cases in ", state), breaks = yblock, labels = names(yblock), trans = "log10", expand = c(.1, .1)) + annotation_logticks(scaled=T) +
            # # geom_vline(xintercept = 10**5, linetype=2) + 
            coord_cartesian(ylim=ylimits, xlim = c(10,100001)) + 
            theme(
                axis.title.x = element_text(size=20),
                axis.text  = element_text(size=16),
                axis.title.y = element_text(size=20)
            ) + guides(color = guide_legend(title="% Chance"),override.aes = list(size = 2) )
        
    }) 
    })

})

