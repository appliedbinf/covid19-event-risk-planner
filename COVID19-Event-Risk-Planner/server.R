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

pcrit = function(x){0.01/x}
USpop=330*10^6;
n=logspace(1,5,100);
pcrit_val=pcrit(n);
numcrit=pcrit_val*USpop;
sizevec= c(10, 100, 1000, 10000, 100000)
risk_vals = c(0.01, 0.02,  0.1, 0.5, 0.9)
pcrit_risk_list = list()
for (i in 1:length(risk_vals)){
    pcrit_risk = 1 - (1-risk_vals[i])**(1/n)
    pcrit_risk = pcrit_risk*USpop
    pcrit_risk_list[[i]] = data.frame("risk" = risk_vals[i], "ystart" = max(pcrit_risk), "yend" = min(pcrit_risk), "xstart" = 1, "xend" = max(n)  )
}


ytarget = 100000;
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

calc_risk = function(I,n){
    p_I = I/USpop
    1-(1-p_I)**n
}



ylimits = c(10**4, 3*10**7)
xblock = c(10, 100, 1000, 10**4, 10**5)
yblock = c(10**5, 4*10**5, 10**6, 2*10**6, 8*10**6)
names(xblock) <- c("10\nDinner party", "100\nWedding reception", "1,000\nSmall concert", "10,000\nSoccer match", "100,000\nNFL game" )
names(yblock) <- c("100,000", "400,000", "1 million", "2 million", "8 million")

l.rise =  pcrit.df$yend - pcrit.df$ystart
l.run =  pcrit.df$xend
l.slope = l.rise/l.run
l.x = (140000*1.4 - pcrit.df$ystart)/l.slope



shinyServer(function(input, output, session) {

    values <- reactiveValues(infect = 200000, event_size = 275)
    observeEvent(input$calc, {
        req(input$event_size)
        req(input$infect)
        # validate(need(is.numeric(event_size), "Event size must be a number"),
        #          need(event_size > 0, "Event size must be >0"),
        #          need(event_size <= 100000, "Event size must be <= 100,000"),
        #          need(is.numeric(infect), "Number of active cases must be a number"),
        #          need(infect > 0, "Number of active cases must be >0"),
        #          need(infect < 8*10**6, "Number of active cases must be <8,000,000")
        #
        event_size = isolate(input$event_size)
        # validate(need(is.numeric(event_size), "Event size must be a number"),
        #          need(event_size > 0, "Event size must be >0"),
        #          need(event_size <= 100000, "Event size must be <= 100,000"),
        #          need(is.numeric(infect), "Number of active cases must be a number"),
        #          need(infect > 0, "Number of active cases must be >0"),
        #          need(infect < 8*10**6, "Number of active cases must be <8,000,000")
        #
        values$event_size = as.numeric(gsub("[ ,_]", "", event_size))
        infect = isolate(input$infect)
        values$infect = as.numeric(gsub("[ ,_]", "", infect))

    })
    # 
    # cat(infect, " - ", event_size)

    
    output$plot <- renderPlot({
        infect <- values$infect
        event_size <- values$event_size
        risk <- calc_risk(infect, event_size)
        ggplot() + geom_point(data = risk.df, aes(x=svec, y=nvec)) + 
        geom_text(data = pcrit_lab.df, aes(x=c(9, 20, 200, 2000, 7000), y = 80000, label=paste(risk * 100, "% chance")), angle=-45, size=6) + 
        geom_hline(yintercept = risk.df$nvec, linetype=2) + 
        geom_segment(data=pcrit.df, aes(x=xstart, y=ystart, xend=xend, yend=yend)) +
        geom_label(data = risk.df, aes(x=svec, y=nvec, label = paste(ifelse(risk > 99,">99", round(risk, 1)))), nudge_y = .1, size=5, fill="white", alpha=.75) + 
        geom_vline(xintercept = values$event_size, linetype=2) + 
        geom_hline(yintercept = values$infect, linetype=2) + 
        geom_point(aes(x=event_size, y=infect), size=4, color="red") + 
        geom_label_repel(aes(x=event_size, y=infect, label = paste(ifelse(risk > .99,">99", round(100*risk, 1)) , "% Chance someone is \ninfected with COVID19"))) + 
        # geom_polygon(aes(x=c(0, 0, 100), y=c(pcrit.df[1,]$ystart, 0, 0), group=c(1,1,1)), fill="grey", alpha = 0.5) +
        ggthemes::theme_clean() + 
        # coord_cartesianxlim(1, 10**5) + ylim(ylimits)
        scale_x_continuous(name="", breaks = xblock, labels = names(xblock), trans = "log10", expand = c(.1, .1)) + 
        scale_y_continuous(name="", breaks = yblock, labels = names(yblock), trans = "log10", expand = c(.1, .1)) + 
        # geom_vline(xintercept = 10**5, linetype=2) + 
        coord_cartesian(ylim=ylimits, xlim = c(10,100001))
  
    }) 

})

