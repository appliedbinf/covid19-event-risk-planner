library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(ggthemes)
library(jsonlite)
library(leaflet)
library(leaflet.extras)
library(lubridate)
library(mapview)
library(matlab)
library(RCurl)
library(rtweet)
library(sf)
library(withr)


get_token()

pcrit <- function(x) {
  0.01 / x
}

calc_risk <- function(I, n, pop) {
  p_I <- I / pop
  1 - (1 - p_I)**n
}

# https://stackoverflow.com/questions/6461209/how-to-round-up-to-the-nearest-10-or-100-or-x
roundUpNice <- function(x, nice = c(1, 2, 4, 5, 6, 8, 10)) {
  if (length(x) != 1) stop("'x' must be of length 1")
  10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}

`%nin%` <- Negate(`%in%`)
current_fh <- tail(list.files("states_current/", full.names = TRUE), 1)
current_time <- gsub(".csv", "", basename(current_fh))
dir.create(paste0('www/daily_risk_plots/', current_time))

daily_fh <- tail(list.files("states_daily/", full.names = TRUE), 1)
daily_time <- gsub(".csv", "", basename(daily_fh))
state_current <<- read.csv(current_fh, stringsAsFactors = F)
states <- state_current %>% filter(state %nin% c('AS', 'MP', 'VI', 'GU', 'PR', 'DC'))
states = states$state
cur_date <- gsub("-", "", Sys.Date())
past_date <- ymd(cur_date) - 14
states_historic <- read.csv(daily_fh, stringsAsFactors = F) %>%
    filter(ymd(date) == past_date) %>% 
    arrange(state)

state_pops <- read.delim("state_pops.tsv", header = T, sep = "\t", stringsAsFactors = F)
state_data <- state_current %>%
    select(state, positive) %>%
    arrange(state)
state_data$C_i <- state_data$positive - states_historic$positive


xblock <- c(10, 50, 100, 1000, 10000)
state <- "GA"
rl = list()
ci_list = list()
for (state in states){
    pop <- as.numeric(state_pops[state_pops$state == state, "pop"])
    C_i <- as.numeric(state_data[state_data$state == state, "C_i"])
    ci_list[[state]] = data.frame("state" = state, "ci" = C_i/pop, "realci" = C_i)
    nvec <- c(C_i, 5 * C_i, 10 * C_i, 20 * C_i)
    event_size = c(10, 100, 1000, 1000)
    risk <- calc_risk(nvec, event_size, pop)

    n <- logspace(0, 6, 100)
    pcrit_val <- pcrit(n)
    numcrit <- pcrit_val * pop
    sizevec <- xblock
    risk_vals <- c(0.01, 0.02, 0.1, 0.5, 0.9)
    
    risk_vals_list <- list()
    sizes = logspace(0, 6, 50)
        for (i in 1:length(nvec)) {
            p_equiv <- nvec[i] / pop
            risk_vals_I <- round(100 * (1 - (1 - p_equiv)**sizes), 2)
            risk_vals_list[[i]] <- data.frame("nvec" = nvec[i], "svec" = sizes, "risk" = risk_vals_I, "state" = state)
        }
        
        risk.df <- do.call(rbind.data.frame, risk_vals_list)
              
        risk.df$nvec <- factor(risk.df$nvec, levels=c(C_i, 5 * C_i, 10 * C_i, 20 * C_i))
    
          
    rl[[state]] = risk.df
    
    
}

ci = do.call(rbind.data.frame, ci_list)
ci$rank = rank(ci$ci)

ci = ci[order(ci$rank),]



plots = list()
for (state in names(rl)){
    rank_ = grep(state, ci$state)   
  p = ggplot(rl[[state]]) +
        geom_area(aes(x = svec, y = risk, group = rev(nvec), fill = nvec), position="identity") +
        scale_fill_manual(values = c("white", "white", "grey", "grey50")) +
        geom_path( aes(x = svec, y = risk, group = nvec, color = nvec), size=c(rep(1, 50), rep(12, 150)), linetype=c(rep(2, 50), rep(1, 150)), position="identity")   +
        scale_color_manual(values = c("black", "#003057", "#EAAA00", "red" ), labels = c(bquote('C'["i"]), bquote('5x C'["i"]), bquote('10x C'["i"]), bquote('20x C'["i"]))) +
      scale_x_continuous(name = "Event size", breaks = xblock, labels = format(xblock, big.mark = ",", trim = T), trans = "log10", expand = c(.1, .1))  +
      theme_clean() +
    annotation_logticks(scaled = , sides = "b") + 
      guides(color = guide_legend(title = "% Chance",  , override.aes = list(size = 50)), fill = F) +
    labs(title = paste0(state, " (#", rank_, ")"), y="Estimated risk") +
    theme(
      axis.title.x = element_text(size = 42),
      axis.text = element_text(size = 42),
      axis.title.y = element_text(size = 42),
      plot.caption = element_text(hjust = 0, face = "italic"),
      plot.caption.position = "plot",
      plot.title = element_text(hjust = 0.5, size = 100),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_text(size=120),
      legend.text = element_text(size=100)
    ) + coord_cartesian(xlim = c(10, 10001), ylim = c(0,100))
    
    
    plots[[state]] = p
    
        p2 = ggplot(rl[[state]]) +
geom_area(aes(x = svec, y = risk, group = rev(nvec), fill = nvec), position="identity") +
        scale_fill_manual(values = c("white", "white", "grey", "grey50")) +
        geom_path( aes(x = svec, y = risk, group = nvec, color = nvec), size=c(rep(1, 50), rep(2, 150)), linetype=c(rep(2, 50), rep(1, 150)), position="identity")   +
        scale_color_manual(values = c("black", "#003057", "#EAAA00", "red" ), labels = c(bquote('C'["i"]), bquote('5x C'["i"]), bquote('10x C'["i"]), bquote('20x C'["i"]))) +
      scale_x_continuous(name = "Event size", breaks = xblock, labels = format(xblock, big.mark = ",", trim = T), trans = "log10", expand = c(.1, .1))  +
      theme_clean() +
    annotation_logticks(scaled = , sides = "b") + 
      guides(color = guide_legend(title = "% Chance",  ), override.aes = list(size = 50), fill = F) +
    labs(title = paste0(state, " (#", rank_, ")"), 
             caption = paste0("© CC-BY-4.0\tChande, A.T., Gussler, W., Harris, M., Lee, S., Rishishwar, L., Jordan, I.K., Andris, C.M., and Weitz, J.S. 'Interactive COVID-19 Event Risk Assessment Planning Tool'\nhttp://covid19risk.biosci.gatech.edu\nData updated on and risk estimates made:  ", ymd_hms(current_time), "\nCurrent reported incidence = ", ci[state,'realci'], "\nReal-time COVID19 data comes from the COVID Tracking Project: https://covidtracking.com/api/"),
         y="Estimated risk") +
    theme(
          axis.title.x = element_text(size = 20),
          axis.text = element_text(size = 16),
          axis.title.y = element_text(size = 20),
          plot.caption = element_text(hjust = 0, face = "italic"),
          plot.caption.position = "plot",
          plot.title = element_text(hjust = 0.5, size = 20),
          plot.subtitle = element_text(hjust = 0.5),
          plot.background = element_blank()
        ) + 
        coord_cartesian(xlim = c(10, 10001), ylim = c(0,100)) 
    ggsave(paste0('www/daily_risk_plots/', current_time,"/",state, ".png" ), plot = p2, width = 12, height = 7, units = "in", dpi = 320)

    
}

p <- ggarrange(plotlist = plots[ci$state], nrow = 10, ncol = 5, common.legend = T)
png(paste0('www/daily_risk_plots/', current_time,"/states-rank.png" ), width = 6600, height = 7100, units = 'px')
annotate_figure(p,
                top = text_grob("US COVID19 Continuous Risk Estimates, by rank", face = "bold", size = 140),
                bottom = text_grob(paste0("© CC-BY-4.0\tChande, A.T., Gussler, W., Harris, M., Lee, S., Rishishwar, L., Jordan, I.K., Andris, C.M., and Weitz, J.S. 'Interactive COVID-19 Event Risk Assessment Planning Tool'\nhttp://covid19risk.biosci.gatech.edu\nData updated on and risk estimates made:  ", ymd_hms(daily_time, tz=""),  "\nReal-time COVID19 data comes from the COVID Tracking Project: https://covidtracking.com/api/"),
                                   hjust=0, size=80, x = 0)
)
dev.off()

p <- ggarrange(plotlist = plots, nrow = 10, ncol = 5, common.legend = T)
png(paste0('www/daily_risk_plots/', current_time,"/states-alpha.png" ), width = 6600, height = 7100, units = 'px')
annotate_figure(p,
                top = text_grob("US COVID19 Continuous Risk Estimates, alphabetical", face = "bold", size = 140),
                bottom = text_grob(paste0("© CC-BY-4.0\tChande, A.T., Gussler, W., Harris, M., Lee, S., Rishishwar, L., Jordan, I.K., Andris, C.M., and Weitz, J.S. 'Interactive COVID-19 Event Risk Assessment Planning Tool'\nhttp://covid19risk.biosci.gatech.edu\nData updated on and risk estimates made:  ", ymd_hms(daily_time, tz=""), "\nReal-time COVID19 data comes from the COVID Tracking Project: https://covidtracking.com/api/"),
                                   hjust=0, size=80, x = 0)
)
dev.off()

post_tweet(status=paste0("State-level risk estimate update for ", ymd_hms(current_time)), media=paste0('www/daily_risk_plots/', current_time,"/states-rank.png" ))


