#' Scale the critical p-valyue by population size
#'
#' @param x Population size (integer)
#'
#' @return Scaled p-value \code{0.01/x}
#' @examples
#' pcrit(19)
#' pcrit(c(1,5,11))
pcrit <- function(x) {
    0.01 / as.integer(x)
}

#' Estimate risk of an individual having COVID19 given
#' the current 14-day infection rate, event size,
#' and population size from which we're drawing from
#'
#' @param num_infected 
#' @param event_size 
#' @param pop_size 
#' @param scaling_factor 
#'
#' @return Estimated risk
#' @export
#'
#' @examples
calc_risk <-
    function(num_infected,
             event_size,
             pop_size,
             scaling_factor = 10 / 14) {
        p_I <- (num_infected / pop_size) * scaling_factor
        r <- 1 - (1 - p_I) ** event_size
        round(100 * r, 1)
    }

#' Nicely round up large numbers to their nearest unit
#'
#' @param x Number to round
#' @param nice Powers of 10 to attempt to \code{ceil} too
#'
#' @return
#' @export
#'
#' @examples
roundUpNice <- function(x, nice = c(1, 2, 4, 5, 6, 8, 10)) {
    if (length(x) != 1) stop("'x' must be of length 1")
    10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}

#' Convert NULL to Unknown
#'
#' @param obj Object to check
#'
#' @return
#' @export
#'
#' @examples
str_or_unk <- function(obj){
    if(is.null(obj)){
        "Unknown"
    } else {
        obj
    }
}

#' Title
#'
#' @param input 
#' @param label 
#'
#' @return
#' @export
#'
#' @examples
make_resp_slider = function(input, label) {
    sliderInput(
        inputId = input,
        label = label,
        min = 0,
        max = 100,
        step = 1,
        value = 50,
        post = "%",
        width = '100%'
    )
}

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

riskParams = function(val) {
    case_when(val < 1 ~ "Not enough data",
              val > 99 ~ "> 99%",
              TRUE ~ as.character(glue::glue("{val}%")))
}

get_data <- function() {
    current_fh <- tail(list.files("states_current/", full.names = TRUE), 1)
    current_time <<- gsub(".csv", "", basename(current_fh))
    daily_fh <- tail(list.files("states_daily/", full.names = TRUE), 1)
    daily_time <<- gsub(".csv", "", basename(daily_fh))
    state_data <<- read.csv(current_fh, stringsAsFactors = F)
    states <<- unique(state_data$state)
    current_time <<- daily_time <<- Sys.Date()
    cur_date <- ymd(Sys.Date())-1
    past_date <- ymd(cur_date) - 14
    states_current <<- subset(state_data, ymd(date) == cur_date) %>% arrange(state)
    states_historic <<- subset(state_data, ymd(date) == past_date) %>% arrange(state)
    state_pops <<- vroom::vroom("map_data/state_pops.tsv")
    state_data <<- states_current %>%
        select(state, cases) %>%
        arrange(state)
    state_data$C_i <<- round((states_current$cases - states_historic$cases)  * 10 / 14)
    state_data$state <<- name2abbr[state_data$state]
    state_data <<- state_data %>% tidyr::drop_na()
    usa_counties <<- vroom::vroom('www/usa_risk_counties.csv') %>%
        select(-NAME, -stname) %>%
        mutate_at(vars(-GEOID, -state, -updated), as.numeric)
    usa_counties <<- county_geom %>% left_join(usa_counties, by = c("GEOID" = "GEOID"))
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


save_willingness <- function(source,
                             asc_bias,
                             event_size,
                             answer,
                             ip,
                             vacc_imm,
                             latitude,
                             longitude,
                             utm_source = "NULL", 
                             utm_medium = "NULL",
                             utm_content = "NULL", 
                             utm_campaign = "NULL") {
    
    
    sql <- "INSERT INTO willingness 
                (source, asc_bias, event_size, answer, 
                ip, vacc_imm, latitude, longitude, 
                utm_source, utm_medium, utm_content, 
                utm_campaign)
            VALUES 
                (?source, ?asc_bias, ?event_size, ?answer,
                ?ip, ?vacc_imm, ?latitude, ?longitude,
                NULLIF(?utm_source, 'NULL'), NULLIF(?utm_medium, 'NULL'), 
                NULLIF(?utm_content, 'NULL'), NULLIF(?utm_campaign, 'NULL'))
    "
    
    query <- sqlInterpolate(ANSI(), gsub("\\n\\w+", " ", sql),
                            source = source,
                            asc_bias = asc_bias,
                            event_size = event_size,
                            answer = answer,
                            ip = ip,
                            vacc_imm = vacc_imm,
                            latitude = str_or_unk(latitude),
                            longitude = str_or_unk(longitude),
                            utm_source = utm_source, 
                            utm_medium = utm_source,
                            utm_content = utm_source, 
                            utm_campaign = utm_source
                            )
    dbSendQuery(db, query)
    
}
