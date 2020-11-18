library(dplyr)
library(lubridate)


data_update <<- NULL

pop_data_fh = "data/county-population.csv"
pop_data = read.csv(pop_data_fh)[,-1]
zip2fips_fh = "data/ZIP-COUNTY-FIPS_2017-06.csv"
zip2fips = read.csv(zip2fips_fh)
dataurl <-
  "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"


get_data <- function() {
  data <-
    read.csv(dataurl, stringsAsFactors = FALSE) %>% mutate(date = as_date(date))
  cur_date <- ymd(gsub("-", "", Sys.Date())) - 1
  past_date <- ymd(cur_date) - 14
  data_cur <- data %>%
    filter(date == cur_date) %>%
    mutate(fips = case_when(county == "New York City" ~ 99999,
                            TRUE ~ as.numeric(fips))) %>%
    select(c(fips, cases_cur = cases, deaths))
  data_past <- data %>%
    filter(date == past_date) %>%
    mutate(fips = case_when(county == "New York City" ~ 99999,
                            TRUE ~ as.numeric(fips))) %>%
    select(fips = fips, cases_past = cases)
  data_join <- data_cur %>%
    inner_join(data_past, by = "fips") %>%
    inner_join(pop_data, by = "fips") %>%
    mutate(cases = round((cases_cur - cases_past) * 10 / 14)) %>%
    as.data.frame()
  assign("data_update", now(), envir=.GlobalEnv)
  assign("api_data", data_join, envir=.GlobalEnv)
}
get_data()

#* Lookup risk
#* @get /<zip:str>/<asc_bias:int>/<event_size:int>
function(zip, asc_bias, event_size) {
  if (is.null(data_update) || difftime(now(), data_update, units="hours") > 4){
  	get_data()
  }
  print(api_data)
  asc_bias = abs(asc_bias)
  if (!zip %in% zip2fips$ZIP) {
    return(paste("Invalid zipcode"))
  }
  if (asc_bias < 1 || asc_bias > 15) {
    return(paste("Please provide an ascertainment bias between 1 - 15"))
  }
  if (event_size < 5 || event_size > 5000) {
    return(paste("Please provide an event size between 5 - 5000"))
  }
	if (is.null(api_data)) {
    return(paste(":("))
  }

  results = api_data %>%
    dplyr::filter(fips == zip2fips[zip2fips$ZIP == zip, "STCOUNTYFP"]) %>%
    mutate(adj_cases = round(cases * asc_bias),
           risk = round((1 - (
             1 - (adj_cases / pop)
           ) ** event_size) * 100 , 1)) %>%
    select(cases_cur, cases_past, adj_cases, population = pop, risk)
  
  return(as.list(results))
  
}
