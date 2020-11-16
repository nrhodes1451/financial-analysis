library(tidyverse)
library(zoo)

source('C:/Users/nrhod/Documents/Coding/R/stock-prices/proc/load_nasdaq.R')

df_fin <- read_csv("data/proc/nasdaq/financials.csv")
df_close <- read_feather("data/proc/nasdaq/nasdaq.feather")

get_pe <- function(t, df_fin,
                   df_close){
  message("Calculating price/earnings for ", t)
  fin <- df_fin %>%
    filter(ticker == t,
           metric %in% c(
             "Earnings Per Share",
             "Shares")) %>% 
    select(date, metric, value) %>%
    pivot_wider(names_from="metric", values_from="value") %>% 
    mutate(date = as.Date(paste0(date, "-01")) + months(1) - 1) %>% 
    filter(!is.na(date),
           !is.na(`Earnings Per Share`),
           !is.na(Shares))
  
  if(nrow(fin)==0){
    message("No data for ", t)
    return(NULL)
  }
  
  fill <- tibble(
    date = seq(fin$date[1], fin$date[nrow(fin)], by = 1)
  ) %>% 
    left_join(fin) %>% 
    mutate(
      `Earnings Per Share` = na.approx(`Earnings Per Share`),
      Shares = na.approx(Shares)
    )
  
  df <- df_close %>%
    filter(name == t) %>% 
    left_join(fill) %>% 
    filter(!is.na(Shares)) %>% 
    mutate(PE = close / `Earnings Per Share`)
}

tickers <- df_fin$ticker %>% unique

proc_pe <- function(tickers){
  for(t in tickers){
    fp <- paste0("data/dump/nasdaq/pe-ratio/", t, ".csv")
    if(file.exists(fp)){
      message("Skipping. File exists for ", t)
    }
    else{
      df <- get_pe(t, df_fin, df_close)
      if(!is.null(df)){
        df %>% write_csv(fp)
      }
    }
  }
}
