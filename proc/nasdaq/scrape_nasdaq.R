library(tidyverse)
library(quantmod)

load_nasdaq_tickers <- function(){
  read_delim("data/raw/nasdaqtraded.txt", "|") %>%
    filter(`Nasdaq Traded`=="Y",
           str_detect(Symbol, "^[A-Z0-9]+$"))
}

scrape_nasdaq <- function(){
  df_tickers <- load_nasdaq_tickers()
}

tickerlist <- df_ticker$Symbol # %>% sample(100)

for(ticker in tickerlist){
  message("Fetching data for ", ticker)
  
  fp <- paste0("data/dump/", ticker, ".csv")
  
  if(!file.exists(fp)){
    tryCatch({
      df <- getSymbols(ticker, 
                       from = "1900-01-01",
                       to = "2020-12-31",
                       auto.assign = FALSE) %>% 
        as_tibble(rownames="date")
      df %>% write_csv(paste0("data/dump/", ticker, ".csv"))
    }, error=function(e){})
  }
  else{
    message("already downloaded: skipping")
  }
}
