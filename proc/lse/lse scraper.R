library(tidyverse)
library(quantmod)
library(readxl)
library(data.table)
library(arrow)
library(rvest)
library(rjson)

source('proc/lse/scrape_fundamentals.R')

scrape_lse <- function(
  ticker_list = "data/raw/lse tickers.xlsx",
  dump_dir = "data/dump/lse/daily-prices/"
){
  df_ticker <- read_xlsx(ticker_list)
  tickerlist <- df_ticker$Code
  
  for(ticker in tickerlist){
    message("Fetching data for ", ticker)
    
    fp <- paste0(dump_dir, ticker, ".csv")
    
    if(!file.exists(fp)){
      tryCatch({
        df <- getSymbols(paste0(ticker, ".L"), 
                         from = "1900-01-01",
                         to = "2020-12-31",
                         auto.assign = FALSE) %>% 
          as_tibble(rownames="date")
        df %>% write_csv(fp)
      }, error=function(e){})
    }
    else{
      message("already downloaded: skipping")
    }
  }
}