library(tidyverse)
library(quantmod)
library(readxl)
library(data.table)
library(arrow)

scrape_lse <- function(){
  df_ticker <- read_xlsx("data/raw/lse tickers.xlsx")
  tickerlist <- df_ticker$Code
  
  for(ticker in tickerlist){
    message("Fetching data for ", ticker)
    
    fp <- paste0("data/dump/lse/daily-prices/", ticker, ".csv")
    
    if(!file.exists(fp)){
      tryCatch({
        df <- getSymbols(paste0(ticker, ".L"), 
                         from = "1900-01-01",
                         to = "2020-12-31",
                         auto.assign = FALSE) %>% 
          as_tibble(rownames="date")
        df %>% write_csv(paste0("data/dump/lse/daily-prices/", ticker, ".csv"))
      }, error=function(e){})
    }
    else{
      message("already downloaded: skipping")
    }
  }
}

merge_lse_dump <- function(){
  fp <- "data/dump/lse/daily-prices"
  dflist <- list.files(fp) %>% 
    lapply(function(f){
      df <- read_csv(file.path(fp, f))
      ticker <- f %>% str_remove_all(".csv")
      names(df) <- str_remove_all(names(df), "^.*\\.")
      df %>% pivot_longer(-date, names_to="metric") %>% 
        mutate(name = ticker) %>% 
        select(name, date, metric, value)
    })
  dflist <- dflist[lapply(dflist, nrow) > 0]
  df <- rbindlist(dflist)
  df %>% write_feather("data/proc/lse/close.feather")
}

s