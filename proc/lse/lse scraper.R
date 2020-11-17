library(tidyverse)
library(quantmod)
library(readxl)
library(data.table)
library(arrow)
library(rvest)

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

scrape_fundamentals <- function(ticker,
                              name){
  page <- html(paste(
    "https://www.londonstockexchange.com/stock",
    ticker,
    name,
    "fundamentals",
    sep = "/")
  ) %>%
    html_nodes("#ng-lseg-state") %>% html_text()
  # fml 200k characters of garbage
  contents <- str_replace_all(page, "&q;", "\"") %>% 
    str_replace_all("&a;", "&") %>% 
    fromJSON
  names(contents)[1] <- "top"
  data <- contents$top$body$components[[3]]$status$childComponents[[2]]$content$fundamentals
  
  proc_table <- function(datalist){
    datalist$items %>% lapply(function(item){
      df_year <- item %>% lapply(function(i){
        if(class(i) == "list"){
          if(sum(names(i) == 
                 c("label","value","visibility","errorText"))==4){
            return(tibble("key"=i$label, "value"=i$value))
          }
        }
      }) %>% do.call(what=rbind) %>% 
        filter(nchar(key)>1) %>% 
        mutate(date_year_end = item$dateyearend$value,
               currency = item$currency$value)
      df_year
    }) %>%
      do.call(what=rbind)
  }
  
  data %>% names %>% lapply(function(section){
    data[[section]] %>% 
      proc_table %>% 
      mutate("section"=section)
  }) %>% 
    do.call(what = rbind)
}

ticker = "AA."
name = "aa-plc"

df_aa <- scrape_fundamentals(ticker, name)

# need lists of tickers / names