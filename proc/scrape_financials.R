library(tidyverse)
library(rvest)
library(rjson)

source('C:/Users/nrhod/Documents/Coding/R/stock-prices/proc/load_nasdaq.R')

parse_table <- function(df){
  df <- df %>% 
    mutate_all(as.character) %>%
    pivot_longer(-1, names_to = "date")
  df$dimension <- names(df)[1]
  names(df)[1] <- "metric"
  df
}
parse_ticker <- function(ticker){
  
  # url <- paste0("http://financials.morningstar.com/finan/financials/getKeyStatPart.html?&callback=?&t=",
  #               ticker)
  url <- paste0("http://financials.morningstar.com/finan/financials/getFinancePart.html?&callback=?&t=",
                ticker)
  data <- readLines(url)
  
  if(length(data)==0){
    return(NULL)
  }
  
  data <- substr(data, 3, nchar(data)-1)
  data <- rjson::fromJSON(data)
  tables <- data$componentData 
  if(is.null(tables)){
    return(NULL)
  }
  tables <- tables %>% 
    read_html() %>% 
    html_nodes("table") %>% 
    html_table
  
  df <- tables %>% lapply(function(tbl){
    df <- tbl[seq(2, nrow(tbl), by=2),]
    
    if(names(df)[1]==""){
      rowix <- which(apply(df, 1, function(x){length(unique(x))})==1)
      if(length(rowix) == 0){
        names(df)[1] <- "Financials"
        df <- parse_table(df)
      }
      else{
        df <- seq(rowix) %>% lapply(function(i){
          dfx <- df[(rowix[i]+1):(rowix[min(length(rowix), i+1)]-1),]
          names(dfx)[1] <- df[rowix[i], 1]
          dfx %>% parse_table
        }) %>% 
          do.call(what=rbind)
      }
    }
    else{
      df <- parse_table(df)
    }
    
    return(df)
  }) %>% 
    do.call(what=rbind)
  
  df$ticker <- ticker
  
  return(df)
}

scrape_financials <- function(){
  nasdaq <- load_nasdaq()
  tickers <- nasdaq$name %>% unique
  tickers <- tickers[order(tickers)]
  
  for(ticker in tickers){
    print(ticker)
    dump_path <- "data/dump/nasdaq/financials"
    fp <- file.path(dump_path,ticker) %>% paste0(".csv")
    if(file.exists(fp)){
      message("File exists. Skipping ", ticker)
    }
    else{
      df <- parse_ticker(ticker)
      if(is.null(df)){
        message("No data available for ", ticker)
      }
      else{
        write_csv(df, fp)
      }
    }
  }
}

merge_financials <- function(){
  fp <- "data/dump/nasdaq/financials"
  df <- list.files(fp) %>% 
    lapply(function(f){
      read_csv(file.path(fp, f))
    }) %>%
    do.call(what = rbind) %>% 
    mutate(metric = gsub(" %", "_%", metric)) %>% 
    mutate(metric = gsub(" ", "~", metric)) %>% 
    mutate(metric = gsub("\\s", "_", metric)) %>% 
    mutate(metric = gsub("~", " ", metric)) %>% 
    separate(metric, c("metric", "units"), "_") %>% 
    mutate(value = str_remove_all(value, ",")) %>% 
    mutate(value = as.numeric(value))
  
  df %>% write_csv("data/proc/nasdaq/financials.csv")
}