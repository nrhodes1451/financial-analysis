library(tidyverse)
library(quantmod)

df_ticker <- read_delim("data/nasdaqtraded.txt", "|") %>%
  filter(`Nasdaq Traded`=="Y",
         str_detect(Symbol, "^[A-Z0-9]+$"))
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
