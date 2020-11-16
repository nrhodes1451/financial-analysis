library(tidyverse)
library(quantmod)

directory <- "data/dump/lse"

concatenate <- function(directory){
  files <- dir(directory)
  
  fetch_file <- function(path){
    ticker <- str_extract(path, "^[A-Z0-9]+")
    message(ticker)
    df <- read_csv(file.path(directory, path))
    names(df) <- str_remove_all(names(df), paste0(ticker,"\\."))
    df <- df %>% select(date, Close)
    names(df)[2] <- ticker
    return(df)
  }
  
  dfs <- files %>% lapply(fetch_file)
  
  df <- dfs %>% reduce(full_join)
}

concatenate(directory) %>% 
  write_csv("data/proc/lse/lse.csv")
