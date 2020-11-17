library(tidyverse)
library(quantmod)
library(readxl)
library(data.table)
library(arrow)

merge_lse_dump <- function(
  dump_dir = "data/dump/lse/daily-prices",
  feather_path = "data/proc/lse/close.feather"
){
  fp <- dump_dir
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
  df %>% write_feather(feather_path)
}