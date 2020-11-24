library(tidyverse)
library(quantmod)
library(readxl)
library(data.table)
library(arrow)

# Process large datasets into monthly/quarterly data files for analysis

# Monthly stock prices and volumes
proc_close <- function(){
  df <- read_feather("data/proc/lse/close.feather") %>% 
    mutate(date = strftime(date, "%Y-%m")) %>%
    filter(!is.na(value)) %>% 
    group_by(name, date, metric) %>% 
    summarise(value = mean(value)) %>% 
    ungroup %>% 
    write_feather("data/proc/lse/quarterly/close.feather")
}

# Fundamentals
proc_fundamentals <- function(){
  df_fundamentals <- "data/dump/lse/fundamentals" %>% 
    list.files() %>% 
    lapply(function(f){
      read_csv(file.path("data/dump/lse/fundamentals", f)) %>% 
        mutate(code = str_remove(f, ".csv"))
    }) %>% 
    do.call(what=rbind)
  
  fundamentals <- df_fundamentals$section %>% 
    unique %>% 
    sapply(function(s){
      # handle duplicate values
      df <- df_fundamentals %>% 
        filter(section == s) %>% 
        unique
      dupes <- df %>% 
        group_by(date_year_end, currency, section, code, key) %>% 
        tally %>%
        filter(n>1) %>% 
        ungroup %>% 
        left_join(df) %>% filter(
          value == "-"
        ) %>% 
        select(-n)
      df %>%
        anti_join(dupes) %>% 
        pivot_wider(names_from = key, values_from = value)
    }, USE.NAMES = TRUE)
  
  fundamentals$incomeStatement %>% write_feather("data/proc/lse/quarterly/income_statement.feather")
  fundamentals$balanceSheet %>% write_feather("data/proc/lse/quarterly/balance_sheet.feather")
  fundamentals$ratios %>% write_feather("data/proc/lse/quarterly/ratios.feather")
}
