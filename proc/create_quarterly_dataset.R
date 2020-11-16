library(tidyverse)
library(arrow)
library(lubridate)

df_fin <- read_csv("data/proc/nasdaq/financials.csv")
df_close <- read_feather("data/proc/nasdaq/nasdaq.feather")
lookup <- read_

proc_quarterly <- function(){
  df <- read_feather("data/proc/nasdaq/pe.feather") %>% 
    mutate(
      quarter = paste0(year(date), "Q", ceiling(month(date)/3))
    ) %>% 
    group_by(quarter, name) %>% 
    summarise_all(list(mean)) %>% 
    ungroup
}
