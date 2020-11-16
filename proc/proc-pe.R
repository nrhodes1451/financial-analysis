library(tidyverse)
library(lubridate)

tickers <- read_delim("data/nasdaqtraded.txt", delim="|") %>% 
  head(-1)

df_full <- read_csv("data/proc/nasdaq/nasdaq.csv")
df_longer <- df_full %>% 
  pivot_longer(-date) %>% 
  filter(!is.na(value))

tickers_5y <- df_longer %>% 
  group_by(name) %>% 
  summarise(date = min(date)) %>% 
  ungroup %>% 
  filter(date<"2015-01-01") %>% 
  .$name

