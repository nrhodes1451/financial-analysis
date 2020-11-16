library(tidyverse)
library(zoo)

source('C:/Users/nrhod/Documents/Coding/R/stock-prices/proc/load_nasdaq.R')

df_fin <- read_csv("data/proc/nasdaq/financials.csv")
df_close <- read_feather("data/proc/nasdaq/nasdaq.feather")

t <- "AAPL"

fin <- df_fin %>%
  filter(ticker == t,
         metric %in% c(
           "Earnings Per Share",
           "Shares")) %>% 
  select(date, metric, value) %>%
  pivot_wider(names_from="metric", values_from="value") %>% 
  mutate(date = as.Date(paste0(date, "-01")) + months(1) - 1) %>% 
  filter(!is.na(date))
fill <- tibble(
  date = seq(fin$date[1], fin$date[nrow(fin)], by = 1)
) %>% 
  left_join(fin) %>% 
  mutate(
    `Earnings Per Share` = na.approx(`Earnings Per Share`),
    Shares = na.approx(Shares)
  )

df <- df_close %>%
  filter(name == t) %>% 
  left_join(fill) %>% 
  filter(!is.na(Shares)) %>% 
  mutate(PE = close / `Earnings Per Share`)
