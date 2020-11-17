library(tidyverse)
library(quantmod)
library(readxl)
library(data.table)
library(arrow)

# Combine all data sources into a final frame

# Stock tickers, along with category etc.

# Institutional ownership %



# Daily stock prices and volumes
df_daily <- read_feather("data/proc/lse/close.feather")

s