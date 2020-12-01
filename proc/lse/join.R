library(tidyverse)
library(arrow)

# load categorical data
df_index <- read_csv("data/proc/lse/index.csv")
df_listings <- read_csv("data/proc/lse/listings.csv")
df_df_lookups <- read_csv("data/proc/lse/lookups.csv") %>% 
  rename(code = `listings code`,
         `Company Name` = `index name`) %>% 
  select(-`lookup name`)

# join on lookups table
df_listings <- df_listings %>%
  left_join(df_lookups) %>% 
  left_join(df_index)
df_listings %>% write_feather("data/proc/lse/full_list.feather")