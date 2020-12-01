library(tidyverse)
library(readxl)

proc_issuer_list <- function(){
  df <- readxl::read_xlsx(
    "data/raw/lse/issuer list.xlsx",
    skip = 3) %>% 
    filter(!is.na(`Market`)) %>% 
    mutate(`Admission Date` = as.Date(`Admission Date`))
  df %>% write_csv("data/proc/lse/index.csv")
}