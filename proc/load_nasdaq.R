load_nasdaq <- function(path = "data/proc/nasdaq/nasdaq.csv",
                        min_date = "2010-01-01"){
  headers <- read_csv(path, n_max = 1)
  headers <- "D" %>% str_c(rep("d", length(headers)-1) %>% str_c(collapse=""), collapse="")
  df <- read_csv(path, col_types = headers)
  
  names(df)[names(df)=="A.x"] <- "A"
  df <- df[!str_detect(names(df), "\\.")]
  
  df_long <- df %>% 
    pivot_longer(-date) %>% 
    rename(close=value) %>% 
    filter(!is.na(close))
  
  if(!is.null(min_date)){
    dmin <- df_long %>% 
      group_by(name) %>% 
      summarise(date = min(date))
    
    cos <- dmin %>% 
      filter(date < min_date) %>% .$name
    
    df_long <- df_long %>% 
      filter(name %in% cos)
  }
  
  return(df_long)
}