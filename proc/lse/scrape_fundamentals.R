source('proc/lse/scrape_listings.R')

scrape_fundamentals <- function(ticker,
                                name){
  page <- html(paste(
    "https://www.londonstockexchange.com/stock",
    ticker,
    name,
    "fundamentals",
    sep = "/")
  ) %>%
    html_nodes("#ng-lseg-state") %>% html_text()
  
  contents <- str_replace_all(page, "&q;", "\"") %>% 
    str_replace_all("&a;", "&") %>% 
    fromJSON
  names(contents)[1] <- "top"
  components <- contents$top$body$components[[3]]$status$childComponents
  if(length(components) > 1){
    data <- components[[2]]$content$fundamentals
    
    proc_table <- function(datalist){
      datalist$items %>% lapply(function(item){
        df_year <- item %>% lapply(function(i){
          if(class(i) == "list"){
            if(sum(names(i) == 
                   c("label","value","visibility","errorText"))==4){
              return(tibble("key"=i$label, "value"=i$value))
            }
          }
        }) %>% do.call(what=rbind) %>% 
          filter(nchar(key)>1) %>% 
          mutate(date_year_end = item$dateyearend$value,
                 currency = item$currency$value)
        df_year
      }) %>%
        do.call(what=rbind)
    }
    
    df <- data %>% names %>% lapply(function(section){
      data[[section]] %>% 
        proc_table %>% 
        mutate("section"=section)
    }) %>% 
      do.call(what = rbind)
    return(df)
  }
  else{
    return(NULL)
  }
}

scrape_all <- function(){
  ticker_list <- combine_listings() %>% 
    filter(key=="issuername") %>%
    arrange(code) %>% 
    mutate(name = str_replace_all(tolower(value), " ", "-")) %>% 
    mutate(name = str_remove_all(name, "[\"\\(\\)&/]")) %>% 
    mutate(name = str_replace_all(name, "-+", "-"))
  
  for(i in seq(nrow(ticker_list))){
    ticker <- ticker_list$code[i]
    name <- ticker_list$name[i]
    fp <- paste0("data/dump/lse/fundamentals/", ticker, ".csv")
    if(file.exists(fp)){
      message("Skipping ", ticker, " as file already exists")
    }
    else{
      message("Fetching data for ", ticker)
      df <- scrape_fundamentals(ticker, name)
      if(is.null(df)){
        message("No data found")
      }
      else{
        df %>% write_csv(fp)
      }
    }
  }
}
