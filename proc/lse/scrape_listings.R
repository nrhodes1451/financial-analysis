scrape_listings <- function(pages=112){
  root <- "https://www.londonstockexchange.com/live-markets/market-data-dashboard/price-explorer?categories=EQUITY&showonlylse=true&page="
  # 112 pages currently
  for(i in seq(pages)){
    fp <- paste0("data/dump/lse/listings/", i, ".csv")
    if(!file.exists(fp)){
      page <- root %>% 
        paste0(i)
      message("Scraping ", page)
      contents <- page %>% 
        html %>% 
        html_nodes("#ng-lseg-state") %>% 
        html_text %>%
        str_replace_all("&q;", "\"") %>% 
        str_replace_all("&a;", "&") %>% 
        fromJSON
      
      names(contents)[1] <- "top"
      listings <- contents$top$body$components[[3]]$content$priceExplorerResults
      
      df <- listings %>% lapply(function(l){
        names(l) %>% lapply(function(field){
          val <- l[[field]]$value
          if(is.null(val)) val <- "-"
          tibble(
            "key" = field,
            "value" = val
          )
        }) %>%
          do.call(what=rbind) %>% 
          mutate(code = l$tidm$value)
      }) %>% 
        do.call(what=rbind)
      
      df %>% write_csv(fp)
    }
    else{
      message("Skipping ", i, " as file exists")
    }
  }
}

combine_listings <- function(){
  list.files("data/dump/lse/listings") %>% 
    lapply(function(f){read_csv(file.path("data/dump/lse/listings", f))}) %>% 
    do.call(what=rbind)
}
