
library(tidyverse)
library(rvest)

get_h5_index <- function(data, journal) {
  journal <- enquo(journal)
  h5_scraping <- function(query = query, .pb = NULL) {
    if (.pb$i < .pb$n) .pb$tick()$print()
    query <- str_replace_all(paste0('"', query, '"'), " ", "+") # adding quotation marks for exact search
    scholar_result <- read_html(paste0("https://scholar.google.com/citations?hl=de&view_op=search_venues&vq=", query, "&btnG="))
    xpath <- '//*[contains(concat( " ", @class, " " ), concat( " ", "gsc_mvt_n", " " ))] | //*[contains(concat( " ", @class, " " ), concat( " ", "gsc_mp_anchor", " " ))]'
    h5_index <- suppressWarnings( # message about NA which is expected
      scholar_result %>% 
        html_nodes(xpath = xpath) %>%
        html_text() %>%
        as.numeric())
    h5_index[min(which(!is.na(h5_index)))]
  }
  pb <- progress_estimated(nrow(data), 0)
  data %>% 
    mutate(journal = str_remove_all(!!journal, " \\(.*\\)"),  # publishers are usually in parentheses and won't work with google scholar 
           h5_index = pmap_dbl(list(query = !!journal), h5_scraping, .pb = pb))
}

journals <- data %>% 
  select(journal) %>% 
  distinct()

journals <- journals %>% 
  get_h5_index(journal = journal)

beepr::beep(8)

