
library(xml2)
library(dplyr)
library(purrr)
library(tidyr)
library(rvest)

get_nlm <- function(data, issn = issn) {
  get_nlm_scrape <- function(data, issn = issn, .pb = NULL) {
    if (.pb$i < .pb$n) .pb$tick()$print()
    nlm_scrape <- read_html(paste0("https://www.ncbi.nlm.nih.gov/nlmcatalog/?term=", issn))
    xpath <- '//*[contains(concat( " ", @class, " " ), concat( " ", "nlmcat_entry", " " ))]'
    result <- nlm_scrape %>% 
      html_nodes(xpath = xpath) %>% 
      html_text() %>% 
      str_extract("(?<=Continued By: ).*\n.*\n") %>% 
      str_extract("[:digit:]*-(([:digit:]*)|X)")
    
    ifelse(is_empty(result), NA_character_, result)
  }
  pb <- progress_estimated(nrow(data), 0)
  data %>% 
    mutate(cont = pmap_chr(list(issn = issn), get_nlm_scrape, .pb = pb))
  
}

pubmed_journals <- pubmed_data_extract %>% 
  select(journal, issn) %>% 
  distinct() %>% 

lookup <- still_missing_by_hand %>% 
  left_join(pubmed_journals) 
  
lookup_cont <- lookup %>% 
 get_nlm()

write_excel_csv(lookup_cont, "look_up.csv")

lookup_cont <- read_csv("look_up.csv")

sjr_journals_join_cont <- sjrdata %>% 
  select(year, issn, cites_doc_2years, sjr_best_quartile, sjr_journal) %>% 
  filter(issn != "-") %>% # remove unknown issn
  mutate(cites_doc_2years = str_replace(cites_doc_2years, ",", "\\.") %>% as.numeric) %>%  # transform to numeric
  separate(issn, into = c("issn_1", "issn_2"), sep = ", ") %>% 
  gather(issn_col, issn, -year, -sjr_best_quartile, -cites_doc_2years, -sjr_journal) %>% 
  filter(!is.na(issn)) %>% 
  select(-issn_col) %>% 
  mutate(issn = str_c(str_sub(issn, start = 1, end = 4), 
                      "-", 
                      str_sub(issn, start = 5, end = -1)),
         cites_doc_2years = str_replace(cites_doc_2years, ",", "\\.") %>% as.numeric)

lookup_cont <- lookup_cont %>% 
  mutate(cont = str_pad(cont, width = 9, side = "right", pad = "X")) %>% 
  left_join(sjr_journals_join_cont %>% select(issn, sjr_journal), by = c("cont" = "issn")) %>% 
  mutate(sjr_journal.x = if_else(!is.na(sjr_journal.y), sjr_journal.y, sjr_journal.x)) %>% 
  distinct()

write_excel_csv2(lookup_cont, "look_up_sjr.csv")


