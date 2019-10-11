
library(tidyverse)
library(rvest)


# load journals -----------------------------------------------------------

journals <- readRDS("journals.RDS")

get_h5_index <- function(data, journal = journal) {
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
    if (length(h5_index[which(!is.na(h5_index))]) > 4) {
      return(NA)
    } else {
    h5_index[min(which(!is.na(h5_index)))]
    }
    
  }
  pb <- progress_estimated(nrow(data), 0)
  data <- data %>% 
    mutate(journal = str_remove_all(!!journal, " \\(.*\\)"),  # publishers are usually in parentheses and won't work with google scholar 
           h5_index = pmap_dbl(list(query = !!journal), h5_scraping, .pb = pb)) %>% 
    as_tibble()
  if (any(is.na(data$h5_index))) {
    warning("Check NA values for possible multiple entries. No exact match found.")
    data
  } else {
    data
  }
}


journals <- journals %>% 
  get_h5_index(journal = journal)

beepr::beep(8)

saveRDS(journals, "journals_h5.RDS")



# get_h5_index_table function ---------------------------------------------


get_h5_index_table <- function(data, journal = journal) {
  journal <- enquo(journal)
  h5_scraping_table <- function(query = query, .pb = NULL) {
    if (.pb$i < .pb$n) .pb$tick()$print()
    scholar_result <- read_html(paste0("https://scholar.google.com/citations?hl=de&view_op=search_venues&vq=", query, "&btnG="))
    xpath <- '//*[(@id = "gsc_mvt_table")]'    
    h5_index_table <- scholar_result %>% 
      html_nodes(xpath = xpath) %>%
      html_table()
  }
  pb <- progress_estimated(nrow(data), 0)
  data <- data %>% 
    group_by(group = rep(row_number(), length.out = n(), each = 5)) %>% 
    mutate(journal = str_replace_all(!!journal, "&", "%26"),
           string = str_replace_all(!!journal, " ", "+"),
           string = paste0('%22', string, '%22'),
           string = paste0(string, collapse = "+OR+")) %>% 
    select(group, string) %>% 
    distinct() %>% 
    mutate(table = pmap(list(query = string), h5_scraping_table, .pb = pb)) %>% 
    ungroup() %>% 
    select(table) %>% 
    unnest(table) %>% 
    unnest() 
  colnames(data) <- c("number", "journal", "h5_index", "h5_median")
  data %>% 
    select(-number)
}

journals_h5_table <- journals %>% 
  mutate(journal = str_replace_all(journal, "&amp;", "&"),
         journal = str_remove_all(journal, " \\(.*\\)"),
         journal = str_remove_all(journal, " \\:.*"),
         journal = str_remove_all(journal, " \\=.*")) %>% 
  get_h5_index_table()

beepr::beep(2)

saveRDS(journals_h5_table, "journals_h5_table.RDS")


# get missing journals ----------------------------------------------------

journal_check <- journals %>% 
  mutate(old_journal = journal,
         journal = str_replace_all(journal, "&amp;", "&"),
         journal = str_remove_all(journal, " \\(.*\\)"),
         journal = str_remove_all(journal, " \\:.*"),
         journal = str_remove_all(journal, " \\=.*"),
         journal = tolower(journal)) %>% 
  anti_join(journals_h5_table %>% 
              mutate(journal = tolower(journal))) %>% 
get_h5_index()

journals_still_missing <- journal_check %>% 
  filter(is.na(h5_index))

beepr::beep(2)

journals_still_missing <- journals_still_missing %>% 
  mutate(journal = str_remove_all(journal, "the "),
         journal = str_replace_all(journal, "&", "%26")) %>% 
  get_h5_index(journal = journal)

journal_check <- journal_check %>% 
  filter(!is.na(h5_index)) %>% 
  bind_rows(journals_still_missing %>% filter(!is.na(h5_index)))

journals_still_missing <- journals_still_missing %>% 
  filter(is.na(h5_index)) %>% 
  mutate(journal = str_replace_all(journal, "%26", "and")) %>% 
  get_h5_index(journal = journal)

journal_check <- journal_check %>% 
  bind_rows(journals_still_missing %>% filter(!is.na(h5_index)))

journals_still_missing <- journals_still_missing %>% 
  filter(is.na(h5_index))

journals_still_missing_2 <- journals_still_missing %>% 
  get_h5_index(journal = old_journal)

beepr::beep(2)

journal_check <- journal_check %>% 
  bind_rows(journals_still_missing_2 %>% filter(!is.na(h5_index)))

write_excel_csv(journals_still_missing_2 %>% filter(is.na(h5_index)), "journals_still_missing.csv", delim = ";")

journals_hand_lookup <- read_delim("journals_still_missing.csv", 
                                   ";", escape_double = FALSE, trim_ws = TRUE)

journal_check <- journal_check %>% 
  bind_rows(journals_hand_lookup %>% filter(!is.na(h5_index)))

check_by_hand <- journals_hand_lookup %>% 
  filter(is.na(h5_index)) %>% 
  left_join(data, by = c("old_journal" = "journal")) %>% 
  distinct(journal, .keep_all = TRUE)

write_excel_csv(check_by_hand, "check_by_hand.csv", delim = ";")
