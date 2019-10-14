
library(tidyverse)
library(janitor)
library(fuzzyjoin)


# scimago downloaded ------------------------------------------------------

# data was downloaded from the scimago website for all available years (1999 - 2018)
# frist step: loading all data and merging in one data frame
data_path <- "scimago_data"

files <- dir(data_path, pattern = "*.csv")

scimago_raw <- tibble(year = files) %>%
  mutate(file_contents = 
           map(year, ~ read_delim(file.path(data_path, .), ";", escape_double = FALSE, trim_ws = TRUE))) 

sjrdata <- scimago_raw %>% 
  mutate(year = str_extract(year, "\\-*\\d+\\.*\\d") %>% as.numeric()) %>% 
  unnest(file_contents) %>% 
  clean_names() %>% 
  rename(sjr_journal = title)
  
saveRDS(sjrdata, "sjrdata.RDS")

# later in the analysis this error showed up for special character (Ä); correcting this here:
sjrdata$sjr_journal[which(sjrdata$sjr_journal == "Deutsches ?rzteblatt international")] <- "Deutsches Ärzteblatt international"

# selecting year and issn as identifiers
sjr_journals <- sjrdata %>% 
  select(year, issn, cites_doc_2years, sjr_best_quartile, sjr_journal) %>% 
  filter(issn != "-") %>% # remove unknown issn
  mutate(cites_doc_2years = str_replace(cites_doc_2years, ",", "\\.") %>% as.numeric) # transform to numeric 
  
  
# checking for duplicates 
sjr_dupes <- sjr_journals %>% 
  semi_join(sjr_journals %>% 
              select(year, issn) %>% 
              get_dupes())

# gathering issn numbers (sometimes >1 per journal) and bring the in format to be joined with pubmed data
sjr_dupes <- sjr_dupes %>% 
  separate(issn, into = c("issn_1", "issn_2"), sep = ", ") %>% 
  gather(issn_col, issn, -year, -sjr_best_quartile, -cites_doc_2years) %>% 
  filter(!is.na(issn)) %>% 
  select(-issn_col) %>% 
  mutate(issn = str_c(str_sub(issn, start = 1, end = 4), # format: 0000-0000
                      "-", 
                      str_sub(issn, start = 5, end = -1)))

# checking if duplicates are also in pubmed data (18 records)
sjr_pubmed_dupes <- sjr_dupes %>% 
  semi_join(pubmed_data_extract %>% 
              select(issn)) %>% 
  group_by(year, issn) %>% 
  mutate(cites_doc_2years = str_replace(cites_doc_2years, ",", "\\.") %>% as.numeric) %>% 
  filter(cites_doc_2years == max(cites_doc_2years)) %>%  # correct by only keeping maximum value per issn and year
  ungroup() %>% 
  distinct()

# preparing all journals for join
sjr_journals_join <- sjr_journals %>% 
  separate(issn, into = c("issn_1", "issn_2"), sep = ", ") %>% 
  gather(issn_col, issn, -year, -sjr_best_quartile, -cites_doc_2years) %>% 
  filter(!is.na(issn)) %>% 
  select(-issn_col) %>% 
  mutate(issn = str_c(str_sub(issn, start = 1, end = 4), 
                      "-", 
                      str_sub(issn, start = 5, end = -1)),
         cites_doc_2years = str_replace(cites_doc_2years, ",", "\\.") %>% as.numeric)

sjr_journals_join <- sjr_journals_join %>% 
  anti_join(sjr_pubmed_dupes %>% select(year, issn)) %>% 
  bind_rows(sjr_pubmed_dupes) # corrected the duplicates before join

# joining scimago data with pubmed on year and issn
pubmed_data_extract_sjr <- pubmed_data_extract %>% 
  left_join(sjr_journals_join)

# checking number of missing (e.g. due to different issn for journal in pubmed and on scimago)
pubmed_scimago_missing <- pubmed_data_extract_sjr %>% 
  filter(year >= 1999) %>%  # no data available on scimago before 1999
  filter(is.na(cites_doc_2years)) %>% # 53,262 missing
  select(journal, year) %>% # selecting journals and year to look up in detail
  distinct()

sjr_journals_name <- sjrdata %>% 
  select(year, sjr_journal, cites_doc_2years, sjr_best_quartile) %>% 
  distinct()

# checking for duplicates and correcting for best values
sjr_dupes_name <- sjr_journals_name %>% 
  semi_join(sjr_journals_name %>% 
              select(year, sjr_journal) %>% 
              get_dupes()) %>% 
  group_by(year, sjr_journal) %>% 
  filter(cites_doc_2years == max(cites_doc_2years)) %>%  # correct by only keeping maximum value per sjr_journal and year
  ungroup() %>% 
  distinct()

sjr_journals_name <- sjr_journals_name %>% 
  anti_join(sjr_dupes_name %>% 
              select(year, sjr_journal)) %>% 
  bind_rows(sjr_dupes_name)


# trying to match pubmed journal name with scimago journal name 
library(fuzzyjoin)

sjr_journals_name_only <- sjr_journals_name %>% 
  select(sjr_journal) %>% 
  distinct()

matched_journals <- pubmed_scimago_missing %>%
  select(journal) %>%
  stringdist_left_join(sjr_journals_name_only, by = c("journal" = "sjr_journal"),
                       distance_col = "distance",
                       max_dist = 5)

# correct match
matched_journals_zero_distance <- matched_journals %>% 
  filter(distance == 0) 

# correct match by name
matched_journals_correct <- matched_journals %>% 
  anti_join(matched_journals_zero_distance %>% select(sjr_journal)) %>% 
  arrange(journal, distance, sjr_journal) %>% 
  group_by(journal) %>% 
  mutate(old = tolower(journal),
         new = tolower(sjr_journal),
         correct = if_else(old == new, TRUE, FALSE)) %>% 
  select(-old, -new) %>% 
  ungroup() %>% 
  filter(correct == TRUE) %>% 
  select(-distance, -correct)

# export to check by hand
matched_journals_by_hand <- matched_journals %>% 
  anti_join(matched_journals_zero_distance %>% select(journal)) %>% 
  anti_join(matched_journals_correct %>% select(journal)) %>% 
  group_by(journal) %>% 
  filter(distance == min(distance)) %>% 
  arrange(journal, sjr_journal) %>% 
  select(-distance)

# write_excel_csv(matched_journals_by_hand, "matched_journals_by_hand.csv", delim = ";")

matched_journals_by_hand <- read_delim("matched_journals_by_hand.csv", 
                                      ";", escape_double = FALSE, trim_ws = TRUE)

# putting it all together
matched_journals_complete <- matched_journals_zero_distance %>% 
  select(-distance) %>% 
  bind_rows(matched_journals_correct) %>% 
  bind_rows(matched_journals_by_hand)

# are we stilling missing some?
still_missing <- pubmed_scimago_missing %>% 
  select(-year) %>% 
  distinct() %>% 
  anti_join(matched_journals_complete %>% select(journal))
  
# yes we are still missing some: trying to catch them
still_missing <- still_missing %>% 
  mutate(journal_mod = tolower(journal),
         journal_mod = str_remove_all(journal_mod, "the "),
         journal_mod = str_remove_all(journal_mod, " \\(.*\\)"),
         journal_mod = str_remove_all(journal_mod, "&amp;"))

# fuzzy join on name again
still_missing <- still_missing %>% 
  stringdist_left_join(sjr_journals_name_only %>% 
                         mutate(sjr_journal_mod = tolower(sjr_journal)), by = c("journal_mod" = "sjr_journal_mod"),  
                       distance_col = "distance",
                       max_dist = 5) 
beepr::beep(2)

# filter out the ones that worked
still_missing_correct <- still_missing %>% 
  filter(distance == 0) %>% 
  select(-journal_mod, -sjr_journal_mod, -distance)

# trying a more fuzz join on the name
still_missing_extended <- still_missing %>% 
  anti_join(still_missing_correct, by = "journal") %>% 
  mutate(journal_mod = str_remove_all(journal_mod, " \\:.*"),
         journal_mod = str_remove_all(journal_mod, " \\=.*")) %>% 
  select(-distance, -sjr_journal, -sjr_journal_mod) %>% 
  stringdist_left_join(sjr_journals_name_only %>% 
                         mutate(sjr_journal_mod = tolower(sjr_journal)), by = c("journal_mod" = "sjr_journal_mod"),  
                       distance_col = "distance",
                       max_dist = 5) 

beepr::beep(2)

# finding again some that worked
still_missing_extended_correct <- still_missing_extended %>% 
  filter(distance == 0) %>% 
  select(-journal_mod, -sjr_journal_mod, -distance)

# sorting out the ones that need to be found by hand
still_missing_by_hand <- still_missing_extended %>% 
  anti_join(still_missing_extended_correct, by = "journal") %>% 
  anti_join(matched_journal_by_hand, by = "journal") %>%
  group_by(journal) %>% 
  filter(distance == min(distance) | is.na(distance)) %>% 
  select(-journal_mod, -sjr_journal_mod, -distance) %>% 
  distinct()

# write_excel_csv(still_missing_by_hand, "still_missing_by_hand.csv", delim = ";")

still_missing_by_hand_done <- read_tsv("still_missing_by_hand_done.tsv") %>% 
  select(journal, sjr_journal)

# binding all together
pubmed_to_sjr_complete <- still_missing_correct %>% 
  bind_rows(still_missing_extended_correct) %>% 
  bind_rows(still_missing_by_hand_done) %>% 
  bind_rows(matched_journals_complete) %>% 
  distinct()

# correcting to journals by hand
pubmed_to_sjr_complete <- pubmed_to_sjr_complete %>% 
  mutate(sjr_journal = case_when(
    sjr_journal == "Current Problems in Dermatology" ~ "Current problems in dermatology",
    sjr_journal == "Revista medico-chirurgicala a Societatii de Medici si Naturalisti din Iasi" ~ "Revista Medico-Chirurgicala a Societatii de Medici si Naturalisti din Iasi",
    TRUE ~ sjr_journal
  )) %>% 
  distinct()

saveRDS(pubmed_to_sjr_complete, "pubmed_to_sjr_complete.RDS")
