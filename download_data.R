
# download data (2019-09-25) -----------------------------------------------------------

library(jsonlite)
library(RPushbullet)
library(RISmed)
library(tidyverse)
library(tidytext)
library(janitor)

search_string <- '("Anti-Bacterial Agents" [Mesh] OR Anti-Bacterial* [tiab] OR antibacterial* [tiab] OR antibiotic* [tiab] OR antimicrobial* [tiab] OR antimycobacterial* [tiab] OR "Antifungal Agents"[Mesh] OR Antifungal* [tiab] or anti-fungal* [tiab]) AND ("Drug Resistance"[Mesh] OR resistan* [tiab] OR "Microbial Sensitivity Tests"[Mesh])'

years <- tibble(year = seq(2019-25, 2019, 1))

pubmed_data <- years %>% 
  mutate(results_summary = pmap(list(query = search_string, 
                                     retmax = 15000,
                                     mindate = year,
                                     maxdate = year,
                                     url = paste0('https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=("Anti-Bacterial+Agents"+[Mesh]+OR+Anti-Bacterial*+[tiab]+OR+antibacterial*+[tiab]+OR+antibiotic*+[tiab]+OR+antimicrobial*+[tiab]+OR+antimycobacterial*+[tiab]+OR+"Antifungal+Agents"[Mesh]+OR+Antifungal*+[tiab]+or+anti-fungal*+[tiab])+AND("Drug+Resistance"[Mesh]+OR+resistan*+[tiab]+OR+"Microbial+Sensitivity+Tests"[Mesh])&mindate=', year, '&maxdate=', year, '&retmax=30000&tool=RISmed&email=s.a.kovalchik@gmail.com')),
                                EUtilsSummary),
         results_data = map(results_summary, EUtilsGet))

saveRDS(pubmed_data, "pubmed_batch_2019-09-25.RDS")

pubmed_data <- readRDS("pubmed_batch_2019-09-25.RDS")

#extract information
pubmed_data_extract_type <- pubmed_data %>% 
  mutate(pmid = map(results_data, PMID),
         author = map(results_data, Author),
         title = map(results_data, ArticleTitle),
         abstract = map(results_data, AbstractText),
         publication_type = map(results_data, PublicationType),
         journal = map(results_data, Title),
         # affiliation = map(results_data, Affiliation),
         # journal_country = map(results_data, Country),
         # iso_abbr = map(results_data, ISOAbbreviation),
         issn = map(results_data, ISSN),
         language = map(results_data, Language),
         mesh = map(results_data, Mesh)) %>%
  select(-results_summary, -results_data) %>% 
  unnest()

saveRDS(pubmed_data_extract, "pubmed_data-extract_2019-10-11.RDS")

# filter for PMIDS with abstracts
pubmed_data_extract <- pubmed_data_extract %>% 
  mutate(abstract = if_else(str_length(abstract) == 0, NA_character_, abstract)) %>% 
  filter(!is.na(abstract))

# check for duplicates
get_dupes(pubmed_data_extract %>% 
            select(pmid, title, abstract, journal, year)) # no duplicates
pubmed_data_dupes <- get_dupes(pubmed_data_extract %>% 
            select(title, journal, year)) # 787 (several PMIDS for same article) - to be removed! 

# the problem with these PMIDs is that some have citations for several PMIDS referring to the same article
# therefore, the citations of these PMIDS will be merged into one PMID

citations <- readRDS("citations_2019-09-26.RDS")

citations_dupes <- citations %>% 
  semi_join(pubmed_data_dupes %>% 
              left_join(pubmed_data_extract), by = c("article_pmid" = "pmid")) %>% 
  left_join(pubmed_data_dupes %>% 
              left_join(pubmed_data_extract) %>% 
              select(pmid, title, journal), by = c("article_pmid" = "pmid")) %>% 
  distinct()

citations_dupes_corrected <- citations_dupes %>% 
  group_by(title, journal) %>% 
  mutate(article_pmid = min(article_pmid)) %>% # changing duplicate PMIDs to first PMID
  ungroup() %>% 
  select(article_pmid, citing_pmid) %>% 
  distinct() # removing duplicates

citations <- citations %>% 
  anti_join(citations_dupes) %>% # removing duplicates
  bind_rows(citations_dupes_corrected) # adding corrected citations

# same needs to be done within pubmed_data_extract - keeping first PMID only

pubmed_data_extract <- pubmed_data_extract %>% 
  group_by(title, journal, year) %>% 
  filter(pmid == min(pmid))

# joining SJR data
pubmed_data_extract <- pubmed_data_extract %>% 
  left_join(pubmed_to_sjr_complete) 

pubmed_data_extract <- pubmed_data_extract %>% 
  left_join(sjrdata %>% 
              select(year, sjr_journal, rank, h_index, cites_doc_2years, sjr_best_quartile))

pubmed_data_extract <- pubmed_data_extract %>% 
  filter(h_index == max(h_index) | is.na(h_index)) # keeping only best h_index per year if more than one recorded

# merging title and abstract for clustering

no_title <- pubmed_data_extract %>% 
  filter(str_detect(title, "Not Available"))

no_abstract <- pubmed_data_extract %>% 
  filter(str_detect(abstract, "n/a."))

pubmed_data_extract %>% 
  mutate(length = str_length(abstract)) %>%
  arrange(length) %>% View()
  filter(str_detect(abstract, "Not Av")) %>% View()
  
type <- pubmed_data_extract_type %>% 
  select(pmid, publication_type) %>% 
  mutate(publication_type = str_remove_all(publication_type, "PublicationType \\="),
         publication_type = str_remove_all(publication_type, "c\\("),
         publication_type = str_remove_all(publication_type, "\\)"),
         publication_type = str_remove_all(publication_type, '"'))
  
  
type_list <- tibble(type = unlist(pubmed_data_extract_type$publication_type)) %>% 
  distinct()

unique(type_list$type) %>% sort()

exclude_type <- tibble(type = c("Address", "Autobiography", "Bibliography", "Biography", "Case Reports", "Comment", 
  "Corrected and Republished Article", # to be discussed
  "Directory", "Editorial", "Festschrift", "Interactive Tutorial", "Interview", "Lecture",
  "Legal Case", "Letter", "News", "Newspaper Article", "Patient Education Handout", 
  "Personal Narrative", 
  "Published Erratum", # to be discussed
  "Review", # to be discussed
  "Systematic Review", # to be discussed
  "Video-Audio Media", "Webcasts"))

type %>% 
  filter(str_detect(publication_type, exclude_type$type))
  

# affiliations <- pubmed_data_extract %>% 
#   select(pmid, affiliation) %>% 
#   mutate(affiliation = paste(affiliation))
# 
# countries <- maps::world.cities %>%
#   select(country = country.etc) %>% 
#   mutate_all(tolower) %>% 
#   distinct() %>% 
#   mutate(country_join = country)
# 
# affiliations_country_correct <- affiliations %>% 
#   unnest_tokens(input = affiliation, output = word) %>% 
#   left_join(countries, by = c("word" = "country_join")) %>% 
#   filter(!is.na(country)) %>% 
#   select(-word) %>% 
#   distinct()
# 
# affiliations_country_identified <- affiliations_country_correct %>% 
#   count(pmid) %>% 
#   filter(n == 1) %>% 
#   select(-n) %>% 
#   left_join(affiliations_country_correct %>% select(pmid, country)) 
# 
# pubmed_data_extract <- pubmed_data_extract %>% 
#   left_join(affiliations_country_identified)
# 
# pubmed_data_extract <- pubmed_data_extract %>% 
#   left_join(pubmed_journals_issn_h_index)

saveRDS(pubmed_data_extract, "pubmed_data_extract_2019-09-26.RDS")


pbPost(type = "note", title = "JOB DONE")





