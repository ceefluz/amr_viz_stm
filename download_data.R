
# download data (2019-09-25) -----------------------------------------------------------

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
pubmed_data_extract <- pubmed_data %>% 
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

n_all <- nrow(pubmed_data_extract)

# filtering for last 20 years (>= 1999)
pubmed_data_extract <- pubmed_data_extract %>% 
  filter(year >= 1999)

n_years <- nrow(pubmed_data_extract)

# change missing to NA and filter to include only non-missing
missing <- pubmed_data_extract %>% 
  mutate(abstract = if_else(str_length(abstract) == 0, NA_character_, abstract),
         title = if_else(str_length(title) == 0, NA_character_, title)) %>% 
  filter(is.na(abstract) | is.na(title))

pubmed_data_extract <- pubmed_data_extract %>% 
  anti_join(missing, by = "pmid")

# identifying missing data by explicit characters
no_title <- pubmed_data_extract %>% 
  filter(str_detect(title, "Not Available"))

no_abstract <- pubmed_data_extract %>% 
  filter(str_detect(abstract, "n/a."))

pubmed_data_extract <- pubmed_data_extract %>% 
  anti_join(no_title, by = "pmid") %>% 
  anti_join(no_abstract, by = "pmid")

n_missing <- nrow(pubmed_data_extract)

# filter not relevant literature by publication type
# extract and clean publication types
pub_type <- pubmed_data_extract %>% 
  select(pmid, publication_type) %>% 
  mutate(publication_type = str_remove_all(publication_type, "PublicationType \\="),
         publication_type = str_remove_all(publication_type, "c\\("),
         publication_type = str_remove_all(publication_type, "\\)"),
         publication_type = str_remove_all(publication_type, '"'))

# get available publication type levels
pub_type_list <- tibble(type = unlist(pubmed_data_extract$publication_type)) %>% 
  distinct()

unique(pub_type_list$type) %>% sort()

exclude_type <- tibble(type = c("Address", "Autobiography", "Bibliography", "Biography", "Comment", 
                                "Corrected and Republished Article", # to be discussed
                                "Directory", "Editorial", "Festschrift", "Interactive Tutorial", "Interview", "Lecture",
                                "Legal Case", "Letter", "News", "Newspaper Article", "Patient Education Handout", 
                                "Personal Narrative", 
                                "Published Erratum", # to be discussed
                                "Review", # to be discussed
                                "Systematic Review", # to be discussed
                                "Video-Audio Media", "Webcasts"))

# filtering out
to_exclude <- pub_type %>% 
  separate_rows(publication_type, sep = ",") %>% 
  mutate(publication_type = str_squish(publication_type)) %>% 
  filter(str_detect(publication_type, paste(exclude_type$type, collapse = "|")))

# anti join to exclude
pubmed_data_extract <- pubmed_data_extract %>% 
  anti_join(to_exclude, by = "pmid")

n_types <- nrow(pubmed_data_extract)

# check for duplicates
get_dupes(pubmed_data_extract %>% 
            select(pmid, title, abstract, journal, year)) # no duplicates
pubmed_data_dupes <- get_dupes(pubmed_data_extract %>% 
            select(title, journal, year)) # 115 (several PMIDS for same article) - to be removed! 

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

saveRDS(citations, "citations_2019-10-18.RDS")

# same needs to be done within pubmed_data_extract - keeping first PMID only
pubmed_data_extract <- pubmed_data_extract %>% 
  group_by(title, journal, year) %>% 
  filter(pmid == min(pmid)) %>% 
  ungroup()

n_dupes <- nrow(pubmed_data_extract)

# joining SJR data
pubmed_to_sjr_complete <- readRDS("pubmed_to_sjr_complete.RDS")
pubmed_data_extract <- pubmed_data_extract %>% 
  left_join(pubmed_to_sjr_complete) 

sjrdata <- readRDS("sjrdata.RDS")
pubmed_data_extract <- pubmed_data_extract %>% 
  left_join(sjrdata %>% 
              select(year, sjr_journal, rank, h_index, cites_doc_2years, sjr_best_quartile))

join_dupes <- pubmed_data_extract %>% 
  group_by(pmid) %>% 
  add_count(sjr_journal) %>% 
  filter(n > 1) %>% 
  filter(h_index == max(h_index) | is.na(h_index)) # keeping only best h_index per year if more than one recorded


pubmed_data_extract <- pubmed_data_extract %>% 
  anti_join(join_dupes, by = "pmid") %>% 
  bind_rows(join_dupes)


# merging title and abstract together
pubmed_data_extract_model <- pubmed_data_extract %>% 
  ungroup() %>% 
  mutate(text = paste(title, abstract, sep = " ")) %>% 
  select(year, pmid, text, journal, sjr_journal, h_index, cites_doc_2years, rank)
  
saveRDS(pubmed_data_extract_model, "pubmed_data_extract_2019-10-18.RDS")

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

pbPost(type = "note", title = "JOB DONE")

library(metagear)

phases <- c(str_glue("START_PHASE: All records since 1999: {n_years}"), 
            str_glue("After exclusion of missing title or abstract: {n_missing}"),
            str_glue("EXCLUDE_PHASE: Missing title/abstract:{n_years - n_missing} records removed"),
            str_glue("After removing reviews etc.: {n_types}"),
            str_glue("EXCLUDE_PHASE: Article type exclusions: {n_missing - n_types} records removed"),
            str_glue("Final set of records for topic modelling: {n_dupes}"),
            str_glue("EXCLUDE_PHASE: Duplicate removals: {n_types - n_dupes} records removed"))

# PRISMA plot with custom layout
thePlot <- plot_PRISMA(phases, design = "grey")




