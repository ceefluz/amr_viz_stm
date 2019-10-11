
library(xml2)
library(dplyr)
library(purrr)
library(tidyr)

get_refs <- function(data, pmid = pmid) {
  get_refs_internal <- function(pmid) {
    pmid_lookup <- paste0("id=", pmid, collapse = "&")
    url <- paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&linkname=pubmed_pubmed_citedin&", pmid_lookup, "tool=R&email=c.f.luz@umcg.nl")
    pubmed_extract <- read_xml(url)
    
    xml <- xml_find_all(pubmed_extract, ".//LinkSet") %>% 
      map(xml_children) %>% 
      map(xml_children) %>% 
      map(xml_children) 
    
    xml_df <- tibble(article_pmid = pmid,
                     nodeset = xml)
    
    xml_df %>%
      mutate(citing_pmid = nodeset %>% map(~ xml_text(.))) %>%
      select(article_pmid, citing_pmid) %>%
      unnest(citing_pmid)
  }
  data %>% 
    group_by(group = rep(row_number(), length.out = n(), each = 300)) %>% # 300 seems to be max per query (header overflow)
    do(data.frame(get_refs_internal(.$pmid))) %>% 
    ungroup() %>% 
    select(-group)
}

citations_1 <- pubmed_data_extract[1:20000,] %>% get_refs()
citations_2 <- pubmed_data_extract[20001:30000,] %>% get_refs()
citations_3 <- pubmed_data_extract[30001:40000,] %>% get_refs()
citations_4 <- pubmed_data_extract[40001:50000,] %>% get_refs()
citations_5 <- pubmed_data_extract[50001:60000,] %>% get_refs()
citations_6 <- pubmed_data_extract[60001:70000,] %>% get_refs()
citations_7 <- pubmed_data_extract[70001:80000,] %>% get_refs()
citations_8 <- pubmed_data_extract[80001:85000,] %>% get_refs()
citations_8.1 <- pubmed_data_extract[85001:87250,] %>% get_refs()
citations_8.2 <- pubmed_data_extract[87251:90000,] %>% get_refs()
citations_9 <- pubmed_data_extract[90001:100000,] %>% get_refs()
citations_10 <- pubmed_data_extract[100001:105000,] %>% get_refs()
citations_10.1 <- pubmed_data_extract[105001:110000,] %>% get_refs()
citations_11 <- pubmed_data_extract[110001:120000,] %>% get_refs()
citations_12 <- pubmed_data_extract[120001:130000,] %>% get_refs()
citations_13 <- pubmed_data_extract[130001:135000,] %>% get_refs()
citations_13.1 <- pubmed_data_extract[135001:137500,] %>% get_refs()
citations_13.2 <- pubmed_data_extract[137501:140000,] %>% get_refs()
citations_14 <- pubmed_data_extract[140001:142500,] %>% get_refs()
citations_14.1 <- pubmed_data_extract[142501:145000,] %>% get_refs()
citations_14.2 <- pubmed_data_extract[145001:150000,] %>% get_refs()
citations_15 <- pubmed_data_extract[150001:160000,] %>% get_refs()
citations_16 <- pubmed_data_extract[160001:170000,] %>% get_refs()
citations_17 <- pubmed_data_extract[170001:180000,] %>% get_refs()
citations_18 <- pubmed_data_extract[180001:190000,] %>% get_refs()
citations_19 <- pubmed_data_extract[190001:192500,] %>% get_refs()
citations_19.1 <- pubmed_data_extract[192501:nrow(pubmed_data_extract),] %>% get_refs()


citations <- do.call("bind_rows", list(citations_1, citations_2, citations_3, citations_4, citations_5,
                                       citations_6, citations_7, citations_8, citations_8.1, citations_8.2,
                                       citations_9, citations_10, citations_10.1, citations_11, 
                                       citations_12, citations_13, citations_13.2, citations_13.2,
                                       citations_14, citations_14.1, citations_14.2, citations_15, 
                                       citations_16, citations_17, citations_18, citations_19, citations_19.1))

saveRDS(citations, "citations_2019-09-26.RDS")

beepr::beep(2)
  

group_by(group = rep(row_number(), length.out = n(), each = 1200)) %>% 
  do(data.frame(get_refs(.))) # this should download all citing articles per pmid (if cited) for all pmids 

pbPost(type = "note", title = "JOB DONE")

citations_1 <- data[1:5000,] %>% get_refs()
citations_2 <- data[5001:10000,] %>% get_refs()
citations_3 <- data[10001:15000,] %>% get_refs()
citations_4 <- data[15001:17500,] %>% get_refs()
citations_5 <- data[17501:20000,] %>% get_refs()
citations_6 <- data[20001:22500,] %>% get_refs()
citations_7 <- data[22501:25000,] %>% get_refs()
citations_8 <- data[25001:27500,] %>% get_refs()
citations_9 <- data[27501:30000,] %>% get_refs()
citations_10 <- data[30001:32500,] %>% get_refs()
citations_11 <- data[32501:35000,] %>% get_refs()
citations_12 <- data[35001:3,] %>% get_refs()

citations <- do.call("bind_rows", list(citations_1, citations_2, citations_3, citations_4, citations_5,
                                                    citations_6, citations_7, citations_8, citations_9, citations_10,
                                                    citations_11, citations_12))
saveRDS(citations, "citations.RDS")
