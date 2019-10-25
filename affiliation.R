
library(tidyverse)
library(RISmed)
library(textclean)
library(tidytext)
library(multidplyr)
library(janitor)
library(mapsapi)

# affiliations ------------------------------------------------------------

# loading data
pubmed_data <- readRDS("pubmed_batch_2019-09-25.RDS")
pubmed_data_extract <- readRDS("pubmed_data_extract_2019-10-18.RDS")

# extracting affiliation
aff_raw <- pubmed_data %>% 
  mutate(pmid = map(results_data, PMID),
         author = map(results_data, Author),
         affiliation = map(results_data, Affiliation)) %>% 
  select(-results_summary, -results_data) %>% 
  unnest()

# filter to include selected articles
aff_raw <- aff_raw %>% 
  semi_join(pubmed_data_extract, by = "pmid")

# setting up parallel cleaning
cluster <- new_cluster(16)
group <- rep(1:16, length.out = nrow(aff_raw))
aff <- bind_cols(tibble(group), aff_raw)
aff <- aff %>% group_by(group) %>% partition(cluster)
cluster_library(cluster, packages = c("purrr", "stringr", "textclean"))

# cleaning affilations
aff <- aff %>% 
  mutate(affiliation = str_squish(affiliation), 
         affiliation = strip(affiliation, char.keep = ",")) %>% # strip anything but letters, numbers, spaces, and apostrophes 
  collect()

# transform to long format
aff_long <- aff %>% 
  ungroup() %>% 
  select(year, pmid, affiliation) %>% 
  separate_rows(affiliation, sep = ",") %>%
  mutate(affiliation = str_squish(affiliation))

# transform to one row per word
aff_token <- aff %>% 
  ungroup() %>% 
  select(year, pmid, affiliation) %>% 
  unnest_tokens(word, affiliation) %>% 
  rename(affiliation = word) %>% 
  filter(affiliation != "of") # city in turkey

# get country names
countries <- maps::world.cities %>% 
  select(country = country.etc, city = name) %>% 
  mutate_all(tolower) %>% 
  distinct()

# adding country names in more languages
countries_join <- countries %>% 
  select(country) %>% 
  bind_rows(list(tibble(country = countrycode::codelist$un.name.en),
                 tibble(country = countrycode::codelist$un.name.es),
                 tibble(country = countrycode::codelist$un.name.fr),
                 tibble(country = countrycode::codelist$un.name.ru),
                 tibble(country = countrycode::codelist$country.name.de),
                 tibble(country = countrycode::codelist$country.name.en))) %>% 
  mutate_all(tolower) %>% 
  filter(!is.na(country)) %>% 
  distinct()

# detect countries per row
country_detected <- aff_long %>%
  semi_join(countries_join, by = c("affiliation" = "country")) %>%
  rowid_to_column() %>%
  group_by(pmid) %>%
  filter(rowid == min(rowid)) %>% # keeping only first country (first author)
  ungroup() %>%
  select(-rowid)

# detect countries per word
country_detected_token <- aff_token %>%
  anti_join(country_detected, by = "pmid") %>%
  semi_join(countries_join, by = c("affiliation" = "country")) %>%
  rowid_to_column() %>%
  group_by(pmid) %>%
  filter(rowid == min(rowid)) %>% # keeping only first country (first author)
  ungroup() %>%
  select(-rowid)

# join detected countries
country_detected_all <- country_detected %>%
  bind_rows(country_detected_token)

saveRDS(country_detected_all, "country_detected_all.RDS")

# filter for missing countries
country_missing <- aff %>% 
  ungroup() %>% 
  anti_join(country_detected_all %>% select(pmid))

# make missing NA
country_missing_no_aff <- country_missing %>% 
  filter(affiliation == "character") %>% 
  mutate(affiliation = NA_character_)

saveRDS(country_missing_no_aff, "country_missing_no_aff.RDS")

# filter for missing non-NA
country_missing <- country_missing %>% 
  anti_join(country_missing_no_aff %>% select(pmid))

# shortening too long affilations
country_missing <- country_missing %>% 
  mutate(affiliation = str_trunc(affiliation, 1000)) # to prevent Google API from failing

saveRDS(country_missing, "country_missing.RDS")

# get affiliation data from Google Maps through peregrine cluster
# get location with Google API
# google_api <- "AIzaSyDS-H9ZKplI2injQ4SeepCcqwr1awedB64"

# import result from peregrine
country_missing_google <- read_rds("peregrine/country_missing_google.RDS")

# get completed affilations
country_missing_google_complete <- country_missing_google %>% 
  filter(!is.na(google_address))

# filter NAs
country_missing_google_na <- country_missing_google %>% 
  filter(is.na(google_address))

# detect countries in new google address
country_missing_google_complete <- country_missing_google_complete %>% 
  select(year, pmid, google_address) %>% 
  separate_rows(google_address, sep = ",") %>%
  mutate(google_address = str_squish(google_address),
         google_address = tolower(google_address)) %>% 
  semi_join(countries_join, by = c("google_address" = "country")) %>%
  rowid_to_column() %>%
  group_by(pmid) %>%
  filter(rowid == min(rowid)) %>% # keeping only first country (first author)
  ungroup() %>%
  select(-rowid)

# get missing countries
country_missing_google_rest <- country_missing_google %>% 
  anti_join(country_missing_google_complete, by = "pmid") %>% 
  anti_join(country_missing_google_na, by = "pmid") 

countries_eng <- countrycode::codelist %>% 
  select(country = country.name.en) %>% 
  mutate(country = tolower(country))

cities <- countries %>% 
  select(city, country) %>% 
  filter(str_length(city) > 3)

# extract country names from affiliations
country_missing_google_rest <- country_missing_google_rest %>% 
  mutate(google_address = tolower(google_address),
         found = str_extract(google_address, paste(countries_eng$country, collapse = "|")),
         found_city = if_else(is.na(found), str_extract(google_address, "hong kong|gaza"), NA_character_))

missing_cities <- tibble(
  city = c("hong kong", "gaza", country_missing_google_rest %>% filter(is.na(found) & is.na(found_city)) %>% pull(google_address) %>% unique()),
  country = c("china", "palestine", "israel", "india", "kosovo", "saint kitts and nevis", "india", "palestine", "palestine", "ukraine", "pakistan", "south korea")
)

country_missing_google_rest <- country_missing_google_rest %>% 
  left_join(missing_cities, by = c("found_city" = "city")) %>% 
  left_join(missing_cities, by = c("google_address" = "city")) %>% 
  mutate(google_address = case_when(
    !is.na(found) ~ found, 
    is.na(found) & !is.na(found_city) ~ country.x,
    is.na(found) & is.na(found_city) ~ country.y,
    TRUE ~ NA_character_
  )) %>% 
  select(-found, -found_city, -country.x, -country.y)

cities <- maps::world.cities %>% 
  filter(pop > 100000) %>% 
  mutate_all(tolower) %>% 
  filter(str_length(name) > 5)

country_missing_google_na <- country_missing_google_na %>% 
         mutate(found = str_extract(affiliation, paste(countries_eng$country, collapse = "|")),
         found_city = if_else(str_count(affiliation, '\\w+') > 1, # excluding one word affiliations (i.e. email addresses)
                              str_extract(affiliation, paste(cities$name, collapse = "|")),
                              NA_character_))

# found missing countries
country_missing_google_na_found <- country_missing_google_na %>% 
  filter(!is.na(found)) %>% 
  mutate(google_address = found) %>% 
  select(year, pmid, google_address)

# export rest to clean by hand
country_missing_google_na %>% 
  anti_join(country_missing_google_na_found %>% select(pmid)) %>% 
  select(year, pmid, affiliation, google_address, found_city) %>% 
  write_excel_csv2("country_by_hand.csv")

country_by_hand <- read_csv2("country_by_hand.csv") %>% 
  select(-found_city) %>% 
  mutate(pmid = as.character(pmid))

# merging it all
country_all <- country_detected_all %>% 
  bind_rows(list(country_missing_no_aff %>% select(-author, -group), 
                 country_missing_google_complete %>% rename(affiliation = google_address),
                 country_missing_google_na_found %>% rename(affiliation = google_address), 
                 country_by_hand %>% select(-affiliation) %>% rename(affiliation = google_address), 
                 country_missing_google_rest %>% select(-affiliation, -author, -group) %>% rename(affiliation = google_address)))

saveRDS(country_all, "country_all_2019-10-25.RDS")

pubmed_data_extract <- pubmed_data_extract %>% 
  left_join(country_all)

saveRDS(pubmed_data_extract, "pubmed_data_extract_2019-10-25.RDS")



# mapping
library(tidyverse)
library(readxl)
library(ggalt)

world_data <- map_data('world') %>%
  filter(region != "Antarctica")

country_count <- country_all %>% 
  count(affiliation) %>% 
  mutate(n_group = case_when(
    n <= 5 ~ "<= 5 (1. quartile)",
    n > 5 & n <= 29 ~ "5 - 29 (median)",
    n > 29 & n <= 250 ~ "29-250 (3. quartile)",
    n > 250 ~ "> 250",
    TRUE ~ NA_character_
  )) %>% 
  mutate(n_group = factor(n_group, levels = c("<= 5 (1. quartile)", "5 - 29 (median)", "29-250 (3. quartile)", "> 250"))) %>% 
  rename(country = affiliation)

world_data <- world_data %>%
  mutate(country = tolower(region)) %>% 
  left_join(country_count)

ggplot(world_data, aes(long, lat, group = group)) +
  geom_polygon(fill = "lightgrey", alpha = 0.8) +
  geom_polygon(data = world_data, aes(long, lat, group = group, fill = n_group),
               colour = "black", size = 0.1) +
  scale_fill_manual(values = c('#ffdead', '#e5a37e', '#c66a52', '#a52a2a')) +
  coord_proj("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") +
  labs(fill = "Number of studies") +
  theme_void()

ggplot(world_data, aes(long, lat, group = group)) +
  geom_polygon(fill = "lightgrey", alpha = 0.8) +
  geom_polygon(data = world_data, aes(long, lat, group = group, fill = group),
               colour = "black", size = 0.1) +
  scale_fill_manual(values = c('#ffdead', '#e5a37e', '#c66a52', '#a52a2a')) +
  coord_proj("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") +
  labs(fill = "Number of studies") +
  theme_void()

ggplot(world_data, aes(long, lat, group = group)) +
  geom_polygon(fill = "lightgrey", alpha = 0.8) +
  geom_polygon(data = world_data, aes(long, lat, group = group, fill = n),
               colour = "black", size = 0.1) +
  scale_fill_gradient(low = '#ffdead', high = '#a52a2a', trans = "log", breaks = c(10, 100, 1000, 10000)) +
  coord_proj("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") +
  labs(fill = "Number of studies\n(logarithmic scale)") +
  theme_void()
