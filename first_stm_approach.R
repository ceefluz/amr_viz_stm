
library(tidyverse)
library(janitor)
library(tidytext)
library(textclean)
library(tm)
library(stm)
library(ggthemes)
library(scales)

set.seed(123)

# loading data
data <- readRDS("pubmed_data_2019-08-20.RDS") %>% 
  clean_names()

colnames(data)

# last ten years only
data <- data %>% 
  filter(year_pub >= 2009)

# merging all relevant text to new variable
data$pmc_citation_count <- as.integer(data$pmc_citation_count)

data <- data %>% 
  mutate(text = paste(title, abstract),
         cit_per_year = pmc_citation_count / (2019 - year_pub + 1)) %>%  # per year but not 0
  rowwise() %>% 
  mutate(cit_per_year = if_else(max(cit_per_year) >= 20, 20, cit_per_year)) %>% # citations per year capped at 20 max (see Bohr % Dunlap)
  select(journal, authors, text, year_pub, cit_per_year) %>% 
  ungroup()

# checking tidiness of text data and cleaning with textclean package
data$text %>% 
  str_extract_all(boundary("character")) %>% 
  unlist() %>% 
  unique() # will print all characters

data <- data %>% 
  mutate(text = strip(text), # strip anything but letters, numbers, spaces, and apostrophes
         text = replace_non_ascii(text), #remove non-ascii characters
         rowID = row_number()) 


# tokenize text, stopwords, and stemming
# removing resistance related words as they were already part of search strategy
# stemming not required (reference: http://www.cs.cornell.edu/~xanda/winlp2017.pdf) but used in this case as first models showed many similar words among top words (e.g. infection and infections)
resistance_stopwords <- tibble(word = c("resistance", "resistances", "resistant",
                                        "antimicrobial", "antimicrobials",
                                        "antibacterial", "antbacterials",
                                        "antibiotic", "antibiotics",
                                        "susceptible", "susceptibility",
                                        "antifungal", "antifungals"))

tidy_data <- data %>% 
  unnest_tokens(word, text) %>%
  anti_join(get_stopwords(language = "en")) %>% 
  anti_join(get_stopwords(language = "es")) %>% # removing stopwords from additional language as they showed up as seperate topic in test modeling
  anti_join(get_stopwords(language = "fr")) %>% 
  anti_join(get_stopwords(language = "de")) %>% 
  anti_join(resistance_stopwords) %>% 
  mutate(word = stemDocument(word))
  


tidy_data_sparse <- tidy_data %>%
  count(rowID, word) %>%
  cast_sparse(rowID, word, n)

# trying out several model for K 5 to 50
library(furrr) # for parallel processing
plan(multiprocess)

## see many_models object in Google Drive
# many_models <- tibble(K = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50)) %>%
#   mutate(topic_model = future_map(K, ~stm(tidy_data_sparse, K = .,
#                                           verbose = FALSE)))
# 
# saveRDS(many_models, 'many_models.RDS')

# evaluation the models

## see k_result object in Google Drive
# heldout <- make.heldout(tidy_data_sparse)
# 
# k_result <- many_models %>%
#   mutate(exclusivity = map(topic_model, exclusivity),
#          semantic_coherence = map(topic_model, semanticCoherence, tidy_data_sparse),
#          eval_heldout = map(topic_model, eval.heldout, heldout$missing),
#          residual = map(topic_model, checkResiduals, tidy_data_sparse),
#          bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
#          lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
#          lbound = bound + lfact,
#          iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))
# 
# saveRDS(k_result, 'k_result.RDS')
# 
# beepr::beep(2)

k_result %>%
  transmute(K,
            `Lower bound` = lbound,
            Residuals = map_dbl(residual, "dispersion"),
            `Semantic coherence` = map_dbl(semantic_coherence, mean),
            `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
  gather(Metric, Value, -K) %>%
  ggplot(aes(K, Value, color = Metric)) +
  geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(x = "K (number of topics)",
       y = NULL,
       title = "Model diagnostics by number of topics")

k_result %>%
  select(K, exclusivity, semantic_coherence) %>%
  filter(K %in% c(40, 45, 50)) %>%
  unnest() %>%
  mutate(K = as.factor(K)) %>%
  ggplot(aes(semantic_coherence, exclusivity, color = K)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence",
       subtitle = "Models with fewer topics have higher semantic coherence for more topics, but lower exclusivity")


# based on plots above the "best" number of topic would be around 20 to 30 (chosing 20 first)

topic_model <- k_result %>% 
  filter(K == 45) %>% 
  pull(topic_model) %>% 
  .[[1]]

topic_model #A topic model with 25 topics, 36598 documents and a 131872 word dictionary.

td_beta <- tidy(topic_model)

td_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic",
       subtitle = "Different words are associated with different topics")

td_gamma <- tidy(topic_model, matrix = "gamma",
                 document_names = rownames(tidy_data_sparse))

top_terms <- td_beta %>%
  arrange(beta) %>%
  group_by(topic) %>%
  top_n(7, beta) %>%
  arrange(-beta) %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest()

gamma_terms <- td_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

gamma_terms %>%
  top_n(20, gamma) %>%
  ggplot(aes(topic, gamma, label = terms, fill = topic)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 0, nudge_y = 0.0005, size = 3) +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.30),
                     labels = percent_format()) +
  theme_tufte(base_family = "Arial") +
  theme(plot.title = element_text(size = 16, family = "Arial"),
        plot.subtitle = element_text(size = 13)) +
  labs(x = NULL, y = expression(gamma),
       title = "Top 20 topics by prevalence in the PubMed corpus",
       subtitle = "With the top words that contribute to each topic")

gamma_terms %>%
  select(topic, gamma, terms)
