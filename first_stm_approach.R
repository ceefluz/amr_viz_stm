
library(tidyverse)
library(janitor)
library(tidytext)
library(textclean)
library(qdapRegex)
library(tm)
library(stm)
library(ggthemes)
library(ggforce)
library(scales)

set.seed(123)

# loading data ------------------------------------------------------------
data <- readRDS("ris_data_peregrine.RDS") %>% 
  clean_names()

# merging all relevant text to new variable -------------------------------
data <- data %>% 
  mutate(text = paste(title, abstract),
         cit_per_year = as.integer(cit_count) / (2019 - year + 1)) %>%  # per year but not 0
  rowwise() %>% 
  mutate(cit_per_year = if_else(max(cit_per_year) >= 20, 20, cit_per_year)) %>% # citations per year capped at 20 max (see Bohr & Dunlap)
  ungroup()

test <- data %>% 
  mutate(text = strip(text), # strip anything but letters, numbers, spaces, and apostrophes
         text = replace_non_ascii(text), #remove non-ascii characters
         text = rm_non_words(text),
         text = rm_nchar_words(text, 1),
         rowID = row_number()) 

# tokenize text, stopwords, and stemming ----------------------------------
tidy_data <- test %>% 
  unnest_tokens(word, text) %>%
  anti_join(get_stopwords(language = "en")) %>% 
  anti_join(get_stopwords(language = "es")) %>% # removing stopwords from additional language as they showed up as seperate topic in test modeling
  anti_join(get_stopwords(language = "fr")) %>% 
  anti_join(get_stopwords(language = "de")) %>% 
  mutate(word = stemDocument(word))

add_stopwords <- read_tsv("stopwords.txt", col_names = "word")

tidy_data1 <- tidy_data %>% 
  anti_join(add_stopwords)

tidy_data_sparse <- tidy_data %>%
  count(rowID, word) %>%
  cast_sparse(rowID, word, n)

saveRDS(tidy_data_sparse, "tidy_data_sparse.RDS")

# modeling for several K --------------------------------------------------
library(furrr) # for parallel processing
plan(multiprocess)

## see many_models object in Google Drive
many_models <- tibble(K = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55)) %>%
  mutate(topic_model = future_map(K, ~stm(tidy_data_sparse, K = .,
                                          verbose = FALSE)))
beepr::beep(2)

# many_models_prev <- tibble(K = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55)) %>%
#   mutate(topic_model = future_map(K, ~stm(tidy_data_sparse, K = ., prevalence = ~year_pub,
#                                           verbose = FALSE)))

saveRDS(many_models, 'many_models.RDS')
# saveRDS(many_models_prev, 'many_models_prev.RDS')


# evaluation the models for all K -----------------------------------------

## see k_result object in Google Drive
heldout <- make.heldout(tidy_data_sparse)

k_result <- many_models %>%
  mutate(exclusivity = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, tidy_data_sparse),
         eval_heldout = map(topic_model, eval.heldout, heldout$missing),
         residual = map(topic_model, checkResiduals, tidy_data_sparse),
         bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound = bound + lfact,
         iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))

# k_result_prev <- many_models_prev %>%
#   mutate(exclusivity = map(topic_model, exclusivity),
#          semantic_coherence = map(topic_model, semanticCoherence, tidy_data_sparse),
#          eval_heldout = map(topic_model, eval.heldout, heldout$missing),
#          residual = map(topic_model, checkResiduals, tidy_data_sparse),
#          bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
#          lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
#          lbound = bound + lfact,
#          iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))

saveRDS(k_result, 'k_result.RDS')
# saveRDS(k_result_prev, 'k_result_prev.RDS')

beepr::beep(8)


# model diagnostics by number of topics -----------------------------------
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
  filter(K %in% c(25, 40, 45, 50)) %>%
  unnest() %>%
  mutate(K = as.factor(K)) %>%
  ggplot(aes(semantic_coherence, exclusivity, color = K)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_mark_ellipse() +
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence",
       subtitle = "Models with fewer topics have higher semantic coherence for more topics, but lower exclusivity")



# investigating different topic models by filtering for K ------------------------------------

topic_model <- k_result %>% 
  filter(K == 125) %>% # selecting model by K
  pull(topic_model) %>% 
  .[[1]]

topic_model #A topic model with 25 topics, 36598 documents and a 131872 word dictionary.

td_beta <- tidy(topic_model)

# word probability for each topic
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
  top_n(10, beta) %>%
  arrange(-beta) %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest()

top_terms %>% 
  rowwise() %>% 
  mutate(topic_titles_1 = data$text[which(data$pmid %in% unlist(findThoughts(topic_model, texts=data$pmid, n=3, topics=topic)$doc))][1],
         topic_titles_2 = data$text[which(data$pmid %in% unlist(findThoughts(topic_model, texts=data$pmid, n=3, topics=topic)$doc))][2],
         topic_titles_3 = data$text[which(data$pmid %in% unlist(findThoughts(topic_model, texts=data$pmid, n=3, topics=topic)$doc))][3],
         pmid = paste0(data$pmid[which(data$pmid %in% unlist(findThoughts(topic_model, texts=data$pmid, n=3, topics=topic)$doc))], collapse = ", ")) %>% 
  write_excel_csv2("top_terms_75_wo_stop-11-11.csv")


gamma_terms <- td_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

# top 20 topics by prevalence in the PubMed corpus
gamma_terms %>%
  top_n(20, gamma) %>% # select top n
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

# full list of topics
gamma_terms %>%
  select(topic, gamma, terms)
