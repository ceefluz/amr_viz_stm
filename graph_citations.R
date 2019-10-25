
library(tidyverse)
library(tidygraph)
library(ggraph)

# network test ------------------------------------------------------------

citations_raw <- readRDS("citations_2019-09-26.RDS")
pubmed_data <- readRDS("pubmed_data_extract_2019-09-26.RDS")

citations <- citations_raw

citations <- citations %>% 
  rename(to = article_pmid, from = citing_pmid) %>% 
  left_join(pubmed_data %>% select(pmid, year), by = c("to" = "pmid"))

years <- citations %>% 
  gather(type, pmid, -year) %>% 
  distinct() %>% 
  select(-type)

citations$year[1:500] <- 1995

graph_routes <- as_tbl_graph(citations)

thm <- theme_minimal() +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major = element_blank(),
  ) 

theme_set(thm)

graph_routes %>%
  ggraph(layout = 'kk') +
  geom_node_point( alpha = 0.3) +
  geom_edge_diagonal(alpha = 0.3, aes(colour = year)) 
