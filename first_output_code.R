
# # A tibble: 40 x 3
# topic     gamma terms                                                   
# <fct>     <dbl> <chr>                                                   
#   1 Topic 39 0.0507 develop, diseas, health, emerg, infect, pathogen, new   
# 2 Topic 6  0.0426 isol, type, st, strain, sequenc, clone, genotyp         
# 3 Topic 7  0.0425 isol, hospit, studi, cultur, infect, patient, bacteri   
# 4 Topic 10 0.0379 patient, infect, risk, factor, hospit, associ, care     
# 5 Topic 26 0.0357 treatment, infect, therapi, drug, combin, clinic, effect
# 6 Topic 27 0.0352 patient, case, infect, report, caus, present, transplant
# 7 Topic 8  0.0350 activ, peptid, structur, inhibitor, target, bind, report
# 8 Topic 40 0.0342 select, differ, may, increas, can, popul, chang         
# 9 Topic 15 0.0335 gene, isol, detect, pcr, strain, determin, class        
# 10 Topic 3  0.0294 coli, pneumonia, esbl, klebsiella, k, isol, produc      
# # … with 30 more rows

example_topics <- c(10, 3)

# top 10 PMID for topic 16
top_10 <- findThoughts(topic_model, texts=data$pmid, n=10, topics=10)$doc

# top 10 PMID for topic 3
top_3 <- findThoughts(topic_model, texts=data$pmid, n=10, topics=3)$doc

# top 6 PMID for topic 3
top_6 <- findThoughts(topic_model, texts=data$pmid, n=10, topics=6)$doc
