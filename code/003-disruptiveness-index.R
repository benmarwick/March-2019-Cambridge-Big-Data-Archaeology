

# https://pubsonline.informs.org/doi/10.1287/mnsc.2015.2366
# https://www.nature.com/articles/s41586-019-0941-9

library(tidyverse)
library(here)

# load data --------------------------------------------------------------------
# this is the fast way to resume:
items_df <- readRDS(here("data", "derived-data", "wos-data-df.rds"))

items_df <- 
  items_df %>% 
  # remove branding
  filter(authors != "FN Clarivate Analytics Web of Science\nVR 1.0") %>% 
  # uniques only 
  group_by(authors, title, journal) %>% 
  filter(row_number() == 1) 

# get articles with DOIs and reference lists
items_wth_dois_and_refs <- 
  items_df %>% 
  filter(!is.na(refs)) %>% 
  filter(!is.na(year)) %>% 
  filter(str_detect(doi, "^10.")) %>% 
  mutate(doi = str_remove_all(doi, "\\[|\\]")) %>% 
  mutate(doi_article = doi) %>% 
  select(-doi)

# get items in the reference lists with DOIs
items_df_one_row_per_ref_with_doi <- 
  items_wth_dois_and_refs %>% 
  separate_rows(refs, sep = "\n  ") %>% 
  ungroup %>% 
  mutate(refs = tolower(str_trim(refs))) %>% 
  mutate(refs = str_extract(refs, "doi.*")) %>% 
  mutate(refs = str_remove_all(refs, "\\[|\\]")) %>% 
  mutate(doi = str_remove_all(refs, "doi"))  %>% 
  mutate(doi_cited = str_trim(doi)) %>%  
  filter(!is.na(doi_cited)) %>% 
  select(-doi)

# for a focal article:
# - find the articles that cite the focal article
# - find what articles that citing article cites 
# - find the ratio of citing articles that cite focal articles refs vs those that don't

disruption_output <- list(length = length(items_wth_dois_and_refs$doi_article))

for(i in seq_along(items_wth_dois_and_refs$doi_article)){

  this_article_doi <- items_wth_dois_and_refs$doi_article[i] # 1000
  
  # what articles are cited by the focal paper
  cited_by_focal_paper <- 
    items_df_one_row_per_ref_with_doi %>% 
    filter(doi_article %in% this_article_doi) 
  
  # what articles cite the focal paper, that are in our list
  citing_the_focal_paper <- 
    items_df_one_row_per_ref_with_doi %>% 
    filter(doi_cited %in% this_article_doi)
  
  # what articles are cited by the citing papers
  cited_by_citing_papers <- 
  items_df_one_row_per_ref_with_doi %>% 
    filter(doi_article %in% citing_the_focal_paper$doi_article)
  
  # articles that cite the focal paper's citations, but not the focal paper itself
  n_k <- 
  items_df_one_row_per_ref_with_doi %>% 
    group_nest(doi_article) %>% 
    mutate(cites_focal_paper = map_lgl(data, ~(this_article_doi %in% .x$doi_cited))) %>% 
    filter(!cites_focal_paper) %>% 
    mutate(cites_focal_papers_citations = map_int(data, ~sum(.x$doi_cited %in% cited_by_focal_paper$doi_cited))) %>% 
    filter(cites_focal_papers_citations >= 1) %>% 
    nrow()
  
  # do the citing papers cite any of the references in the focal paper?
  common_cites <- 
  cited_by_citing_papers %>% 
    group_by(doi_article) %>% 
    summarise(common_cites = sum(doi_cited %in% cited_by_focal_paper$doi_cited ))  %>% 
    count(common_cites) 
  
  n_i <- # cite focal article, but none of its references
    common_cites[common_cites$common_cites == 0,]$n
  
  n_j <- # cite focal article, and at least one of its references
    common_cites[common_cites$common_cites == 1,]$n
  
  disruption <- (n_i - n_j) / (n_i + n_j + n_k)
  
  
  disruption_output[[i]] <- tibble(doi = this_article_doi,
                                   n_i = n_i,
                                   n_j = n_j,
                                   n_k = n_k,
                                   disruption = disruption)
   print(i)
    
}

disruption_df <- 
  bind_rows(disruption_output) %>% 
  arrange(desc(disruption)) %>% 
  left_join(items_wth_dois_and_refs, 
            by = c('doi' = 'doi_article')) %>% 
  mutate(first_author = str_remove_all(authors, '\\"')) %>% 
  mutate(first_author = gsub("^(.*?),.*", "\\1", first_author)) %>% 
  mutate(short_ref = as.character(str_glue('{first_author} {year}, {str_trunc(title, 50)}')))

saveRDS(disruption_df, here("data", "derived-data", "disruption_df.rds"))

ggplot() +
  geom_histogram(data = disruption_df,
                 aes(disruption)) +
  theme_minimal(base_size = 12) +
  xlab("Disruption index") +
  scale_x_continuous(limits = c(-1, 1))

disruption_df_top_10 <-  
  disruption_df %>% 
  arrange(desc(disruption)) %>% 
  slice(1:10) 

disruption_df_bottom_10 <-  
  disruption_df %>% 
  arrange((disruption)) %>% 
  slice(1:10) 

y_pos <-  seq.int(from = 400, 
                  to = 210, 
                  length.out = length(disruption_df_top_10$disruption))

y_pos_bottom <-  seq.int(from = 10, 
                  to = 200, 
                  length.out = length(disruption_df_bottom_10$disruption))
  
ggplot() +
  geom_histogram(data = disruption_df,
                 aes(disruption),
                 alpha = 0.4) +
  theme_minimal(base_size = 12) +
  xlab(str_glue("Disruption index values for {nrow(disruption_df)} articles from Web of Science (1998-2018)")) +
  scale_x_continuous(limits = c(-1, 1)) +
  geom_segment(data = disruption_df_top_10,
          aes(x = disruption,
               xend = disruption,
               y = rep(0, length(disruption_df_top_10$disruption)),
               yend = y_pos),
          colour = "darkgreen",
          alpha = 0.3,
          size = 0.1) +
  geom_segment(data = disruption_df_bottom_10,
               aes(x = disruption,
                   xend = disruption,
                   y = rep(0, length(disruption_df_bottom_10$disruption)),
                   yend = y_pos_bottom),
               colour = "darkred",
               alpha = 0.3, 
               size = 0.1) +
  annotate("text", 
           x = disruption_df_top_10$disruption - 0,
           y = y_pos,
          label = disruption_df_top_10$short_ref, 
          colour = "darkgreen",
          size = disruption_df_top_10$disruption * 7,
          hjust = 1) +
  annotate("text", 
           x = disruption_df_bottom_10$disruption - 0,
           y = y_pos_bottom,
           label = disruption_df_bottom_10$short_ref, 
           colour = "darkred",
           size = -disruption_df_bottom_10$disruption * 15,
           hjust = 1)

ggsave(here("figures", "disruption-index.png"))


ggplot(disruption_df,
       aes(disruption, authors_n)) +
  geom_jitter() +
  geom_smooth(alpha = 0.2) +
  theme_minimal(base_size = 12)





  



