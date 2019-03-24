# Nosek citations -----------------------------------------------------------------

# from https://gist.github.com/benmarwick/931a7ede38afd473825baface2f0b8b8

# Nosek B.A., et al. (2015) Promoting an open research culture . Science, 348 (6242): 1422-1425.

# https://www.scopus.com/results/citedbyresults.uri?sort=plf-f&cite=2-s2.0-84933567354&src=s&nlo=&nlr=&nls=&imp=t&sid=FC638E8E6CD66FC3870075E5AB1B7B26.wsnAw8kcdt7IPYLO0V48gA%3a270&sot=cite&sdt=a&sl=0&origin=resultslist&offset=1&txGid=FC638E8E6CD66FC3870075E5AB1B7B26.wsnAw8kcdt7IPYLO0V48gA%3a33

# download csv

library(tidyverse)
library(here)

scopus <- readr::read_csv(here("data", "raw-data", "scopus.csv"))

# what disciplines have cited this paper the most?
library(dplyr)

# look at journal titles...
journal_titles <- 
  scopus %>% 
  group_by(`Source title`, `Cited by`) %>% 
  tally() %>% 
  arrange(desc(n))

# n is very small, many unique titles, not a good indicator

# look at affiliations
scopus %>% 
  group_by(Affiliations) %>% 
  tally() %>% 
  arrange(desc(n))

# journals by discpline from https://raw.githubusercontent.com/sdspieg/zotero-classify-articles/master/chrome/content/journals.json
# json <- "https://raw.githubusercontent.com/sdspieg/zotero-classify-articles/master/chrome/content/journals.json"
# library(jsonlite)
# journals <- fromJSON(json, flatten=TRUE)
# write_csv(journals, here("data", "derived-data", "journals.csv"))

journals <- read_csv(here("data", "derived-data", "journals.csv"))

journal_titles_by_discpline <- 
  journal_titles %>% 
  inner_join(journals, 
             by = c("Source title" = "Journal")) %>% 
  arrange(desc(n))


# many multiple matches, let's get uniques
journal_titles_by_discpline <-   
  journal_titles_by_discpline[!duplicated(journal_titles_by_discpline[,c('Source title','Main Discipline')]),]

# tally of disciplines
journal_titles_by_discpline_tally <- 
  journal_titles_by_discpline %>% 
  group_by(`Main Discipline`) %>% 
  summarise(n_articles = sum(n),
            n_cites = sum(`Cited by`, 
                          na.rm = TRUE)) %>% 
  arrange(desc(n_cites, n_articles)) %>% 
  dplyr::filter(!is.na(`Main Discipline` ))

library(ggplot2)
library(ggrepel)
ggplot(journal_titles_by_discpline_tally, 
       aes(n_articles,
           n_cites,
           label = `Main Discipline`)) +
  geom_point() +
  geom_text_repel() +
  scale_y_log10() +
  scale_x_log10() +
  theme_bw(base_size = 12) +
  xlab("Number of citing articles") +
  ylab("Number of citations of those articles citing Nosek et al.") +
  ggtitle(paste0("Disciplines of ", nrow(scopus), " articles citing \nNosek B.A., et al. (2015) Promoting an open research culture. \nScience, 348 (6242): 1422-1425.")) +
  labs(caption = paste0("Data from Scopus."))

ggsave(filename = here("figures", "citations_by_discpline.png"), 
       height = 6, 
       width = 6)

# what about the citing articles and which of those are most cited?



# http://janet.vertesi.com/sites/default/files/related_files/SOC-356-Vertesi_0.pdf

# http://anthrosource.onlinelibrary.wiley.com/hub/issue/10.1111/cuan.2001.16.issue-4/
