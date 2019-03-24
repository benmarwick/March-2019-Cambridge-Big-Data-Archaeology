# We did a 'Cited Reference Search' in WoS for title "R: A Language and Environment for Statistical Computing" and author = "R Core Team", which is the output from  `citation()`. This function first appeared in 2004 https://github.com/wch/r-source/commits/9a5c086855d2e8fb07f670e3315871b2de31a4fd/src/library/base/inst/CITATION
#
# Note that before 2012 the authors were "R Development Core Team", and then it was simplified to "R Core Team" https://github.com/wch/r-source/commit/9a5c086855d2e8fb07f670e3315871b2de31a4fd#diff-80bb8a8b133a6204e9642d76791927e2
#
# This is our exact search string for WoS:

# Results: 38,040
# (from Web of Science Core Collection)
# You searched for: CITED AUTHOR: ("R DEV COR TEAM" OR "R # CORE" OR "R CORE TEAM" OR "R DEVELOPMENT CORE TEAM")
# Refined by: DOCUMENT TYPES: ( ARTICLE ) AND LANGUAGES: ( ENGLISH ) AND RESEARCH AREAS: ( ENVIRONMENTAL SCIENCES ECOLOGY )
# Timespan: 1900-2018. Indexes: SCI-EXPANDED, SSCI, A&HCI, # ESCI.

# field tags
# TS= Topic
# TI= Title
# AU= Author [Index]
# AI= Author Identifiers
# GP= Group Author [Index]
# ED= Editor
# SO= Publication Name [Index]
# DO= DOI
# PY= Year Published
# AD= Address
# OG= Organization-Enhanced [Index]
# OO= Organization
# SG= Suborganization
# SA= Street Address
# CI= City
# PS= Province/State
# CU= Country/Region
# ZP= Zip/Postal Code
# FO= Funding Agency
# FG= Grant Number
# FT= Funding Text
# SU= Research Area
# WC= Web of Science Category
# IS= ISSN/ISBN
# UT= Accession Number
# PMID= PubMed ID


cran_cites_files <-
  list.files(here::here("data", "raw-data", "cited_cran"),
             full.names = TRUE)


cran_cites_all_areas_all_years <-
  map(cran_cites_files,
      ~readr::read_tsv(.x,
                       quote = "",
                       col_types = cols(.default = col_character()))) %>%
  bind_rows() %>%
  mutate(PY = as.numeric(PY)) %>%
  filter(PY <= 2017)

# limit to top journals
top_journals_for_cran_cites <-
  cran_cites_all_areas_all_years %>%
  group_by(SO) %>%
  tally(sort = TRUE)

how_many_articles_in_top_journals_for_cran_cites <-
  sum(top_journals_for_cran_cites$n)


top_journals_for_cran_cites_articles_per_year <-
  read_csv(here::here("data", 
                       "raw-data", 
"top_journals_for_cran_cites_articles_per_year.csv")) %>%
  select(-X1) %>%
  gather(variable, value, -year) %>%
  separate(variable,
           into = str_glue('X{1:4}'),
           by = "_") %>%
  select(year, X4, value)

# tally by year
cran_cites_all_areas_top_journals_by_year <-
  cran_cites_all_areas_all_years %>%
  filter(SO %in% top_journals_for_cran_cites$SO[1:10]) %>%
  group_by(SO, PY) %>%
  tally(sort = TRUE) %>%
  mutate(journalname = str_remove_all(SO, " |-"),
         PY = as.integer(PY)) %>%
  left_join(top_journals_for_cran_cites_articles_per_year,
            by = c('journalname' = 'X4',
                   'PY' = "year")) %>%
  mutate(prop = n / value ) %>%
  filter(SO != 'JOURNAL OF STATISTICAL SOFTWARE') %>%
  ungroup()

articles_shown_in_plot <-
  cran_cites_all_areas_top_journals_by_year %>%
  filter(!is.na(prop)) %>%
  summarise(sum_n = sum(n)) %>%
  pull(sum_n)

# nice labels with n for plot
library(glue)
cran_cites_all_areas_top_journals_by_year <-
  cran_cites_all_areas_top_journals_by_year %>%
  group_by(SO) %>%
  summarise(total_n_articles = sum(n)) %>%
  mutate(SO_n = as.character(glue('{SO} (n = {total_n_articles})'))) %>%
  right_join(cran_cites_all_areas_top_journals_by_year,
             by = "SO")


min_y <- 2007
max_y <- 2025
all_sciences_citing_R_over_time <-
  ggplot(cran_cites_all_areas_top_journals_by_year,
         aes(PY,
             prop,
             colour = SO)) +
  geom_line(size = 2) +
  geom_text_repel(
    data = subset(cran_cites_all_areas_top_journals_by_year,
                  PY == max(PY)),
    aes(label = str_wrap(SO_n, 40)),
    size = 4,
    nudge_x = 0,
    hjust = 0,
    segment.color = NA,
    direction = "y"
  ) +
  scale_x_continuous(breaks = min_y:max_y,
                     labels = c(min_y:2017, rep("", length(2018:max_y))),
                     limits = c(min_y, max_y)) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Publication year") +
  ylab("Percentage of articles in that journal in that year") +
  theme_minimal() +
  theme(legend.position="none",
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

ggsave(plot = all_sciences_citing_R_over_time,
       filename = "all_sciences_citing_R_over_time.png",
       path = here::here('figures'),
       height = 5,
       width = 9)


# Data are from an 'advanced search' of apps.webofknowledge.com using WC=Archaeology on apps.webofknowledge.com.

# Results: 44,047
# (from Web of Science Core Collection)
# You searched for: (WC=Archaeology) AND LANGUAGE: (English) AND DOCUMENT TYPES: # (Article)
# Timespan: 1900-2018. Indexes: SCI-EXPANDED, SSCI, A&HCI, ESCI

# 2018 has  3818
# 1-499 0 
# 500-999 1 
# 1000-1499 2 
# 1500-1999 3
# 2000-2499 4
# 2500-2999 5
# 3000-3499 6
# 3500-3818 7


#- get journals with the most number of articles

archy_files <-
  list.files(here::here("data", "raw-data", "wos_archaeology"),
             full.names = TRUE)

archy_all_years <-
  map(archy_files,
      ~readr::read_tsv(.x,
                       quote = "",
                       col_types = cols(.default = col_character()))) %>%
  bind_rows() %>%
  mutate(PY = as.numeric(PY)) %>%
  filter(PY <= 2019)

# limit to top journals
top_journals_for_archy<-
  archy_all_years %>%
  group_by(SO) %>%
  tally(sort = TRUE)

# n articles in top journals per year
top_journals_for_archy_per_year <-
  archy_all_years %>%
  group_by(SO, PY) %>%
  tally() %>%
  arrange(PY)

how_many_articles_in_top_archy_journals <-
  sum(top_journals_for_archy$n)

#-- get articles citing R that are archaeology


# Results: xxx
# (from Web of Science Core Collection)
# You searched for: CITED AUTHOR: ("R DEV COR TEAM" OR "R CORE" OR "R CORE TEAM" OR "R DEVELOPMENT CORE TEAM")
# Refined by: WEB OF SCIENCE CATEGORIES: ( ANTHROPOLOGY ) AND WEB OF SCIENCE CATEGORIES: ( ARCHAEOLOGY )
# Timespan: All years. Indexes: SCI-EXPANDED, SSCI, A&HCI, ESCI

archy_cran_files <-
  list.files(here::here("data", "raw-data", "cited_cran_archaeology/"),
             full.names = TRUE)

archy_cran_all_years <-
  map(archy_cran_files,
      ~readr::read_tsv(.x,
                       quote = "",
                       col_types = cols(.default = col_character()))) %>%
  bind_rows() %>%
  mutate(PY = as.numeric(PY)) %>%
  filter(PY <= 2019)

# tally by year
archy_cran_cites_all_areas_top_journals_by_year <-
  archy_cran_all_years %>%
  filter(SO %in% top_journals_for_archy$SO) %>%
  group_by(SO, PY) %>%
  tally(sort = TRUE)  %>%
  left_join(top_journals_for_archy_per_year,
            by = c('SO', 'PY')) %>%
  mutate(prop = n.x / n.y) %>%
  ungroup()

total_number_archaeology_articles_citing_r <- sum(archy_cran_cites_all_areas_top_journals_by_year$n.x)

# check for slight differences in journal names
# sort(unique(archy_cran_cites_all_areas_top_journals_by_year$SO))

# nice labels with n for plot
archy_cran_cites_all_areas_top_journals_by_year <-
  archy_cran_cites_all_areas_top_journals_by_year %>%
  group_by(SO) %>%
  summarise(total_n_articles = sum(n.x)) %>%
  mutate(SO_n = as.character(glue('{SO} (n = {total_n_articles})'))) %>%
  right_join(archy_cran_cites_all_areas_top_journals_by_year,
             by = "SO")


min_y <- 2007
max_y <- 2040

all_archaeology_citing_R_over_time <-
  ggplot(archy_cran_cites_all_areas_top_journals_by_year,
         aes(PY,
             prop,
             colour = SO)) +
  geom_line(size = 2) +
  geom_point() +
  geom_text_repel(
    data = archy_cran_cites_all_areas_top_journals_by_year %>%
      group_by(SO) %>%
      slice(which.max(PY)) ,
    aes(label = SO_n),
    direction = "y",
    size = 5,
    nudge_x = 1,
    hjust = 0) +
  theme_minimal() +
  theme(legend.position="none",
        axis.text.x = element_text(angle = 90,
                                   hjust = 1,
                                   vjust = 0.5)) +
  scale_x_continuous(breaks = min_y:max_y,
                     labels = c(min_y:2018,
                                rep("",
                                    length(2019:max_y))),
                     limits = c(min_y, max_y)) +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 0.2)) +
  xlab("Publication year") +
  ylab("Percentage of articles in that journal")

ggsave(plot = all_archaeology_citing_R_over_time,
       filename = "all_archaeology_citing_R_over_time.png",
       path = here::here('figures'),
       height = 7,
       width = 11)

# look at the trend in JAS, where there are the most articles

how_many_articles_in_jas <-  
  archy_cran_cites_all_areas_top_journals_by_year$total_n_articles[1]

library(ggpmisc)
jas_cites_r <-
  archy_cran_cites_all_areas_top_journals_by_year %>%
  filter(SO == "JOURNAL OF ARCHAEOLOGICAL SCIENCE") %>%
  ggplot(aes(PY,
             prop)) +
  geom_point(size = 4) +
  geom_smooth(method = "lm",
              se = FALSE) +
  stat_poly_eq(aes(label =  paste(..eq.label..,
                                  ..adj.rr.label..,
                                  sep = "~~~~")),
               formula = y~x,
               parse = TRUE,
               size = 3,
               label.y.npc = 0.7) +
  stat_fit_glance(label.y.npc = 0.7,
                  size = 3,
                  method = "lm",
                  method.args = list(formula = y ~ x),
                  geom = "text",
                  aes(label = paste("p-value: ",
                                    signif(..p.value..,
                                           digits = 4)))) +
  xlab("year") +
  ylab("Proportion of articles citing R") +
  ggtitle(expression(paste("Articles citing R in ",
                           italic("Journal of Archaeological Science")))) +
  theme_bw(base_size = 6)

# combine main plot and subplot
all_archaeology_citing_R_over_time_and_subplot <-
  all_archaeology_citing_R_over_time  +
  annotation_custom(grob = ggplotGrob(jas_cites_r),
                    xmin = 2030, xmax = 2040,
                    ymin = 0.10, ymax = 0.17)

ggsave(plot = all_archaeology_citing_R_over_time_and_subplot,
       filename = "all_archaeology_citing_R_over_time_and_subplot.png",
       path = here('figures'),
       height = 9,
       width = 12)

## What are R-using archaeologists writing about?

# https://www.tidytextmining.com/twitter.html

# These are all archy articles
# wc_archaeology_2007_to_2017

# These are ones citing R
# cran_cites_in_archy_journals

# subtract ones citing R from ones that do not

library(tidytext)
library(Hmisc)

wc_archaeology_2007_to_2017_no_R_cites <-
  archy_all_years %>%
  filter(TI %nin% archy_cran_all_years$TI)

# combine
archy_journal_articles <-
  bind_rows(wc_archaeology_2007_to_2017_no_R_cites %>%
              mutate(ID = "no_R_cites",
                     VL = as.character(VL)),
            archy_cran_all_years %>%
              mutate(ID = "has_R_cites"))


tidy_cites <-
  archy_journal_articles %>%
  unnest_tokens(word, TI) %>%
  filter(!word %in% c(stop_words$word, "archaeology",
                      "archaeological",
                      "analysis", "study", "site", "period", "past", "patterns", "investigation", "analyses", "late", "region", "based", "central", "north", "south", "east", "west", "western", "eastern", "ad", "pre", "sites", "evidence", "prehistoric", "remains", "change", "de", "identification", "northern", "southern", "valley", "implications", "valley", "history", "historical", "modern", "context", "stable", "middle", "approach", "san", "artifacts", "palaeolithic"),
         str_detect(word, "[a-z]"))

frequency <-
  tidy_cites %>%
  group_by(ID) %>%
  count(word, sort = TRUE) %>%
  left_join(tidy_cites %>%
              group_by(ID) %>%
              summarise(total = n())) %>%
  mutate(freq = n/total)

frequency <-
  frequency %>%
  select(ID, word, freq) %>%
  spread(ID, freq) %>%
  arrange(no_R_cites, has_R_cites)

library(scales)
archaeology_articles_word_freqs <-
  ggplot(frequency,
         aes(no_R_cites,
             has_R_cites)) +
  geom_abline(color = "red",
              size = 1) +
  geom_point(alpha = 0.1,
             size = 2.5,
             width = 0.25,
             height = 0.25) +
  geom_text_repel(aes(label = word),
                  size = 3,
                  segment.colour = "grey80") +
  scale_x_log10(labels = percent_format(),
                limits = c(0.00075, 0.015)) +
  scale_y_log10(labels = percent_format(),
                limits = c(0.00075, 0.015)) +
  theme_minimal(base_size = 16) +
  xlab("Articles that do not cite R") +
  ylab("Articles that cite R") +
  coord_equal()

ggsave(plot = archaeology_articles_word_freqs,
       filename = "archaeology_articles_word_freqs.png",
       path = here::here('figures'),
       height = 9,
       width = 9)

