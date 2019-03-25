
archy_ctv_readme <- readLines(here::here('data/raw-data/README.md'))

journals <- c("Journal of Archaeological Method and Theory|J Archaeol Sci|Journal of Anthropological Archaeology|Journal of Archaeological Science|PLOS|Journal of Human Evolution|Asian Perspectives|Scientific Reports|PLoS ONE|The Holocene|Archaeology in Oceania|Quaternary International|Internet Archaeology|Quaternary Science Reviews|Quaternary International|Journal of Archaeological Science: Reports|Open Quaternary|Evolution and Human Behavior|PaleoAnthropology")

top_archy_journals <-
  str_subset(archy_ctv_readme, journals)

top_archy_journal_names <-
  unlist(str_extract_all(archy_ctv_readme, journals))


top_archy_journal_names <- ifelse(top_archy_journal_names == "J Archaeol Sci",
                                  "Journal of Archaeological Science",
                                  ifelse(top_archy_journal_names == "PLOS",
                                         "PLoS ONE",
                                         top_archy_journal_names))

archy_ctv_readme <- top_archy_journals
archy_ctv_readme <- str_remove_all(archy_ctv_readme, "[[:punct:]]")
archy_ctv_readme_20XX <- str_extract(archy_ctv_readme, " 20[[:digit:]]{2} ")
archy_ctv_readme_20XX <- str_squish(unlist(archy_ctv_readme_20XX))
archy_ctv_readme_20XX <- as.numeric(archy_ctv_readme_20XX)
archy_ctv_readme_20XX <- archy_ctv_readme_20XX[archy_ctv_readme_20XX > 2010]

number_of_reproducible_articles <- length(archy_ctv_readme_20XX)

data_to_plot <- 
  data_frame(year = archy_ctv_readme_20XX,
             journal_name = top_archy_journal_names) %>%
  filter(!is.na(year),
         year <= 2018) %>%
  group_by(year) 

journals_with_reproducible_articles <- 
data_to_plot %>% 
  group_by(journal_name) %>% 
  tally(sort = TRUE) %>% 
  ggplot(aes(reorder(journal_name, n),
             n)) +
  geom_col() +
  coord_flip() +
  xlab("") +
  theme_bw(base_size = 10) 

data_to_plot_tally <- 
data_to_plot %>% 
  tally() 

archaeology_articles_r_reproducible <-
  ggplot(data_to_plot_tally,
         aes(year, n)) +
  geom_point(size = 10) +
  geom_smooth(method = "lm", 
              alpha = 0.3) +
  scale_y_continuous(breaks = 1:10) +
  xlab("Year of publication") +
  ylab("Number of reproducible\njournal articles") +
  theme_minimal(base_size = 25) 

archaeology_articles_r_reproducible + 
  annotation_custom(ggplotGrob(journals_with_reproducible_articles), 
                    xmin= 2014, 
                    xmax= 2018, 
                    ymin= -3, 
                    ymax= 2.35) 

ggsave(
       filename = "archaeology_articles_r_reproducible.png",
       path = here::here('figures'),
       height = 9,
       width = 12)
