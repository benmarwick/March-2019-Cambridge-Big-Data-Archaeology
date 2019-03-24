
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

data_to_plot <- data_frame(year = archy_ctv_readme_20XX,
                           journal_name = top_archy_journal_names) %>%
  filter(!is.na(year),
         year <= 2018) %>%
  mutate(year = factor(year))


archaeology_articles_r_reproducible <-
  ggplot(data_to_plot,
         aes(year,
             fill = journal_name)) +
  geom_bar(position = "stack") +
  scale_y_continuous(breaks = 1:10) +
  scale_fill_discrete(name = "Journal")  +
  xlab("Year of publication") +
  ylab("Number of articles") +
  theme_minimal(base_size = 18) +
  theme(legend.text=element_text(size=16))

ggsave(plot = archaeology_articles_r_reproducible,
       filename = "archaeology_articles_r_reproducible.png",
       path = here::here('figures'),
       height = 9,
       width = 16)
