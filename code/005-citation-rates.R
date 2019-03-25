# archy papers that use code are more often cited?
# is TC total citations?

archy_journal_articles_tally <-
  archy_journal_articles %>%
  group_by(ID) %>%
  tally() %>%
  pull(n)

archy_journal_articles_citations <- 
  archy_journal_articles %>%
  mutate(TC = as.numeric(TC),
         ID = ifelse(ID == "has_R_cites",
                     "Cites R", "Doesn't cite R")) %>%
  group_by(ID, PY) %>%
  summarise(median = median(TC)) %>%
  filter(PY >= 2010)

archy_journal_articles_n <- 
  archy_journal_articles %>%
  mutate(TC = as.numeric(TC),
         ID = ifelse(ID == "has_R_cites",
                     "Cites R", "Doesn't cite R")) %>%
  filter(PY >= 2010) %>% 
  select(ID, PY, TC) 

ggplot(archy_journal_articles_n,
       aes(ID, TC)) +
  geom_boxplot() +
  scale_y_log10() +
  theme_minimal(base_size = 16)

# compute t-test
archy_journal_articles_citations_t_test <- 
  t.test(  archy_journal_articles_n$TC ~  factor(archy_journal_articles_n$ID))

tt <- t.test(archy_journal_articles_n$TC ~  factor(archy_journal_articles_n$ID))
tvalue <- tt$statistic %>% formatC(digits = 2, format = "f")
pvalue <- tt$p.value %>% formatC(digits = 5, format = "f")
df <- round(tt$parameter, 0)

dd <- archy_journal_articles_n %>% 
  group_by(ID) %>% 
  summarise(mean = mean(TC))

dd_cites_mean <- dd %>% filter(ID == "Cites R") %>%  pull(mean) %>% round(1)
dd_no_cites_mean <- dd %>% filter(ID == "Doesn't cite R") %>%  pull(mean) %>% round(1) 
test_explanation <- 
  str_glue("On average, articles citing R \nhave higher numbers\nof citations (m = {dd_cites_mean}) than articles\nthat do not (m =  {dd_no_cites_mean})\nt({df}) = {tvalue}, p =  {pvalue}")

archy_journal_articles_citations_plot <- 
  ggplot(archy_journal_articles_citations %>% 
           filter(PY < 2018),
         aes(ID,
             median)) +
  geom_col() +
  facet_wrap( ~ PY) +
  theme_minimal(base_size = 16) +
  ylab("Median number of citations") +
  xlab("")


# add text to empty corner
png(here::here("figures", "archy_journal_articles_citations_plot_cap.png"),
    width = 20, height = 15, units = "cm", res = 300)
print(archy_journal_articles_citations_plot)
library(grid)
grid.text(test_explanation, 
          x = 0.70, y = 0.25, 
          gp=gpar(fontsize=11),
          just = 'left')
invisible(dev.off())
