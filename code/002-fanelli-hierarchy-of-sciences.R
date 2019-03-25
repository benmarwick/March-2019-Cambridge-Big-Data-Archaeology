
library(tidyverse)
library(here)

# Bibliometric Evidence for a Hierarchy of the Sciences
# Daniele Fanelli , Wolfgang Glänzel
# Published: June 26, 2013
# https://doi.org/10.1371/journal.pone.0066938

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

items_wth_refs <- 
  items_df %>% 
  filter(!is.na(refs)) %>% 
  filter(!is.na(year)) 

# ------------------------------------------------------------------
# how many items do we have?
nrow(items_wth_refs) # 6864

# span what time period?
range(items_wth_refs$year) # 1998 2018

#  ------------------------------------------------------------------

# number of authors + greater scope and need for collaboration [9,44]
library(ggplot2)

# all on one

items_df %>% 
  filter(!is.na(year)) %>% 
  ggplot(aes(year,
             log(authors_n))) +
  geom_boxplot(colour = "red", 
               size = 1)  +
  scale_y_continuous(limits = c(0, 5)) +
  scale_x_continuous(labels = NULL,
                     name = NULL) +
  theme_minimal() +
  theme(panel.grid  = element_blank()) +
  coord_equal(ratio = 6)

ggsave(here("figures", "box_num_authors.svg"))

# over time
items_df %>% 
  filter(!is.na(year)) %>% 
  ggplot(aes(year,
             log(authors_n))) +
  geom_smooth() +
  geom_jitter(alpha = 0.01) +
  theme_minimal() +
  ylab("Number of authors (ln)")

ggsave(here("figures", "box_num_authors_over_time.png"))

# by journal, wow yes!
library(ggridges)
items_df %>% 
  filter(!is.na(year)) %>% 
  group_by(journal) %>% 
  filter(n() > 100) %>% 
  mutate(log_authors_n = log(authors_n),
         journal_wrp = str_wrap(journal, 30)) %>% 
  ggplot(aes(y = reorder(journal_wrp,
                     authors_n), 
             x = log_authors_n,
             fill = ..x..,
             height = ..density..)) +
  geom_density_ridges_gradient(stat = "density",
                               colour = "white") +
  scale_fill_viridis_c() +
  theme_minimal(base_size = 10) +
  ylab("") + 
  xlab("Number of authors (ln)") +
  guides(fill = 'none')

ggsave(here("figures", "ridge_num_authors_by_journal.png"),
       h = 4, w = 6)

# histogram

tally_authors_n <- 
  items_df %>% 
  filter(!is.na(year)) %>% 
  group_by(authors_n) %>% 
  tally() %>% 
  mutate(perc = n / sum(n) * 100) %>% 
  mutate(cum_per = cumsum(perc))


items_df %>% 
  filter(!is.na(year)) %>% 
  ggplot(aes(authors_n)) +
  geom_histogram()  +
  theme_minimal() +
  xlab("Number of authors") + 
  ylab("Count") +
  geom_vline(xintercept = 4, 
             colour = "red",
             size = 2) +
  annotate("text", 
           x = 15,
           y = 2500,
           label = "80% of archaeology\npapers have <=4 authors",
           size = 7)

ggsave(here("figures", "box_num_histogram.png"))

#  ------------------------------------------------------------------
# length of article - less need to introduce, justify and explain study [47,52]


# all on one
library(ggplot2)

items_df %>% 
  filter(!is.na(year)) %>% 
  ggplot(aes(year,
             log(pages_n))) +
  geom_boxplot(colour = "red", 
               size = 1)  +
  scale_y_continuous(limits = c(0, 5)) +
  scale_x_continuous(labels = NULL,
                     name = NULL) +
  theme_minimal() +
  theme(panel.grid  = element_blank()) +
  coord_equal(ratio = 6)

ggsave(here("figures", "box_num_pages.svg"))


# over time
items_df %>% 
  filter(!is.na(year)) %>% 
  ggplot(aes(year,
             log(pages_n),
             group = year)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.02) +
  theme_minimal() +
  ylab("Number of pages (ln)")

ggsave(here("figures", "box_num_pages_over_time.png"))

# by journal, wow yes!
library(ggforce)
items_df %>% 
  filter(!is.na(year)) %>% 
  group_by(journal) %>% 
  filter(n() > 100) %>% 
  mutate(log_pages_n = log(pages_n),
         journal_wrp = str_wrap(journal, 30)) %>% 
  ggplot(aes(y = reorder(journal_wrp,
                         -pages_n), 
             x = log_pages_n,
             fill = ..x..,
             height = ..density..)) +
  geom_density_ridges_gradient(stat = "density",
                               colour = "white") +
  scale_fill_viridis_c() +
  theme_minimal(base_size = 10) +
  ylab("") + 
  xlab("Number of pages (ln)") +
  guides(fill = 'none')

ggsave(here("figures", "ridge_num_pages_by_journal.png"),
       h = 4, w = 6)

#  ------------------------------------------------------------------
# number of references - less need to justify, explain and support study [47]

# all on one

items_df %>% 
  filter(!is.na(year)) %>% 
  ggplot(aes(year,
             sqrt(refs_n))) +
  geom_boxplot(colour = "red", 
               size = 1)  +
  scale_y_continuous(limits = c(0, 20)) +
  scale_x_continuous(labels = NULL,
                     name = NULL) +
  theme_minimal() +
  theme(panel.grid  = element_blank()) +
  coord_equal(ratio = 6)

ggsave(here("figures", "box_num_refs.svg"))

# over time
items_df %>% 
  filter(!is.na(year)) %>% 
  ggplot(aes(year,
             sqrt(refs_n),
             group = year)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.01)+
  theme_minimal() +
  ylab("Number of references (ln)")

ggsave(here("figures", "box_num_refs_over_time.png"))

# by journal, wow yes!
items_df %>% 
  filter(!is.na(year)) %>% 
  group_by(journal) %>% 
  filter(n() > 100) %>% 
  mutate(sqrt_refs_n = sqrt(refs_n),
         journal_wrp = str_wrap(journal, 30),
         refs_per_page = refs_n / pages_n) %>% 
  ggplot(aes(y = reorder(journal_wrp,
                         -refs_per_page), 
             x = refs_per_page,
             fill = ..x..,
             height = ..density..)) +
  geom_density_ridges_gradient(stat = "density",
                               colour = "white") +
  scale_fill_viridis_c() +
  theme_minimal(base_size = 10) +
  ylab("") + 
  xlab("Number of references per page") +
  guides(fill = 'none')

ggsave(here("figures", "ridge_num_refs_by_journal.png"),
       h = 4, w = 6)

#  ------------------------------------------------------------------
#  references to monographs - focus on simpler questions; less need to justify, explain and support study [53,54]

# not easy...
#  ------------------------------------------------------------------
# age of references - faster settling of disagreements; greater potential to build research upon previous findings [44,56]

# Derek de Solla Price proposed an index, which measures the proportion of
# cited references published in the five years preceding the citing paper

items_wth_refs <- 
  items_df %>% 
  filter(!is.na(refs)) %>% 
  filter(!is.na(year)) 
  
# years are ", YYYY, "
library(stringr)

# output storage
prices_index <- vector("list", length = nrow(items_wth_refs))

# loop, 
for(i in seq_len(nrow(items_wth_refs))){
  
  refs <-  items_wth_refs$refs[i]
  year <-  items_wth_refs$year[i]
  
  ref_years <- 
    as.numeric(str_match(str_extract_all(refs, ", [0-9]{4}, ")[[1]], "\\d{4}"))
  
  preceeding_five_years <-  
    seq(year - 5, year, 1)
  
  refs_n_in_preceeding_five_years <- 
    ref_years[ref_years %in% preceeding_five_years]
  
  prices_index[[i]] <- 
    length(refs_n_in_preceeding_five_years) / length(ref_years)
  
  # for debugging
  # print(i)

}

prices_index <- flatten_dbl(prices_index)

# how many are zero?
length(prices_index[prices_index < 0.00000001])

# add to data frame
items_wth_refs$prices_index <-  prices_index

# saveRDS(items_wth_refs, "data/items_wth_refs_price_index.rds")

# plot

# all on one

items_wth_refs %>% 
  filter(!is.na(year)) %>% 
  ggplot(aes(year,
             prices_index)) +
  geom_boxplot(colour = "red", 
               size = 1)  +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(labels = NULL,
                     name = NULL) +
  theme_minimal() +
  theme(panel.grid  = element_blank()) +
  coord_equal(ratio = 30)

ggsave(here("figures", "box_age_refs.svg"))


# over time
items_wth_refs %>% 
  filter(!is.na(year)) %>% 
  ggplot(aes(year,
             prices_index,
             group = year)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.01)+
  theme_minimal() +
  ylab("Price's Index")

ggsave(here("figures", "box_age_refs_over_time.png"))

# by journal
items_wth_refs %>% 
  filter(!is.na(year)) %>% 
  group_by(journal) %>% 
  filter(n() > 100) %>% 
  mutate(journal_wrp = str_wrap(journal, 30),
         mean_price = median(prices_index)) %>% 
  ggplot(aes(y = reorder(journal_wrp,
                         mean_price), 
             x = prices_index,
             fill = ..x..,
             height = ..density..)) +
  geom_density_ridges_gradient(stat = "density",
                               colour = "white") +
  scale_fill_viridis_c() +
  theme_minimal(base_size = 10) +
  ylab("") + 
  xlab("Price's Index") +
  guides(fill = 'none')

ggsave(here("figures", "ridge_age_refs_by_journal.png"),
       h = 4, w = 6)

#  ------------------------------------------------------------------
# diversity of sources - fewer research topics, which are of more general interest [47,57]

# journal name as species
# article as habitat


# simplify the refs, since they are a bit inconsistent
library(stringr)
ref_list1 <- map(items_wth_refs$refs, ~tolower(.x))
ref_list2 <- map(ref_list1, ~str_replace_all(.x, "\\.|,| ", ""))
ref_list3 <- map(ref_list2, ~str_split(.x, "\n"))
ref_list4 <- map(ref_list3, ~data_frame(x = .x))
ref_list5 <- bind_rows(ref_list4, .id = "id") # 8042
ref_list6 <- unnest(ref_list5)
# one long vec of all refs

# get the journal names out of the refs
ref_list7 <- 
ref_list6 %>% 
  mutate(journal_name = gsub("\\-", "", x)) %>% 
  mutate(journal_name = gsub("\\:", "", journal_name)) %>% 
  mutate(journal_name = gsub("^[a-z'\\(\\)\\:]+[0-9]{4}", "", journal_name)) %>% 
  mutate(journal_name = gsub("v[0-9]+.*", "", journal_name)) %>% 
  mutate(journal_name = gsub("p[0-9]+$", "", journal_name))

# prepare to compute shannon and join with other variables
items_wth_refs$id <- 1:nrow(items_wth_refs)

# tally of all referenced items
all_cited_items <- 
  ref_list7 %>% 
  select(x) %>% 
  group_by(x) %>% 
  tally() %>% 
  arrange(desc(n))  %>% 
  filter(x != "") %>% 
  filter(str_length(x) > 4)

  # get a list of the top journals
top_journals <- 
  ref_list7 %>% 
  select(journal_name) %>% 
  group_by(journal_name) %>% 
  tally() %>% 
  filter(n > 50) %>% 
  arrange(desc(n)) 

# In the Shannon index, p_i is the proportion (n/N) of individuals of 
# one particular species (journal) found (n) divided by the total 
# number of individuals found (N), ln is the natural log, Σ is the 
# sum of the calculations, and s is the number of species. 

# compute diversity of all citations
# for each article (habitat)
shannon_per_item <- 
ref_list7 %>% 
  group_by(id, x) %>% 
  tally() %>% 
  mutate(n_in_article = n) %>% 
  select(-n) %>% 
  left_join(all_cited_items) %>% 
  mutate(p_i = n / sum(n, na.rm = TRUE)) %>% 
  mutate(p_i_ln = log(p_i)) %>% 
  group_by(id) %>% 
  summarise(shannon = -sum(p_i * p_i_ln, na.rm = TRUE)) %>% 
  mutate(id = as.numeric(id)) %>% 
  arrange(id)  %>% 
  left_join(items_wth_refs)

# all on one
shannon_per_item %>% 
  filter(!is.na(year)) %>% 
  ggplot(aes(year,
             shannon)) +
  geom_boxplot(colour = "red",
               size = 1)  +
  scale_y_continuous(limits = c(0, 5)) +
  scale_x_continuous(labels = NULL,
                     name = NULL) +
  theme_minimal()  +
  theme(panel.grid  = element_blank()) +
  coord_equal(ratio = 4.5)

ggsave(here("figures", "box_div_refs.svg"))

# over time
shannon_per_item %>% 
  filter(!is.na(year)) %>% 
  ggplot(aes(year,
             shannon,
             group = year)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.01)+
  theme_minimal() +
  ylab("Shannon Index")

ggsave(here("figures", "box_div_refs_over_time.png"))

# by journal
shannon_per_item %>% 
  filter(!is.na(year)) %>% 
  group_by(journal) %>% 
  filter(n() > 100) %>% 
  mutate(journal_wrp = str_wrap(journal, 30),
         mean_shannon = median(shannon)) %>% 
  ggplot(aes(y = reorder(journal_wrp,
                         -mean_shannon), 
             x = shannon,
             fill = ..x..,
             height = ..density..)) +
  geom_density_ridges_gradient(stat = "density",
                               colour = "white") +
  scale_fill_viridis_c() +
  theme_minimal(base_size = 10) +
  ylab("") + 
  xlab("Shannon Index for the diversity of sources") +
  guides(fill = 'none')

ggsave(here("figures", "ridge_div_refs_by_journal.png"),
       h = 4, w = 6)

# relative title length + clearly defined, substantive research
# questions [52,58], total number of words, divided by total 
# number of pages.

items_df_title <- 
  items_df %>% 
  filter(!is.na(pages_n)) %>%  
  filter(!is.na(title_n)) %>% 
  mutate(relative_title_length = log(title_n / pages_n))


# all on one
items_df_title %>% 
  filter(!is.na(year)) %>% 
  ggplot(aes(year,
             relative_title_length)) +
  geom_boxplot(colour = "red", 
               size = 1)  +
  scale_y_continuous(limits = c(-5, 5),
                     breaks =  seq(-5, 5, 1),
                     labels = seq(-5, 5, 1)) +
  scale_x_continuous(labels = NULL,
                     name = NULL) +
  theme_minimal() +
  theme(panel.grid  = element_blank()) +
  coord_equal(ratio = 5)

ggsave(here("figures", "box_title_length.svg"))

# over time
items_df_title %>% 
  filter(!is.na(year)) %>% 
  ggplot(aes(year,
             relative_title_length,
             group = year)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.01)+
  theme_minimal() +
  ylab("Relative title length")

ggsave(here("figures", "box_title_length_over_time.png"))

# by journal
items_df_title %>% 
  filter(!is.na(year)) %>% 
  group_by(journal) %>% 
  filter(n() > 100) %>% 
  mutate(journal_wrp = str_wrap(journal, 30),
         mean_relative_title_length= median(relative_title_length)) %>% 
  ggplot(aes(y = reorder(journal_wrp,
                         mean_relative_title_length), 
             x = relative_title_length,
             fill = ..x..,
             height = ..density..)) +
  geom_density_ridges_gradient(stat = "density",
                               colour = "white") +
  scale_fill_viridis_c() +
  theme_minimal(base_size = 10) +
  ylab("") + 
  xlab("Relative title length") +
  guides(fill = 'none')

ggsave(here("figures", "ridge_title_length_over_time.png"),
       h = 4, w = 6)


# use of first person (singular vs. plural) in abstracts, - universal validity of claims; less scope for argumentation; fewer appeals to opinion and authority [59], the proportion of first person pronouns, both singular and plural (i.e. ‘‘I’’, ‘‘me’’, ‘‘mine’’, ‘‘we’’, ‘‘our’’ etc.) among all words in the abstract.
  
i <- c(" i'd ", " i'll ", " i'm ", " i've ", " i ")
me <- c(" me ", " my ", " mine ")
us <- c(" us ", " our ", " ours ")
first_persons <- c(i, me, us)

library(stringi)

# function to compute proportion of first person pronouns
first_person_prop <- function(x){
  x <- tolower(x)
  firsts <- sum(str_count(x, first_persons))
  abstract_n_words <- stri_count_words(x)
  firsts / abstract_n_words
}


# compute proportions, single author papers only
items_df_abstract <- 
items_df %>% 
  mutate(abstract_first_person = first_person_prop(abstract)) 

# plots
# all on one
items_df_abstract %>% 
  filter(!is.na(year)) %>% 
  ggplot(aes(year,
             abstract_first_person)) +
  geom_boxplot()  +
  scale_x_continuous(labels = NULL,
                     name = NULL) +
  theme_minimal()

# over time
items_df_abstract %>% 
  filter(!is.na(year)) %>% 
  ggplot(aes(year,
             abstract_first_person,
             group = year)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.01)+
  theme_minimal()

# by journal
items_df_abstract %>% 
  filter(!is.na(year)) %>% 
  group_by(journal) %>% 
  filter(n() > 100) %>% 
  ggplot(aes(reorder(journal,
                     abstract_first_person), 
             abstract_first_person,
             group = journal)) +
  geom_boxplot()  +
  coord_flip() +
  theme_minimal()

#--------------------------------
# Six variables over time

over_time <- 
  items_df_title %>% 
  left_join(items_df_title) %>% 
  left_join(shannon_per_item) %>% 
  filter(!is.na(year)) %>% 
  group_by(journal) %>% 
  filter(n() > 100) %>% 
  mutate(log_authors_n = log(authors_n),
         log_pages_n = log(pages_n),
         sqrt_refs_n = sqrt(refs_n),
         journal_wrp = str_wrap(journal, 30)) %>% 
  select(year,
         log_authors_n,
         log_pages_n,
         prices_index, 
         shannon,
         sqrt_refs_n, 
         relative_title_length)

over_time_long <- 
over_time %>% 
  ungroup() %>% 
  select(-journal) %>% 
  gather(variable, 
         value,
         -year) %>% 
  filter(value != -Inf,
         value !=  Inf) %>% 
  mutate(variable = case_when(
    variable == "log_authors_n" ~ "N. of authors (ln)",
    variable == "log_pages_n"   ~ "N. of pages (ln)",
    variable == "prices_index"  ~ "Price's index",
    variable == "shannon"  ~ "Shannon div. of sources",
    variable == "sqrt_refs_n"  ~ "N. of refs (sqrt)",
    variable == "relative_title_length"  ~ "Relative title length (ln)"
  ))

# compute beta estimates so we can colour lines to indicate more or
# less scientific
library(broom)
over_time_long_models <- 
over_time_long %>% 
  group_nest(variable) %>% 
  mutate(model = map(data, ~tidy(lm(value ~ year, data = .)))) %>% 
  unnest(model) %>% 
  filter(term == 'year') %>% 
  mutate(becoming_more_scientific = case_when(
    variable == "N. of authors (ln)" & estimate > 0 ~ "TRUE",
    variable == "N. of pages (ln)"   & estimate < 0 ~ "TRUE",
    variable == "N. of refs (sqrt)"  & estimate < 0 ~ "TRUE",
    variable == "Price's index"     & estimate > 0 ~ "TRUE",
    variable == " Relative title length (ln)"     & estimate > 0 ~ "TRUE",
    variable == "Shannon div. of sources"     & estimate < 0 ~ "TRUE",
    TRUE ~ "FALSE"
  )) 

# join with data
over_time_long_colour <- 
over_time_long %>% 
  left_join(over_time_long_models)
  
ggplot(over_time_long_colour,
       aes(year, 
           value,
           colour = becoming_more_scientific)) +
  geom_point(alpha = 0.05) +
  geom_smooth(method = "lm", 
              size = 3) +
  facet_wrap( ~ variable,
              scales = "free_y") +
  theme_minimal(base_size = 12) +
  scale_color_manual(values = c("red", "green")) +
  guides(colour = "none") +
  ylab("")

ggsave(here("figures", "facet_six_variables_over_time.png"),
       h = 4, w = 6)
  
  

#----------------------------- END ---------------------------------------------


# sharing of references: clustering of studies around clearly defined, separate questions; less need to cite older and general literature - degree

# similarity of reference lists

library(tm)

# treat a ref like a term in text mining

prep_fun = function(x) {
  x %>% 
    # make text lower case
    str_to_lower %>% 
    # remove non-alphanumeric symbols
    str_replace_all(":|\\\\", "") %>% 
    # collapse multiple spaces
    str_replace_all("\\s+", " ")
}

# make a dtm for the reference list for each article, takes a few min
xx <- 
ref_list7 %>% 
  nest(-id) %>% 
  mutate(refs_only = map(data, ~pull(.x[ , 1]))) %>% 
  mutate(ref_string = map(refs_only, ~str_c(.x, collapse = " "))) %>% 
  mutate(ref_dtm = map(ref_string, ~DocumentTermMatrix(Corpus(VectorSource(prep_fun(.x)))))) %>% 
  mutate(ref_tdm = map(ref_string, ~TermDocumentMatrix(Corpus(VectorSource(prep_fun(.x))))))

# combine the dtm into one big one
Sys.setlocale(category = "LC_ALL", locale = "us")
refs_dtm <- do.call('c', xx$ref_dtm)
refs_tdm <- do.call('c', xx$ref_tdm)

# cosine dist fn
library(slam)

# compute distance of each article from all other articles, by reference list
cosine_dist_mat <- 1 - crossprod_simple_triplet_matrix(refs_tdm)/(sqrt(col_sums(refs_tdm^2) %*% t(col_sums(refs_tdm^2))))

cosine_dist_mat_df <- as_data_frame(cosine_dist_mat)
names(cosine_dist_mat_df) <- 1:ncol(cosine_dist_mat_df)

write_csv(cosine_dist_mat_df,  "cosine_dist_mat.csv")

library("qgraph")
qgraph(cor(cosine_dist_mat))

