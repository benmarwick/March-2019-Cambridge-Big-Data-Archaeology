# read in the data, Jan 2012 data for comparison with Fanelli?
wos_files <- dir(here("data", "raw-data"), full.names = TRUE)

text <- map(wos_files, ~read_file(.x, locale = locale(encoding = "latin1")))

# split on article delimiter provided by WOS
library(stringr)
items <- unlist(str_split(text, pattern = "\nPT J\n"))
# get rid of the advertising
items <- str_replace_all(items, "FN Clarivate Analytics Web of Science\nVR 1.0", "")
items <- items[items != ""] 

#  length(items) # each item is one article

# function to automate getting the variables out of each item
extractor <- function(i){
  
  # debug with 
  # i <- items[[30]]
  
  authors =     gsub(".*AU *(.*?) *\nAF .*", "\\1", i)
  first_author = gsub(".*AF *(.*?) *\nTI .*", "\\1", i)
  authors_n =   str_count(authors, "\n") + 1
  title =       gsub(".*\nTI *(.*?) *\nSO .*", "\\1", i)
  title_n =     str_count(title, " ") - 1
  journal =     gsub(".*\nSO *(.*?) *\nLA .*", "\\1", i)
  abstract =    gsub(".*\nAB *(.*?) *\nC1 .*", "\\1", i)
  refs =        gsub(".*\nCR *(.*?) *\nNR .*", "\\1", i)
  refs_n =      as.numeric(gsub(".*\nNR *(.*?) *\nTC .*", "\\1", i))
  pages_n =     as.numeric(gsub(".*\nPG *(.*?) *\nWC .*", "\\1", i))
  year =        as.numeric(gsub(".*\nPY *(.*?) *\nVL .*", "\\1", i))
  doi =         gsub(".*\nDI *(.*?) *\nPG .*", "\\1", i)
  
  dplyr::data_frame(
    authors =         authors,
    first_author =    first_author,
    authors_n =       authors_n,
    title =           title ,
    title_n =         title_n,
    journal =         journal,
    abstract  =       abstract,
    refs =            refs    ,
    refs_n =          refs_n ,
    pages_n =         pages_n,
    year =            year, 
    doi =             doi
  )
}


# # for debugging, to find the items that break the fn
# for(i in seq_len(length(items))){
#   extractor(items[i])
#   print(i)
# }


# this will take a few mins
items_df <- map_df(items, ~extractor(.x))

saveRDS(items_df, here("data", "derived-data", "wos-data-df.rds"))
