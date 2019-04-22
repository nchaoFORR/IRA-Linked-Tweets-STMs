message("Preprocessing Data...")

require(tidyverse)
require(stm)
require(quanteda)
require(here)
require(lubridate)
require(tidytext)
require(tictoc)

########################## Read data and sample

# lemmatizer function
token_lemmatizer <- function(tokens) {
  
  dict <- lexicon::hash_lemmas
  
  from <- dict$tokens
  to <- dict$lemma
  
  type <- attr(tokens, 'types')
  type_new <- to[match(type, from)]
  type_new <- ifelse(is.na(type_new), type, type_new)
  attr(tokens, 'types') <- type_new
  quanteda:::tokens_recompile(tokens)
  
}

# internet word replacement function
token_internetizer <- function(tokens) {
  
  dict <- lexicon::hash_internet_slang
  
  from <- dict$x
  to <- dict$y
  
  type <- attr(tokens, 'types')
  type_new <- to[match(type, from)]
  type_new <- ifelse(is.na(type_new), type, type_new)
  attr(tokens, 'types') <- type_new
  quanteda:::tokens_recompile(tokens)
  
}



raw_data <- read_rds('full-data.rds')
#### Read Full Data

# english_data <- raw_data %>% 
#   filter(language == "English")

english_data <- raw_data

########################################## Prep data for stm functions

### Clean up data

data_cleaning <- english_data %>% 
  select(content, publish_date, post_type, account_category) %>% 
  mutate(date = str_replace(publish_date, " .*", ""),
         date = mdy(date),
         month = round_date(date, unit = "month")) %>% 
  filter(date >= "2016-01-01" & date <= '2017-01-01') %>% 
  mutate(post_type = ifelse(is.na(post_type), 0, 1)) %>% 
  select(content, month, account_category, post_type) %>% 
  filter(post_type == 1) %>% 
  mutate(content = str_replace(content, "&amp", ""),
         content = str_replace(content, "(RT|via)((?:\\b\\W*@\\w+)+)", ""),
         content = str_replace(content, "@\\w+", ""),
         # content = str_replace(content, "[[:punct:]]", ""),
         content = str_replace(content, "[[:digit:]]", ""),
         content = str_replace(content, "http\\w+", ""),
         content = str_replace(content, "[ \t]{2,}", ""),
         content = str_replace(content, "^\\s+|\\s+$", ""),
         content = str_replace_all(content," "," "),
         # Get rid of URLs -- this one's causing a regex error...
         #content = str_replace_all(content, "http:/,/t.co/[a-z,A-Z,0-9]*{8}",""),
         # Take out retweet header, there is only one
         content = str_replace_all(content,"RT @[a-z,A-Z]*: ",""),
         # Get rid of hashtags
         content = str_replace_all(content,"#[a-z,A-Z]*",""),
         # Get rid of references to other screennames
         content = str_replace_all(content,"@[a-z,A-Z]*",""),
         # Get rid of weird links and retweet shit
         content = str_replace_all(content, "t\\.[^ ]* ", ""),
         content = str_replace_all(content, "t\\.[^ ]*$", ""),
         content = str_replace_all(content, "\\:\\/\\/", ""),
         # Get rid of all non-alphanumeric
         content = str_replace_all(content, "[^a-zA-Z0-9 -]", "")) %>% 
  filter(content != " ") %>% 
  select(-post_type) %>% 
  distinct(content, .keep_all = TRUE) # mad duplicates in the data


data_cleaning$tweet_id <- 1:nrow(data_cleaning)

# Grab corpus

quant_corpus <- corpus(x = data_cleaning,
                       docid_field = "tweet_id",
                       text_field = "content")


# Grab tokens

tokens <- tokens(quant_corpus)

# Clean
custom_stopwords <- c("t.co", "https", "rt", "http", "amp", "1kpxto2hfw", "http",
                      "httphttp", 'w', 's', "hashtag", "payday")

tokens_clean <- tokens %>%
  tokens(remove_punct = TRUE,
         remove_symbols = TRUE,
         remove_numbers = TRUE,
         remove_twitter = TRUE,
         remove_url = TRUE) %>% 
  tokens_tolower() %>% 
  tokens_select(stopwords('english'), selection = 'remove') %>% 
  tokens_select(custom_stopwords, selection = 'remove') %>% 
  token_internetizer() %>% 
  token_lemmatizer() %>% 
  tokens_ngrams(n = 1:3)


# Cast dfm
quant_dfm <- dfm(tokens_clean)

trimmed_dfm <- dfm_trim(quant_dfm, min_docfreq = 15, max_docfreq = 0.2*nrow(data_cleaning), docfreq_type = "count")

# convert to stm  object

stm_mat <- convert(trimmed_dfm, to = "stm", docvars = docvars(quant_corpus))

### Run through stm 

docs <- stm_mat$documents
vocab <- stm_mat$vocab
meta <- stm_mat$meta

##########################################
message("Data is ready for modeling!")
