message("Extracting data for app...")

load('final_model3_stm.RData')

final_model <- k_first

rm(k_first)

# inspect
labelTopics(final_model, topics = 1:15)

message('Estimating covariate effects...')

# inference
model_infer <- estimateEffect(1:67 ~  month,
                              final_model,
                              meta = meta,
                              uncertainty = 'Global')

message("Extracting document-topic distributions...")

# get topic prop
model_prop_out <- make.dt(final_model, meta = meta) %>% as_tibble()



## check out some top quotes

used_docs_id <- names(docs) %>% as.numeric()

used_docs <- data_cleaning$content[used_docs_id]


doc_prop_df <- data_cleaning %>% 
  rowid_to_column() %>% 
  filter(rowid %in% used_docs_id) %>% 
  select(-rowid) %>% 
  rowid_to_column("docnum") %>% 
  left_join(model_prop_out)

# attach to model props


labelTopics(final_model, 51:67)
findThoughts(final_model, texts = used_docs, n = 25, topics = 61:67)


# create topic table:

topic_table <- tibble(
  
  topic = c(2, 4, 6, 8, 9, 10, 20, 12, 23, 31, 35, 36, 38, 39, 40, 57, 64, 65, 56, 60),
  label = c('Racial Tensions', 'MAGA', "Obama", "News", "Trump Campaign", "Election News", "Fox News",
            'Race Relations', "Bernie", "Food", "US Justice System", "Social Justice",
            "State Corruption", "Political Correctness", "Police Shootings", "Political Scandal", "Police Brutality",
            "Past Heroes", "American Patriotism", "Election Fraud")
  
)

# extract for tableau

output <- labelTopics(final_model)

ordered_time <- 
  data_cleaning %>% 
  select(month) %>% 
  unique() %>% 
  arrange(month) %>% 
  .$month %>% 
  as.character()

factored_ordered_time <- base::factor(ordered_time, levels = ordered_time)

identifiers <-
  expand.grid(topic = 1:67, time = factored_ordered_time) %>% 
  as_tibble() %>% 
  arrange(time, topic)

interactions <-
  output$interaction %>%
  as_tibble() %>% 
  mutate(
    topic = identifiers$topic,
    time = identifiers$time
  ) %>% 
  select(topic, time, everything())

selected_topic_table <-
  interactions %>% 
  inner_join(topic_table, by = "topic") %>% 
  select(label, topic, time, everything()) %>% 
  arrange(topic, time)

# these are the topic-time pairs for top words

final_topics <- selected_topic_table 

# get topic counts
document_topic_post_df <- make.dt(final_model, meta = meta) %>% 
  as_tibble() %>% 
  mutate(time = month) %>% 
  select(-month, -account_category) %>% 
  tidyr::gather('topic', 'prop', -docnum, -time) %>% 
  group_by(docnum) %>% 
  filter(prop == max(prop)) %>% 
  ungroup()

topic_counts <- document_topic_post_df %>% 
  group_by(topic, time) %>% 
  summarise(num_tweets = n()) %>% 
  ungroup() %>% 
  mutate(topic = str_replace(topic, "Topic", ""),
         topic = as.numeric(topic),
         time = as.character(time))


# attach to out output

final_topics <- 
  final_topics %>% 
  left_join(topic_counts) %>% 
  filter(topic != 57)


write.csv(final_topics, "topics-full-data.csv")

message("Extracting top frex words...")

#######

document_topic_post_df <- make.dt(final_model, meta = meta) %>% 
  as_tibble() %>% 
  mutate(time = month) %>% 
  select(-month, -account_category, -time) %>% 
  gather('topic', 'prop', -docnum) %>% 
  group_by(docnum) %>% 
  filter(prop == max(prop)) %>% 
  ungroup()

topic_counts <- document_topic_post_df %>% 
  group_by(topic) %>% 
  summarise(num_tweets = n()) %>% 
  mutate(topic = str_replace(topic, "Topic", ""),
         topic = as.numeric(topic))

## now do the same but group by time as well

agg_topic_df <- output$topics %>% 
  as.data.frame() %>% 
  rowid_to_column("topic") %>% 
  left_join(topic_counts) %>% 
  inner_join(topic_table) %>% 
  select(label, topic, everything()) %>% 
  filter(topic != 57)


write.csv(agg_topic_df, "aggregated_topic_data.csv")

message("Finished.")

