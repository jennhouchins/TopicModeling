# ASSIGNMENT DESCRIPTION #####################################
# File:         Wrangling.R
# Project:      Topic Modeling Independent Analysis
# Author:       Jennifer Houchins
#
# Purpose:      Use R for topic modeling of Academic Chatter Tweets
#


# 1 PROJECT SETUP  ################################
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, tidyverse, tidytext, topicmodels, readxl,
               stm, LDAvis, SnowballC, ldatuning,knitr)

# Import Data #########
datafilePath <- "data/academicmentalhealth_tweets_v2.xlsx"
mh_tweets <- read_xlsx(datafilePath) %>% 
  select(status_id, screen_name, text, favorite_count, retweet_count)

# DOCUMENT TERM MATRIX ##############
custom_stop <- data.frame("word" = c("academictwitter", 
                                     "academicchatter", 
                                     "phdchat", 
                                     "mentalhealth", 
                                     "phdlife",
                                     "academicmentalhealth",
                                     "phdvoice",
                                     "sciencetwitter",
                                     "https",
                                     "t.co",
                                     "mentalhealthmatters",
                                     "fyue6ouiiz",
                                     "openacademics",
                                     "efj1zy9z6p",
                                     "qwiw6ynsyu",
                                     "fvkwrihsus"))
tweets_tidy <- mh_tweets %>%
  unnest_tokens(output = word, input = text) %>%
  anti_join(stop_words, by = "word") %>% 
  anti_join(custom_stop, by = "word") # the custom stop words were used 1700 times?!!

tweets_tidy

# WORD COUNT
tweets_counts <- tweets_tidy %>%
  count(word, sort = TRUE)

tweets_dtm <- tweets_tidy %>%
  count(status_id, word) %>%
  cast_dtm(status_id, word, n)

?textProcessor
temp <- textProcessor(mh_tweets$text, 
                      metadata = mh_tweets,  
                      lowercase=TRUE, 
                      removestopwords=TRUE, 
                      removenumbers=TRUE,  
                      removepunctuation=TRUE, 
                      wordLengths=c(3,Inf),
                      stem=FALSE,
                      onlycharacter= FALSE, 
                      striphtml=TRUE, 
                      customstopwords=c("academictwitter", 
                                        "academicchatter", 
                                        "phdchat", 
                                        "mentalhealth", 
                                        "phdlife",
                                        "academicmentalhealth",
                                        "phdvoice",
                                        "sciencetwitter",
                                        "https",
                                        "t.co",
                                        "mentalhealthmatters",
                                        "fyue6ouiiz",
                                        "openacademics",
                                        "efj1zy9z6p",
                                        "qwiw6ynsyu",
                                        "fvkwrihsus"))

meta <- temp$meta
vocab <- temp$vocab
docs <- temp$documents

stemmed_tweets <- mh_tweets %>%
  unnest_tokens(output = word, input = text) %>%
  anti_join(stop_words, by = "word") %>%
  anti_join(custom_stop, by = "word") %>% 
  mutate(stem = wordStem(word))

stemmed_tweets

stemmed_dtm <- mh_tweets %>%
  unnest_tokens(output = word, input = text) %>%
  anti_join(stop_words, by = "word") %>%
  anti_join(custom_stop, by = "word") %>%
  mutate(stem = wordStem(word)) %>%
  count(status_id, stem) %>%
  cast_dtm(status_id, stem, n)

stemmed_dtm

# MODEL ###############################

# n_distinct(ts_forum_data2$forum_name)

tweets_lda <- LDA(tweets_dtm, 
                  k = 5, 
                  control = list(seed = 588) # a special see for a special class :)
)

tweets_lda

# stm Package
?stm
tweets_stm <- stm(documents=docs, 
                  data=meta,
                  vocab=vocab, 
                  # prevalence =~ screen_name,
                  K=5,
                  max.em.its=25,
                  verbose = FALSE)

tweets_stm

plot.STM(tweets_stm, n = 5)

# FINDING K #############################

k_metrics <- FindTopicsNumber(
  tweets_dtm,
  topics = seq(10, 75, by = 5),
  metrics = "Griffiths2004",
  method = "Gibbs",
  control = list(),
  mc.cores = NA,
  return_models = FALSE,
  verbose = TRUE,
  libpath = NULL
)

FindTopicsNumber_plot(k_metrics)

findingk <- searchK(docs, 
                    vocab, 
                    K = c(5:15),
                    data = meta, 
                    verbose=TRUE)

plot(findingk)

# THE LDAVIS EXPLORER ############################

toLDAvis(mod = tweets_stm, docs = docs)

# EXPLORE ########################################

terms(tweets_lda, 5)

tidy_lda <- tidy(tweets_lda)

tidy_lda

top_terms <- tidy_lda %>%
  group_by(topic) %>%
  slice_max(beta, n = 5, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  group_by(topic, term) %>%    
  arrange(desc(beta)) %>%  
  ungroup() %>%
  ggplot(aes(beta, term, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  labs(title = "Top 5 terms in each LDA topic",
       x = expression(beta), y = NULL) +
  facet_wrap(~ topic, ncol = 5, scales = "free")

# Exploring Gamma Values

td_beta <- tidy(tweets_lda)

td_gamma <- tidy(tweets_lda, matrix = "gamma")

td_beta

td_gamma

top_terms <- td_beta %>%
  arrange(beta) %>%
  group_by(topic) %>%
  top_n(7, beta) %>%
  arrange(-beta) %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest()

gamma_terms <- td_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

gamma_terms %>%
  select(topic, gamma, terms) %>%
  kable(digits = 3, 
        col.names = c("Topic", "Expected topic proportion", "Top 7 terms"))

plot(tweets_stm, n = 7)

# READING TEA LEAVES #########################

ts_tweet_data_reduced <-mh_tweets$text[-temp$docs.removed]

findThoughts(tweets_stm,
             texts = ts_tweet_data_reduced,
             topics = 2, 
             n = 10,
             thresh = 0.5)

thoughts <- findThoughts(tweets_stm,
             texts = ts_tweet_data_reduced,
             topics = 5, 
             n = 10,
             thresh = 0.5)


