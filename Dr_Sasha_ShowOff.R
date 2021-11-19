#CSC Download and clean
df <- read.delim("CSC Final Report.txt")


library(tm)

df_source <- VectorSource(df)
df_corpus <- VCorpus(df_source)
#df_tidy <- tidy(df_corpus)

clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  return(corpus)
}
# Apply your customized function to the tweet_corp: clean_corp
clean_corp <- clean_corpus(df_corpus)

#all_stops <- c("united states", "president","congress","senate","technology","cyber","cybersecurity","defense","department", stopwords("en"))
#df <- removeWords(df, stopwords("en"))

df_dtm <- DocumentTermMatrix(clean_corp)


library(dplyr)
library(tidytext)

df_tibble <- tidy(df_dtm)

ap_sentiments <- df_tibble %>%
  inner_join(get_sentiments("bing"), by = c(term = "word"))

ap_sentiments

#sentiment analysis WORKS!

library(ggplot2)

ap_sentiments %>%
  count(sentiment, term, wt = count) %>%
  filter(n >= 10) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(term, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Contribution to sentiment")

#word count
full_word_count <- df_tibble %>%
  group_by(term) %>%
  unnest_tokens(word, count) %>%
  summarise(num_words = n()) %>%
  arrange(desc(num_words))

full_word_count
plot(full_word_count)


#top 10 
df_tibble %>%
  count(count, sort = TRUE) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(count = reorder(count, term)) %>%
  ggplot() +
  geom_col(aes(count, n)) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank()) +
  xlab("") + 
  ylab("Song Count") +
  ggtitle("Most Frequently Used Words in Prince Lyrics") +
  coord_flip()

#new tibble for word freq

new_df <- arrange(df_tibble, desc(count))


#TF-IDF
library(forcats)

new_df

df_tf_idf <- new_df %>%
  bind_tf_idf(term, document, count)


#Word Freq Graph works!
df_tf_idf %>%
  group_by(document[1]) %>%
  slice_max(tf_idf, n = 30) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, reorder(term, tf_idf), fill = document)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~document, ncol = 1, scales = "free") +
  labs(x = "tf-idf", y = NULL)

library(tidyverse)
edge_list <- tibble(from = c(1, 2, 2, 3, 4), to = c(2, 3, 4, 2, 1))
node_list <- tibble(id = 1:4)
