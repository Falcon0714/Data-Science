library(readxl)

library(dplyr)

library(stringr)

library(tm)

library(SnowballC)

library(topicmodels)

library(tidytext)

library(ggplot2)

library(wordcloud)

library(tidyr)

df <- read_excel("D:/news_data_data.xlsx", sheet = "Sheet 1")

df_clean <- df %>%
  
  mutate(Content = tolower(Content),
         
         Content = str_replace_all(Content, "[^a-z\\s]", " "),
         
         Content = str_replace_all(Content, "\\s+", " "),
         
         WordCount = str_count(Content, boundary("word"))) %>%
  
  filter(WordCount >= 100)

corpus <- VCorpus(VectorSource(df_clean$Content))

corpus <- tm_map(corpus, content_transformer(tolower))

corpus <- tm_map(corpus, removePunctuation)

corpus <- tm_map(corpus, removeNumbers)

corpus <- tm_map(corpus, removeWords, stopwords("english"))

corpus <- tm_map(corpus, stemDocument)

corpus <- tm_map(corpus, stripWhitespace)

dtm <- DocumentTermMatrix(corpus)

dtm <- removeSparseTerms(dtm, 0.99)

lda_model <- LDA(dtm, k = 5, control = list(seed = 1234))

top_terms <- tidy(lda_model, matrix = "beta") %>%
  
  group_by(topic) %>%
  
  slice_max(beta, n = 10) %>%
  
  ungroup() %>%
  
  arrange(topic, -beta)

df_clean$doc_id <- as.character(1:nrow(df_clean))

doc_topics <- tidy(lda_model, matrix = "gamma")

dominant_topic <- doc_topics %>%
  
  group_by(document) %>%
  
  slice_max(gamma, n = 1) %>%
  
  ungroup()

dominant_topic <- dominant_topic %>%
  
  left_join(df_clean %>% select(doc_id, Title), by = c("document" = "doc_id"))

topic_titles <- dominant_topic %>%
  
  group_by(topic) %>%
  
  summarise(Representative_Title = str_trunc(Title[1], 50))

top_terms <- top_terms %>%
  
  left_join(topic_titles, by = "topic") %>%
  
  mutate(topic = Representative_Title)

top_terms %>%
  
  mutate(term = reorder_within(term, beta, topic)) %>%
  
  ggplot(aes(term, beta, fill = topic)) +
  
  geom_col(show.legend = FALSE) +
  
  facet_wrap(~ topic, scales = "free") +
  
  coord_flip() +
  
  scale_x_reordered() +
  
  labs(title = "Top Terms per Topic", x = NULL, y = "Beta")

for (i in unique(top_terms$topic)) {
  
  topic_words <- filter(top_terms, topic == i)
  
  print(paste("Topic:", i))
  
  wordcloud(words = topic_words$term,
            
            freq = topic_words$beta,
            
            max.words = 30,
            
            colors = brewer.pal(8, "Dark2"),
            
            random.order = FALSE)
  
}

