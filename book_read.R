library(tidyverse)
library(reshape2)
library(tidytext)
library(sentimentr)
library(textdata)
library(tnum)
library(gutenbergr)

#read in crime and punishment
text <- read_lines("crimeandpun.txt")
text <- text[-c(1:123)]
text <- text[-c(21970:22320)]

###Bag of words
crime <- tibble(line = 1:21969, text = text)
crime <- crime %>%
  unnest_tokens(word,text)
get_sentiments("nrc")


crime_sentiment <- crime %>%
  inner_join(get_sentiments("bing")) %>%
  count(index = line %/% 120, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

sentiment_graph <- ggplot(crime_sentiment, aes(index, sentiment)) +
  geom_col(show.legend = FALSE, aes(fill = "red")) + theme_bw() + 
  ggtitle("Sentiment of Crime and Punishment") + xlab("Index (by 120 lines")


####Sentence sentimentr
sentences <- get_sentences(text)
sentiments <- sentiment(sentences)
summary(sentiments)

library(reshape2)
library(wordcloud)

word_cloud <- crime %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = brewer.pal(7, "Set1"),
                   max.words = 100)

tnum.authorize("mssp1.bu.edu")
tnum.setSpace("test3")
#tnum.getDBPathList(levels = 2)

#crime_punishment <- gutenberg_download(2554)
#crime_punishment <- crime_punishment[-c(4:97),]

source("Book2TN.R")
#tnBooksFromLines(crime_punishment$text[c(1:5000)], "dostoevsky/Crime_and_Punishment/Pt1")
#tnBooksFromLines(crime_punishment$text[c(5001:10000)], "dostoevsky/Crime_and_Punishment/Pt2")
#tnBooksFromLines(crime_punishment$text[c(10001:15000)], "dostoevsky/Crime_and_Punishment/Pt3")
#tnBooksFromLines(crime_punishment$text[c(15001:21967)], "dostoevsky/Crime_and_Punishment/Pt4")

q1 <-tnum.query("dostoevsky/crime_and_punishment# has text", max = 3798)
d1 <- tnum.objectsToDf(q1)
para1 <- d1 %>% pull(string.value) %>% str_replace_all("\"", "") %>%
  str_flatten(collapse = " ")
sentence1 <- get_sentences(para1)
sentiment(sentence1)
max(sentiment(sentence1)$sentiment)
min(sentiment(sentence1)$sentiment)

bingnegative <- get_sentiments("bing") %>% 
  filter(sentiment == "negative")

wordcounts <- crime %>%
  summarize(words = n())

negative_words <- crime %>%
  inner_join(bingnegative) %>%
  count(word)
negative_words %>% arrange(desc(n))

sentence_polarity <- sentence1 %>% sentiment() %>% 
  mutate(polarity_level = ifelse(sentiment < -0.2, "Negative", ( 
    ifelse(sentiment > 0.2, "Positive", "Neutral"))))
