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

ggplot(crime_sentiment, aes(index, sentiment)) +
  geom_col(show.legend = FALSE) + theme_bw() + 
  ggtitle("Sentiment of Crime and Punishment")


####Sentence sentimentr
sentences <- get_sentences(text)
sentiments <- sentiment(sentences)
summary(sentiments)



tnum.authorize("mssp1.bu.edu")
tnum.setSpace("test2")

crime_punishment <- gutenberg_download(2554)
crime_punishment <- crime_punishment[-c(4:97),]

source("Book2TN.R")
tnBooksFromLines(crime_punishment$text[c(1:5000)], "dostoevsky/Crime_and_Punishment/Pt1")
tnBooksFromLines(crime_punishment$text[c(5001:10000)], "dostoevsky/Crime_and_Punishment/Pt2")
tnBooksFromLines(crime_punishment$text[c(10001:15000)], "dostoevsky/Crime_and_Punishment/Pt3")
tnBooksFromLines(crime_punishment$text[c(15001:21967)], "dostoevsky/Crime_and_Punishment/Pt4")

# tnBooksFromLines <- function(theBook, subjectRoot){
#   sentence <- "" #accumulates a sentence across line
#   sentencenum <- 0 #sentence number within paragraph
#   ordinal <- 0 #sequence number in the stream of headers
#   headingnum <- 0 #last heading line number
#   
#   bklen <- length(theBook)
#   tn <- list()
#   for(j in 1:bklen){
#     line <- theBook[j]
#     
#     if (line == "") { #blank line
#       #new section
#       sentence <- ""
#       sentencenum <- 1
#     } else {
#         #non=blank line
#       if (j == 1 || 
#           ((j < bklen-2 && j > 2) #two blank lines ))
#         
#       }
#   }
# }
# tnBooksFromLines(crime$text)