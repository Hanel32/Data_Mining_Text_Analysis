

# Libraries
library(tidytext)
sentiments

# Three general purpose lexicons
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

# Sentiment analysis with inner join
library(janeaustenr)
library(dplyr)
library(stringr)

tidy_books = austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                               ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

nrcjoy = get_sentiments("nrc") %>%
  filter(sentiment=="joy")

tidy_books %>%
  filter(book == "Emma") %>%
  
