

# Term frequency of Jane Austen's words
library(dplyr)
library(janeaustenr)
library(tidytext)

book_words = austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort=T) %>%
  ungroup()

total_words = book_words %>%
  group_by(book) %>%
  summarize(total = sum(n))

# All rows from books_words, all columns from both
book_words = left_join(book_words, total_words)

book_words

# Term frequency calculation
tf_book_words = book_words %>%
  mutate(tf=n/total)

tf_book_words

library(ggplot2)

ggplot(tf_book_words, aes(tf, fill=book)) +
  geom_histogram(show.legend=F) +
  xlim(NA, 0.0009) +
  ylim(NA, 4000)   +
  facet_wrap(~book, ncol=3)

# Zipf's law
# cbind; x is the matrix of all features
freq_by_rank = book_words %>%
  group_by(book) %>%
  mutate(rank = row_number(), tf = n/total)

freq_by_rank

# Visualizing Zipf's law
freq_by_rank %>% 
  ggplot(aes(rank, tf, color=book)) +
  geom_line(alpha=0.5, show.legend=T) +
  scale_x_log10() +
  scale_y_log10() 

# Check the rank interval for words 10-1000
rank_subset = freq_by_rank %>%
  filter(rank < 1000,
         rank > 10)
lm(formula=log10(tf)~log10(rank), data=rank_subset)

# Check the linear model line
freq_by_rank %>%
  ggplot(aes(rank, tf, color=book)) +
  geom_abline(intercept=-.57, slope=-1.14, color="gray50", linetype=2) +
  geom_line(alpha=0.5, show.legend=F) +
  scale_x_log10() +
  scale_y_log10()

tf_idf_book_words = book_words %>%
  bind_tf_idf(word, book, n) %>%
  arrange(desc(tf_idf))

tf_idf_book_words

# Visualize
tf_idf_book_words %>%
  arrange(tf_idf) %>%
  mutate(word=factor(word, levels = rev(unique(word)))) %>%
  group_by(book) %>%
  top_n(10) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 3, scales = "free") +
  coord_flip()

# View the least important words according to TFIDF
tf_idf_book_words %>%
  arrange(tf_idf) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(book) %>%
  filter(tf_idf==0)

# Keeping a union of all books vocabulary
library("tidyr")

tf_books = tf_idf_book_words %>%
  select(c(book, word, tf, idf, tf_idf)) %>%
  spread(book, tf, fill=0) %>%
  gather(book, tf, 'Sense & Sensibility':'Persuasion') %>%
  arrange(desc(tf_idf))

tf_books
