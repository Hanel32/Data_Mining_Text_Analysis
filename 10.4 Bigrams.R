
# Text mining with R
# Part 4
# Relationships between words: n-grams and correlations

# Tokenizing by n-gram
library(dplyr)
library(tidytext)
library(janeaustenr)

austen_bigrams = austen_books() %>%
  unnest_tokens(bigram, text, token="ngrams", n=2)

austen_bigrams

# Counting and filtering n-grams
austen_bigrams %>%
  count(bigram, sort=T)

library(tidyr)

bigrams_separated = austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_separated

# Filter stop words from the bigrams
bigrams_filtered = bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigrams_filtered

# Count the remaining bigrams
bigram_counts = bigrams_filtered %>%
  count(word1, word2, sort=TRUE)

bigram_counts

# Tidy's "unite" is the opposite of "separate"
# Find the common bigrams not containing stop words
bigrams_united = bigrams_filtered %>%
  unite(bigram, word1, word2, sep=" ")

bigrams_unites

# Derive trigrams from Austen's works
austen_books() %>%
  unnest_tokens(trigram, text, token = "ngrams", n=3) %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%
  count(word1, word2, word3, sort=T)

# Analyzing bigrams w/ second word == "street"
bigrams_filtered %>%
  filter(word2 == "street") %>%
  count(book, word1, word2, sort=T)

# TF-IDF
bigram_tf_idf = bigrams_united %>%
  count(book, bigram) %>%
  bind_tf_idf(bigram, book, n) %>%
  count(book, word1, word2, sort=T)

# Visualize bigram TF-IDF
library(ggplot2)

bigram_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  mutate(bigram = factor(bigram, levels = rev(unique(bigram)))) %>%
  group_by(book) %>%
  top_n(10) %>%
  ungroup %>%
  ggplot(aes(bigram, tf_idf, fill = book)) +
  geom_col(show.legend=F) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 3, scales = "free") +
  coord_flip()

# Remember, TF-IDF values equivalent to 0 are smoothed.
bigram_tf_idf %>%
  filter(tf_idf==0)

# See how often words are negated by "not" in bigrams
bigrams_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort=T)

# Use AFINN lexicon for sentiment analysis
AFINN = get_sentiments("afinn")

not_words = bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, score, sort = TRUE) %>%
  ungroup()

not_words

# Figure out which words contributed too heavily in the direction
# opposite of the true bigram sentiment
not_words %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2=reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * score, fill = n * score > 0)) +
  geom_col(show.legend=F) +
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment score * number of occurrences") +
  coord_flip()

negation_words = c("not", "no", "never", "without")

negated_words = bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, score, sort = T) %>%
  ungroup()

# Now, plot the most miss positive and miss negative words for each book
positive = negated_words %>%
  mutate(contribution = n * score) %>%
  mutate(word2=reorder(word2, contribution)) %>%
  arrange(desc(abs(contribution))) %>%
  group_by(word1) %>%
  top_n(4) %>%
  ungroup()


  



#################
# Homework notes#
#################