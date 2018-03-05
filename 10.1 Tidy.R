# Packages for this lesson
install.packages("dplyr")
install.packages("tidytext")
install.packages("stringr")
install.packages("ggplot2")
install.packages("wordcloud")
install.packages("RColorBrewer")
library(ggplot2)
library(wordcloud)
library(RColorBrewer)

# Load in the works of Jane Austen as a package
install.packages("janeaustenr")
library(janeaustenr)

# Tidy text format
text <- c("Because  could not stop for Death -",
          "He kindly sopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")

text

# Create the dataframe
text_df <- data.frame(line=1:4, text=text)
text_df

library(dplyr)

library(tidytext)

text_df <- text %>%
  unnest_tokens(word, text, token="words")

###################################
# Tidying the works of Jane Austen#
###################################
library(janeaustenr)
library(dplyr)
library(stringr)

# Group the dataset by book and mutate to annotate a linenumber
original_books = austen_books() %>%
  group_by(book) %>%
  mutate(linenumber=row_number(),
         chapter=cumsum(str_detect(text, regex("^chapter", ignore_case=T)))) %>%
  ungroup()
original_books

library(tidytext)

# Get dataset into one-token-per-row format
tidy_books = original_books %>%
  unnest_tokens(word, text)

tidy_books

# LOad the list of stop words
data(stop_words)

tidy_books = tidy_books %>%
  anti_join(stop_words)

# Filter the rows of numbers and keep only the words
tidy_books_no_num = tidy_books %>%
  filter(!str_detect(word, "[0-9]"))

tidy_books_no_num

summary(tidy_books)
summary(tidy_books_no_num)

# Use count() to find the most common words in the book
tidy_books_no_num %>%
  count(word, sort=T)

# Create a visualization of the most common words
tidy_books_no_num %>%
  count(word, sort=T) %>%
  filter(n > 600) %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  ggtitle("Frequencies of the words") +
  geom_col() +
  xlab("words") +
  ylab("frequency") +
  coord_flip()

# Word frequencies
library(tidyr)

freq = tidy_books_no_num %>% group_by(book) %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(word, sort=T) %>%
  ungroup() %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>%
  spread(book, proportion) %>%
  gather(book, proportion, 'Sense & Sensibility':'Persuasion')

# Get frequencies for books
pride = freq[freq$book=="Pride & Prejudice",]
sense = freq[freq$book=="Sense & Sensibility",]
books = cbind(pride[,c(1,3)], sense[,3])
names(books)[2:3] = c("Pride & Prejudice", "Sense & Sensibility")

# Now, plot those frequencies
library(ggplot2)
library(scales)

ggplot() +
  geom_point(aes(x=books[,2], y=books[,3], col=2), alpha=0.1) +
  labs(x = "Pride & Prejudice", y = "Sense & Sensibility") +
  scale_x_log10(labels=percent_format()) +
  scale_y_log10(labels=percent_format()) +
  geom_text(aes(label=books[,1], x=books[2], y=books[,3], alpha=0.1), check_overlap=T, vjust=1.5) +
  theme(legend.position="none")

# Modelling
model = lm(books[,2]~books[,3])
summary(model)

ggplot() +
  geom_point(aes(x=books[,2], y=books[,3], col=2), alpha=0.1) +
  labs(x = "Pride & Prejudice", y = "Sense & Sensibility") +
  scale_x_log10(labels=percent_format()) +
  scale_y_log10(labels=percent_format()) +
  geom_text(aes(label=books[,1], x=books[2], y=books[,3], alpha=0.1), check_overlap=T, vjust=1.5) +
  theme(legend.position="none") +
  geom_abline(intercept=mymodel$coefficients[1], slope=mymodel$coefficients[2])

# Test the correlation between the two books
cor.test(books[,2], books[,3])

# Plot the word clouds
library(wordcloud)
library(RColorBrewer)

tidy_books_no_num %>%
  filter(book == "Pride & Prejudice") %>%
  count(word, sort = T) %>%
  with(wordcloud(word, n, max.words=100, color=brewer.pal(8, "Dark2")))
