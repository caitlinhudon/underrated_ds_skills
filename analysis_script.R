library(tidyverse)
library(rtweet)
library(ggraph)
library(igraph)
library(ggiraph)
library(tidytext)
library(wordcloud2)
library(widyr)

tweets <- search_tweets(q = "@beeonaposy OR to:beeonaposy OR beeonaposy",
                          sinceId = 958062360811405313,
                          n = 500,
                          include_rts = FALSE)
tweets <- tweets %>%
  distinct()

id <- c("958062360811405313", "958062683118624768")
diff <- 1
while (diff != 0) {
  id_next <- tweets %>%
    filter(in_reply_to_status_status_id %in% id) %>%
    pull(status_id)
  id_new <- unique(c(id, id_next))
  diff <- length(id_new) - length(id)
  id <- id_new
}

all_replies <- tweets %>%
  filter(in_reply_to_status_status_id %in% id)

################################################################################

drop_pattern <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
#unnest_pattern <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"

handle_list <- c("drewconway", "joelgrus", "timclicks", "aaronslowey",
                 "becomingdatasci", "timsteeno")
numbers <- c(1, 2, 3, 4, 5)

words <- all_replies %>%
  mutate(text = stringr::str_replace_all(text, drop_pattern, "")) %>%
  unnest_tokens(word,
                text,
                token = "ngrams",
                n = 1) %>%
  anti_join(stop_words) %>%
  filter(!(word %in% screen_name),
         !(word %in% handle_list),
         !(word %in% numbers))

# use a widyr function to count when words co-occur
# to remove duplicates, use upper = FALSE
word_pairs <- words %>%
  pairwise_count(word, status_id, upper = FALSE)

pairs <- word_pairs %>%
  mutate(token = paste(item1, item2)) %>%
  select(token, n)

agg <- words %>%
  rename(token = word) %>%
  count(token, sort = TRUE)

combined <- rbind(agg, pairs) %>%
  arrange(desc(n))

bigrams <- all_replies %>%
  mutate(text = stringr::str_replace_all(text, drop_pattern, "")) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

test <- grepl(paste(stop_words$word,collapse="|"),
               bigrams$bigram)

test <- subset(bigrams, grepl(paste(stop_words$word,collapse="|"),
                      bigrams$bigram))


bi_agg <- bigrams %>%
  group_by(bigram) %>%
  summarise(n = n()) %>%
  filter(!(bigram ))
  arrange(desc(n))

trigrams <- all_replies %>%
  mutate(text = stringr::str_replace_all(text, drop_pattern, "")) %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3)

tri_agg <- trigrams %>%
  group_by(trigram) %>%
  summarise(n = n()) %>%
  arrange(desc(n))
