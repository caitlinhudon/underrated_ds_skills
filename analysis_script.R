library(tidyverse)
library(rtweet)
library(ggraph)
library(igraph)
library(ggiraph)
library(tidytext)
library(wordcloud2)

tweets <- rtweet::search_tweets(q = "@beeonaposy OR to:beeonaposy OR beeonaposy",
                                sinceId = 958062360811405313,
                                n = 500,
                                include_rts = FALSE)

tweets <- tweets %>%
  distinct()

# from lucy's blog

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

from_text <- all_replies %>%
  select(in_reply_to_status_status_id) %>%
  left_join(all_replies, c("in_reply_to_status_status_id" = "status_id")) %>%
  select(screen_name, text)

tweet_0 <- "@beeonaposy: Data scientists: what is the most underrated / undervalued skill for a new data scientist?"

to_text <- paste0(all_replies$screen_name, ": ", all_replies$text)
to_text <- gsub("'", "`", to_text)
from_text <- paste0(from_text$screen_name, ": ", from_text$text)
from_text <- gsub("'", "`", from_text)

###################################################################

edges <- tibble::tibble(
  from = from_text,
  to = to_text
) %>%
  mutate(from = ifelse(
    from == "NA: NA",
    tweet_0,
    from)
  )

graph <- graph_from_data_frame(edges, directed = TRUE)
V(graph)$tooltip <- V(graph)$name

set.seed(525)
p <- ggraph(graph, layout = "nicely") +
  geom_edge_link() +
  #geom_point(aes(x, y, color = "red", alpha = 0.05)) +
  geom_point_interactive(aes(x, y, color = "red", alpha = 0.05, tooltip = tooltip)) +
  theme_void() +
  theme(legend.position = "none")

q = ggiraph(code = print(p),
            width_svg = 10,
            zoom_max = 4)

#htmlwidgets::saveWidget(q, "output.html")

################################################################################

drop_pattern <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https|ht"
#unnest_pattern <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"

handle_list <- c("drewconway", "joelgrus", "timclicks", "aaronslowey",
                 "becomingdatasci")

words <- all_replies %>%
  mutate(text = stringr::str_replace_all(text, drop_pattern, "")) %>%
  unnest_tokens(word,
                text,
                token = "ngrams",
                n = 1) %>%
  anti_join(stop_words) %>%
  filter(!(word %in% screen_name),
         !(word %in% handle_list))

agg <- words %>%
  group_by(word) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

bigrams <- all_replies %>%
  mutate(text = stringr::str_replace_all(text, drop_pattern, "")) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

bi_agg <- bigrams %>%
  group_by(bigram) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

trigrams <- all_replies %>%
  mutate(text = stringr::str_replace_all(text, drop_pattern, "")) %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3)

tri_agg <- trigrams %>%
  group_by(trigram) %>%
  summarise(n = n()) %>%
  arrange(desc(n))


######### viz

agg %>%
  filter(n > 2) %>%
  wordcloud2(size = 3, minRotation = -pi/2, maxRotation = -pi/2)
