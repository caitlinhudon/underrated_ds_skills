library(tidyverse)
library(rtweet)
library(ggraph)
library(igraph)
library(ggiraph)
library(tidytext)
library(wordcloud2)

tweets <- search_tweets(q = "@beeonaposy OR to:beeonaposy OR beeonaposy",
                        sinceId = 958062360811405313,
                        n = 500,
                        include_rts = FALSE)

tweets <- tweets %>%
  distinct()

########################## data prep #########################################

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

write.csv(all_replies, "all_replies.csv")

from_text <- all_replies %>%
  select(in_reply_to_status_status_id) %>%
  left_join(all_replies, c("in_reply_to_status_status_id" = "status_id")) %>%
  select(screen_name, text)

tweet_0 <- "@beeonaposy: Data scientists: what is the most underrated / undervalued skill for a new data scientist?"

to_text <- paste0(all_replies$screen_name, ": ", all_replies$text)
to_text <- gsub("'", "`", to_text)
from_text <- paste0(from_text$screen_name, ": ", from_text$text)
from_text <- gsub("'", "`", from_text)

edges <- tibble::tibble(
  from = from_text,
  to = to_text
) %>%
  mutate(from = ifelse(
    from == "NA: NA",
    tweet_0,
    from)
  )

########################## co-occurs ########################################

library(igraph)
library(ggraph)

set.seed(1813)

'star', 'circle', 'gem', 'dh', 'graphopt', 'grid', 'mds',
'randomly', 'fr', 'kk', 'drl', 'lgl'


word_pairs %>%
  filter(n >= 3) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "plum4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, repel = T) +
  ggtitle(expression("\nWord Network of Twitter Responses")) +
  theme_void()





########################## twitter tree ########################################

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

saveWidget(frameableWidget(q), 'output_frameable.html')

############################ word cloud #######################################

agg <- words %>%
  group_by(word) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

agg <- words %>%
  count(word)

agg %>%
  filter(n > 3) %>%
  wordcloud2(size = 3, minRotation = -pi/2, maxRotation = -pi/2)

