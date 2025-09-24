library(tidyverse)
library(bakeoff)
library(tidytext)
library(ggrepel)

nadiya = filter(episodes, series == 6, baker == 'Nadiya')

ggplot(nadiya, aes(x = episode, y = technical)) + geom_line(group = 1) + 
  geom_text(aes(label = signature))

ggplot(nadiya, aes(x = episode, y = technical)) + geom_line(group = 1) + 
  geom_text(aes(label = str_wrap(signature, width = 30)), size = 3)

ggplot(nadiya, aes(x = episode, y = technical)) + geom_line(group = 1) + 
  geom_text_repel(aes(label = str_wrap(signature, width = 30)), size = 3)

signature = episodes %>% 
  select(series:baker, signature) %>% 
  unnest_tokens(word, signature) %>%
  anti_join(get_stopwords())
signature %>% count(word, series) %>% arrange(desc(n))

sig_series = signature %>% count(word, series)
sig_series %>% bind_tf_idf(word, series, n) %>% arrange(desc(tf_idf))

filter(signature, str_detect(word, "[^a-z']"))
filter(signature, str_detect(word, "[^[:alpha:]]"))

