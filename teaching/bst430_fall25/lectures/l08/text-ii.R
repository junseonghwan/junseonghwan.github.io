## ----child = "setup.Rmd"--------------------

## ----setup, include=FALSE-------------------
# R options
options(
  htmltools.dir.version = FALSE, # for blogdown
  show.signif.stars = FALSE,     # for regression output
  warn = 1,
  pillar.print_max = 8,
  pillar.print_min = 4,
  pillar.max_footer_lines = 3,
  pillar.width = 65,
  width = 65)
 
# ggplot2 color palette with gray
color_palette <- list(gray = "#999999", 
                      salmon = "#E69F00", 
                      lightblue = "#56B4E9", 
                      green = "#009E73", 
                      yellow = "#F0E442", 
                      darkblue = "#0072B2", 
                      red = "#D55E00", 
                      purple = "#CC79A7")
# For nonsese...
htmltools::tagList(rmarkdown::html_dependency_font_awesome())
# For magick
dev.off <- function(){
  invisible(grDevices::dev.off())
}

# figure height, width, dpi
knitr::opts_chunk$set(echo = TRUE, 
                      fig.width = 8, 
                      fig.asp = 0.618,
                      out.width = "60%",
                      fig.align = "center",
                      dpi = 300,
                      message = FALSE)
# ggplot2
ggplot2::theme_set(ggplot2::theme_gray(base_size = 16))
# set seed
set.seed(1234)
# fontawesome
htmltools::tagList(rmarkdown::html_dependency_font_awesome())
# magick
dev.off <- function(){
  invisible(grDevices::dev.off())
}
# conflicted
library(conflicted)
conflict_prefer("filter", "dplyr")
# xaringanExtra
library(xaringanExtra)
xaringanExtra::use_panelset()
# output number of lines
hook_output <- knitr::knit_hooks$get("output")
knitr::knit_hooks$set(output = function(x, options) {
  lines <- options$output.lines
  if (is.null(lines)) {
    return(hook_output(x, options))  # pass to default hook
  }
  x <- unlist(strsplit(x, "\n"))
  more <- "..."
  if (length(lines)==1) {        # first n lines
    if (length(x) > lines) {
      # truncate the output, but add ....
      x <- c(head(x, lines), more)
    }
  } else {
    x <- c(more, x[lines], more)
  }
  # paste these lines together
  x <- paste(c(x, ""), collapse = "\n")
  hook_output(x, options)
})



## ---- include = FALSE-----------------------
library(tidyverse)
options(
  htmltools.dir.version = FALSE, # for blogdown
  show.signif.stars = FALSE,     # for regression output
  warn = 1,
  pillar.print_max = 12,
  pillar.print_min = 8,
  pillar.max_footer_lines = 3,
  pillar.width = 65,
  width = 65)
knitr::opts_chunk$set(cache = TRUE)


## -------------------------------------------
library(tidytext); library(stopwords)
#to use gutenbergr currently, you need to download it from github
#if you can do that, you can install it with this line
#devtools::install_github("ropensci/gutenbergr")
book_names = tibble(gutenberg_id = c(158, 1342, 5200, 7849),
                    title = c('Emma', 'Pride and prejudice',
                              'Metamorphosis', 'The Trial'))
books = gutenbergr::gutenberg_download(book_names$gutenberg_id) %>% left_join(book_names)
#If you can't install this library, instead load the data in books
books = read.csv("lecture/l10/data/books.csv")


## ---- output.lines = 24---------------------
books %>% group_by(title) %>% slice_head(n=6)


## -------------------------------------------
book_words = unnest_tokens(books, text, output = 'word', drop = TRUE)
book_words


## -------------------------------------------
word_counts = book_words %>%
  group_by(title) %>% count(title, word) %>%
  arrange(desc(n))
word_counts %>% slice_head(n = 3)


## -------------------------------------------
word_counts %>% anti_join(get_stopwords()) %>% slice_head(n = 3)


## -------------------------------------------
total_words = word_counts %>%
  group_by(title) %>%
  summarize(total = sum(n))
word_counts = left_join(word_counts, total_words)
word_counts


## ----plottf, warning = FALSE----------------
ggplot(word_counts, aes(n/total)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~title, ncol = 2, scales = "free_y") + theme_minimal()


## ----freq_by_rank, fig.show='hide'----------
freq_by_rank = word_counts %>%
  group_by(title) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total) %>%
  ungroup()

freq_by_rank %>%
  ggplot(aes(x = rank, y = `term frequency`, color = title)) +
  geom_abline(intercept = -0.62, slope = -1,
              color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  theme_minimal()


## ----ref.label = 'freq_by_rank', echo = FALSE----


## -------------------------------------------
#these come from the tidytext library
sentiments


## -------------------------------------------
word_sentiments = word_counts %>%
  left_join(sentiments) %>% #<<
  filter(!is.na(sentiment)) %>%
  group_by(title) %>%
  mutate(word_collapse = fct_lump_n(word, n = 10),
    word_collapse = fct_reorder(word_collapse, n, sum)) %>%
    select(title, word_collapse, sentiment, n)
word_sentiments


## -------------------------------------------
ggplot(word_sentiments, aes(y = fct_reorder(word_collapse,  n, .fun = sum), x = n, fill = sentiment)) + geom_col() + facet_wrap(~title, scales = 'free_x') + ylab("Word") + xlab("Occurrence") + theme_minimal()


## ----calc-tf, output.lines = 12-------------
word_counts = word_counts %>% bind_tf_idf(word, title, n)
word_counts


## ---- echo = FALSE--------------------------
word_counts %>% group_by(title) %>% slice_max(tf_idf, n = 15) %>% ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(tf_idf, word)) +
  geom_col(show.legend = FALSE) +
  labs(x = "tf-idf", y = NULL) +
  facet_wrap(~title, ncol = 2, scales = "free") +
  theme_minimal()


## -------------------------------------------
X = cast_sparse(word_counts, title, word, n)
class(X)
dim(X)
sum(X>0)


## -------------------------------------------
X[,10001:10004]


## -------------------------------------------
rowSums(as.matrix(X))

