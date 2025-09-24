
## ---- include = FALSE-----------
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


## ---- output.lines = 13---------
diagnoses = read_csv('data/cardiac-dx.csv')
diagnoses


## -------------------------------
diagnoses %>% count(diagnoses) %>% 
  arrange(desc(n))


## -------------------------------
filter(diagnoses, str_detect(diagnoses, 'hypoplastic'))


## ----  output.lines = 10--------
diagnoses_row = diagnoses %>% 
  tidyr::separate_rows(diagnoses, 
                       sep = " \\| ") #WTH? #<< 
diagnoses_row


## -------------------------------
diagnoses_row %>% filter(str_detect(diagnoses, 'hypoplastic')) %>%
  count(diagnoses) %>% arrange(desc(n))


## ----dx0, fig.show = 'hide'-----
diagnoses_row %>% filter(str_detect(diagnoses, 'hypoplastic')) %>%
  ggplot(aes(y = diagnoses, x = as.factor(id))) + 
  geom_tile() + 
  scale_x_discrete(breaks = NULL) +
  labs(y = "Diagnosis", x = 'Patient', 
       main = 'Co-occurrence of hypoplastic heart disorders')


## ---- ref.label='dx0',  echo = FALSE, out.width="75%"----


## ----dx-tile, fig.show = 'hide'----
diagnoses_row %>% filter(str_detect(diagnoses, 'hypoplastic')) %>%
  ggplot(aes(y = fct_infreq(diagnoses), x = fct_infreq(as.factor(id)))) + #<< 
  geom_tile() + 
  scale_x_discrete(breaks = NULL) +
  labs(y = "Diagnosis", x = 'Patient', 
       main = 'Co-occurrence of hypoplastic heart disorders')


## ---- ref.label='dx-tile',  echo = FALSE, out.width="75%"----


## ----dx-tile-wrap, fig.show = 'hide'----
diagnoses_row %>% filter(str_detect(diagnoses, 'hypoplastic')) %>%
  mutate(diagnoses = str_wrap(diagnoses, width = 40)) %>% #<<
  ggplot(aes(y = fct_infreq(diagnoses), x = fct_infreq(as.factor(id)))) + 
  geom_tile() + 
  scale_x_discrete(breaks = NULL) +
  labs(y = "Diagnosis", x = 'Patient', 
       main = 'Co-occurrence of hypoplastic heart disorders')


## ---- ref.label='dx-tile-wrap',  echo = FALSE, out.width="75%"----


## ----dx-tile-wrap-just, fig.show = 'hide'----
diagnoses_row %>% filter(str_detect(diagnoses, 'hypoplastic')) %>%
  mutate(diagnoses = str_wrap(diagnoses, width = 40)) %>%
  ggplot(aes(y = fct_infreq(diagnoses), x = fct_infreq(as.factor(id)))) + 
  geom_tile() + 
  theme(axis.text.y = element_text(hjust = 0, vjust = 0, size = 8)) + #<<
  scale_x_discrete(breaks = NULL) +
  labs(y = "Diagnosis", x = 'Patient', 
       main = 'Co-occurrence of hypoplastic heart disorders')


## ---- ref.label='dx-tile-wrap-just',  echo = FALSE, out.width="75%"----


## ---- include = FALSE-----------
library(stringr)
library(glue)


## -------------------------------
names = c("Jeff B.", "Larry E.", "Warren B.")
favorite_food = c("caviar", "cake", "Pappy Van Winkle")
str_c(names, 
      " likes ", #note additional spaces
      favorite_food, ".")


## -------------------------------
dinner = glue::glue("{names} likes {favorite_food}.")
dinner


## -------------------------------
glue::glue("{names} \n {favorite_food} \U1F600.")


## -------------------------------
names
nchar(names)


## -------------------------------
str_sub(dinner, 1, 11)


## -------------------------------
str_sub(dinner, 
        #space + l-
        nchar(names) + 2, 
        #space + l-i-k-e
        nchar(names) + 6 
) = "demands"
dinner


## -------------------------------
str_split_fixed(dinner, " ",  4)


## -------------------------------
str_split_fixed(dinner, " ", 6)


## -------------------------------
str_split(dinner, " ")


## ----read-samplesheet-----------
data = read_csv("data/sample_sheet2.csv")
data


## ----split-folder---------------
char_matrix = str_split_fixed(data$folder_name, pattern = "_", n = 6)
char_matrix


## -------------------------------
data %>% select(folder_name) %>% 
  mutate(treatment = char_matrix[,2],
         sample_ID = str_c(char_matrix[,2], "_", char_matrix[,3]),
         tissue_source = char_matrix[,4])


## -------------------------------
data %>% select(dataset) %>% 
  mutate(new_data = str_to_lower(dataset))


## -------------------------------
str_c(data$sample_ID, collapse=";")


## -------------------------------
lunch = c("one app", "two appetizers", "three apples")
str_view_all(lunch, 'apple')


## -------------------------------
str_view_all(lunch, 'app.')


## -------------------------------
str_view_all(lunch, 'app[le]')


## -------------------------------
str_view_all(lunch, 'app(le|etizer)s')


## -------------------------------
str_view_all(lunch, 'app.*')


## -------------------------------
str_view_all("red tired", "\\bred\\b")


## -------------------------------
str_view_all("red tired", "red")


## -------------------------------
str_detect(string = c("A", "AA", "AB", "B"), 
           pattern = "A")
str_detect(string = lunch, pattern = 'app.')


## -------------------------------
feline = c("The fur of cats goes by many names.", 
           "Infimum (the cat) is a cat with a most baleful meow.",
           "Dog.")
str_extract(string = feline, pattern = "cat")
str_extract_all(string = feline, pattern = "cat")


## -------------------------------
feline = c("The fur of cats goes by many names.", 
           "Infimum (the cat) is a cat with a most baleful meow.",
           "Dog.")
str_match(feline, "cat")


## -------------------------------
feline = c("The fur of cats goes by many names.", 
           "Infimum (the cat) is a cat with a most baleful meow.",
           "Dog.")
str_match(feline, "(\\w*) cat.? (\\w*)")


## -------------------------------
feline = c("The fur of cats goes by many names.", 
           "Infimum (the cat) is a cat with a most baleful meow.",
           "Dog.")

str_match_all(feline, "(\\w*) cat.? (\\w*)")



## -------------------------------
feline = c("The fur of cats goes by many names.", 
           "Infimum (the cat) is a cat with a most baleful meow.",
           "Dog.")

str_replace(feline, "cat", "murder machine")


## -------------------------------
feline = c("The fur of cats goes by many names.", 
           "Infimum (the cat) is a cat with a most baleful meow.",
           "Dog.")

str_replace_all(feline, "cat", "murder machine")


## -------------------------------
str_replace_all(feline, "(\\w*)", "\\1\\1")


## -------------------------------
word_df = tibble(word = words)


## -------------------------------
filter(word_df, str_detect(word, "x"))


## -------------------------------
filter(word_df, str_detect(word, "x$"))


## -------------------------------
filter(word_df, str_detect(word, "^x"))


## -------------------------------
filter(word_df, str_detect(word, '.+x.+'))


## -------------------------------
sentence_df = tibble(sentence  = sentences)


## -------------------------------
sentence_df = sentence_df %>% mutate(second = str_match(sentence, "([^ ]*s)\\b")[,2])

