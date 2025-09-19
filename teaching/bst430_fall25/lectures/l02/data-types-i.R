## ----child = "setup.Rmd"---------------------------------

## ----setup, include=FALSE--------------------------------
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



## ----packages, echo=FALSE, message=FALSE, warning=FALSE----
library(tidyverse)
library(DT)
library(scales)


## ----message=FALSE---------------------------------------
cat_lovers = read_csv("l04/data/cat-lovers.csv")


## ----echo=FALSE------------------------------------------
cat_lovers


## --------------------------------------------------------
cat_lovers %>%
  summarise(mean_cats = mean(number_of_cats))


## ----eval=FALSE------------------------------------------
## ?mean


## ----echo=FALSE, caption="Help for mean", out.width="75%"----
knitr::include_graphics("l04/img/mean-help.png")


## --------------------------------------------------------
cat_lovers %>%
  summarise(mean_cats = mean(number_of_cats, na.rm = TRUE))


## --------------------------------------------------------
glimpse(cat_lovers)


## ----echo=FALSE------------------------------------------
options(htmltools.preserve.raw = FALSE)
cat_lovers %>%
  datatable()


## --------------------------------------------------------
cat_lovers %>%
  mutate(number_of_cats = case_when(
    name == "Ginger Clark" ~ 2,
    name == "Doug Bass"    ~ 3,
    TRUE                   ~ as.numeric(number_of_cats)
    )) %>%
  summarise(mean_cats = mean(number_of_cats))


## --------------------------------------------------------
cat_lovers %>%
  mutate(
    number_of_cats = case_when(
      name == "Ginger Clark" ~ "2",
      name == "Doug Bass"    ~ "3",
      TRUE                   ~ number_of_cats
      ),
    number_of_cats = as.numeric(number_of_cats)
    ) %>%
  summarise(mean_cats = mean(number_of_cats))


## --------------------------------------------------------
cat_lovers = cat_lovers %>% #<<
  mutate(
    number_of_cats = case_when(
      name == "Ginger Clark" ~ "2",
      name == "Doug Bass"    ~ "3",
      TRUE                   ~ number_of_cats
      ),
    number_of_cats = as.numeric(number_of_cats)
    )


## --------------------------------------------------------
typeof(TRUE)


## --------------------------------------------------------
typeof("hello")


## --------------------------------------------------------
typeof(1.335)
typeof(7)


## --------------------------------------------------------
typeof(7L)
typeof(1:3)


## --------------------------------------------------------
roots_of_unity = 
  c(1+0i, -1+0i, 0+1i, 0-1i)
typeof(roots_of_unity)
roots_of_unity^2


## --------------------------------------------------------
roots_of_unity^4
Re(roots_of_unity)
Im(roots_of_unity)


## --------------------------------------------------------
mylist = list("A", 1:4, c(TRUE, FALSE))
mylist


## --------------------------------------------------------
str(mylist) #<<


## --------------------------------------------------------
digits = c(1, 2, 3)
hello = c("Hello", "World!")
greet = c(c("hi", "hello"), 
              c("bye", "jello"))


## --------------------------------------------------------
str(digits)
str(hello)
str(greet)


## --------------------------------------------------------
x = c(1, 2, 3)
y = character(2)
empty_dbl = numeric(0)
empty_chr = character(0)


## --------------------------------------------------------
length(x)
length(y)
length(empty_dbl)
length(empty_chr)


## --------------------------------------------------------
list1 = list(1, 2, 3)
list2 = list(
  c("Hi!", "I'm a vector", 
    "nested inside", "a list"))
cat12 = c(list1, list2)


## --------------------------------------------------------
str(cat12)


## --------------------------------------------------------
length(list1)
length(list2)
length(c(list1, list2))


## --------------------------------------------------------
myotherlist = list(A = "hello", B = 1:4, "knock knock" = "who's there?")
str(myotherlist)
names(myotherlist)
myotherlist$B


## --------------------------------------------------------
str(myotherlist)
unlist(myotherlist, recursive = TRUE)


## --------------------------------------------------------
x = 1:3
x
typeof(x)


## --------------------------------------------------------
y = as.character(x)
y
typeof(y)


## --------------------------------------------------------
x = c(TRUE, FALSE)
x
typeof(x)


## --------------------------------------------------------
y = as.numeric(x)
y
typeof(y)


## --------------------------------------------------------
c(1, "Hello")
c(FALSE, 3L)


## --------------------------------------------------------
c(1.2, 3L)
c(2L, "two")


## --------------------------------------------------------
pi / 0
0 / 0


## --------------------------------------------------------
1/0 - 1/0
1/0 + 1/0


## --------------------------------------------------------
x = c(1, 2, 3, 4, NA)


## --------------------------------------------------------
mean(x)
mean(x, na.rm = TRUE)
summary(x)


## --------------------------------------------------------
typeof(NA)


## --------------------------------------------------------
# TRUE or NA
TRUE | NA


## --------------------------------------------------------
# FALSE or NA
FALSE | NA


## --------------------------------------------------------
TRUE | TRUE  # if NA was TRUE
TRUE | FALSE # if NA was FALSE


## --------------------------------------------------------
FALSE | TRUE  # if NA was TRUE
FALSE | FALSE # if NA was FALSE


## ---- error=TRUE-----------------------------------------
x = c(8,4,7)


## --------------------------------------------------------
x[1]


## --------------------------------------------------------
x[[1]]


## --------------------------------------------------------
y = list(8,4,7)


## --------------------------------------------------------
y[2]


## --------------------------------------------------------
y[[2]]


## --------------------------------------------------------
typeof(1)


## --------------------------------------------------------
typeof("A")


## --------------------------------------------------------
typeof(list(1))


## --------------------------------------------------------
attributes(1)


## --------------------------------------------------------
attributes(starwars)


## --------------------------------------------------------
class(starwars)


## --------------------------------------------------------
class(1)


## --------------------------------------------------------
class(ggplot(starwars, aes(x = weight))+ geom_histogram())


## --------------------------------------------------------
df = tibble(x = 1:3, y = c("a", "b", "c"))
typeof(df)
class(df)
str(df)


## --------------------------------------------------------
attributes(df)
typeof(df$y)


## --------------------------------------------------------
mean_cats = cat_lovers %>%
  summarise(mean_cats = mean(number_of_cats))

cat_lovers %>%
  filter(number_of_cats < mean_cats) %>%
  nrow()


## --------------------------------------------------------
mean_cats
str(mean_cats)


## --------------------------------------------------------
mean_cats = cat_lovers %>%
  summarise(mean_cats = mean(number_of_cats)) %>%
  pull()

cat_lovers %>%
  filter(number_of_cats < mean_cats) %>%
  nrow()


## --------------------------------------------------------
mean_cats
class(mean_cats)


## --------------------------------------------------------
x = factor(c("BS", "MS", "PhD", "MS"))


## --------------------------------------------------------
attributes(x)


## --------------------------------------------------------
typeof(x)


## --------------------------------------------------------
str(cat_lovers)


## --------------------------------------------------------
ggplot(cat_lovers, mapping = aes(x = handedness)) +
  geom_bar()


## --------------------------------------------------------
cat_lovers = cat_lovers %>%
  mutate(handedness = fct_relevel(handedness, 
                                  "right", "left", "ambidextrous"))


## --------------------------------------------------------
ggplot(cat_lovers, mapping = aes(x = handedness)) +
  geom_bar()

