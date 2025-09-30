## ----child = "setup.Rmd"----------------------------------------------------------------------------

## ----setup, include=FALSE---------------------------------------------------------------------------
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



## ----include = FALSE--------------------------------------------------------------------------------
library(tidyverse)
library(nycflights13)
library(lubridate)


## ----warning = FALSE--------------------------------------------------------------------------------
my_list = list(A = 1:4, B = rnorm(4), C = stringr::fruit[1:4])
my_list


## ---------------------------------------------------------------------------------------------------
print(my_list)


## ---------------------------------------------------------------------------------------------------
print


## #include <iostream>
## 
## class Person {
##  public:
##   void Print() const;
## 
##  private:
##   std::string name_;
##   int age_ = 5;
## };
## 
## void Person::Print() const {
##   std::cout << name_ << ':' << age_ << '\n';
## }

## ----echo = FALSE-----------------------------------------------------------------------------------
DT::datatable(tibble::tibble(method = methods(print)))


## ----output.lines = 15------------------------------------------------------------------------------
base:::print.data.frame


## ---------------------------------------------------------------------------------------------------
class(my_list) = 'dr_awesome'
print.dr_awesome = function(x, ...){
  type = class(x)
  len = length(x)
  cat(glue::glue("An object of type {type} and length {len}.
  Andrew drools, Dr. Awesome Rules!"))
}


## ---------------------------------------------------------------------------------------------------
print(my_list)


## ----init-cache, eval = FALSE-----------------------------------------------------------------------
## knitr::opts_chunk$set(cache = TRUE, autodep = TRUE)


## ----echo = FALSE-----------------------------------------------------------------------------------
knitr::include_graphics('l14/img/knitr-clear-cache.png')


## ----eval = FALSE-----------------------------------------------------------------------------------
## rmarkdown::render("<path to my document>")


## ---------------------------------------------------------------------------------------------------
base_plot = function(x, y, list_data = NULL) {
  if (!is.null(list_data)) 
    plot(list_data, main="A plot from list_data!")
  else
    plot(x, y, main="A plot from x, y!")
}


## ----error=TRUE-------------------------------------------------------------------------------------
base_plot(list_data=list(x=-10:10, y=(-10:10)^3))


## ----error=TRUE-------------------------------------------------------------------------------------
base_plot() # Easy to understand error message
base_plot(list_data=list(x=-10:10, Y=(-10:10)^3)) # Not as clear


## ----cancel-plot-1, fig.show = 'hide', error = TRUE-------------------------------------------------
flights %>% 
  mutate(hourf = as.factor(hour), day_of_week = weekdays(ymd(glue::glue("{year}-month-{day}")))) %>%
  group_by(hourf, day_of_week, .drop = FALSE) %>%
  summarize(cancel_rate = mean(is.na(arr_time))) %>%
  ggplot(aes(x = hour, y = day_of_week, fill = cancel_rate)) + 
    geom_tile() + 
    scale_fill_distiller(palette = 'PuBu', direction = 1)


## ----ref.label = "cancel-plot-1", echo = FALSE, error = TRUE----------------------------------------


## ----cancel-plot-2, fig.show = 'hide'---------------------------------------------------------------
cancellation = flights %>% 
  mutate(hourf = as.factor(hour), day_of_week = weekdays(ymd(glue::glue("{year}-{month}-{day}")))) %>%
  group_by(hourf, day_of_week, .drop = FALSE) %>%
  summarize(cancel_rate = mean(is.na(arr_time)), n_flights =  n()) %>%
  filter(n_flights > 500)

cancellation %>%
  ggplot(aes(x = hourf, y = day_of_week, fill = cancel_rate)) + 
    geom_tile() + 
    scale_fill_distiller(palette = 'PuBu', direction = 1)


## ----ref.label = "cancel-plot-2", echo = FALSE------------------------------------------------------


## ----echo = FALSE-----------------------------------------------------------------------------------
knitr::include_graphics('l14/img/l14-debugging.png')


## ----eval=FALSE-------------------------------------------------------------------------------------
## # As an example, here's a code chunk that we can keep around in this Rmd doc,
## # but that will never be evaluated (because eval=FALSE) in the Rmd file, take
## # a look at it!
## mat = matrix(rnorm(1000)^3, 1000, 1000)
## mat
## # Note that the output of big.mat is not printed to the console, and also
## # that big.mat was never actually created! (This code was not evaluated)

