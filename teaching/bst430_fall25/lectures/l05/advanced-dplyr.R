## ----child = "setup.Rmd"----------

## ----setup, include=FALSE---------
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


## ---- include = FALSE-------------
library(tidyverse)


## ---------------------------------
library(nycflights13)
ontime = flights %>% 
  mutate(ontime = arr_delay <= 0)  
ontime %>% 
  summarize(ontime_pct = mean(ontime, na.rm = TRUE)*100)


## ---------------------------------
ontime %>% group_by(carrier) %>% 
  summarize(ontime_pct = mean(ontime, na.rm = TRUE)*100) #<<


## ----hourly-departures-drop, fig.show='hide'----
departures = flights %>% group_by(carrier, hour) %>% summarize(n_departures = n())
ggplot(departures, aes(x = hour, y = n_departures)) + 
  geom_line() + 
  facet_wrap(~carrier, scales = 'free_y') #<<


## ----ref.label="hourly-departures-drop", echo = FALSE, out.width="75%"----


## ----hourly-departures-keep, fig.show='hide'----
departures2 = flights %>% 
  mutate(carrier = factor(carrier), hour = factor(hour)) %>% #<<
  group_by(carrier, hour, .drop = FALSE) %>% #<<
  summarize(n_departures = n())
plt = ggplot(departures2, aes(x = hour, y = n_departures, group = carrier)) + 
  geom_line() + 
  facet_wrap(~carrier, scales = 'free_y')
plt


## ----ref.label="hourly-departures-keep", echo = FALSE, out.width="75%"----


## ----hourly-departures-keep-hour, fig.show='hide', message = TRUE----
departures3 = flights %>% 
  mutate(carrier = factor(carrier), hourf = factor(hour)) %>%
  group_by(carrier, hourf, .drop = FALSE) %>%
  summarize(n_departures = n()) %>%
  mutate(hour = as.numeric(as.character(hourf))) #<<
(plt %+% departures3) + aes(x = hour) #<<


## ----ref.label="hourly-departures-keep-hour", echo = FALSE, out.width="75%"----


## ---------------------------------
options(dplyr.summarise.inform = FALSE)


## ---------------------------------
ontime_drop = ontime %>% group_by(carrier, dest) %>% summarize(ontime_pct = mean(ontime, na.rm = TRUE)*100)
ontime_drop %>% summarize(n_dest = n_distinct(dest))


## ---------------------------------
ontime_drop %>% ungroup() %>% #<<
  summarize(n_dest = n_distinct(dest))


## ---------------------------------
ontime_1500 = ontime %>% group_by(dest) %>% filter(n() > 1500) %>%
  mutate(n_flights_dest = n(), .after = 1)

ontime_1500 %>% arrange(n_flights_dest)



## ---- fig.width = 10, fig.asp=.5----
ontime_drop = ontime_1500 %>% group_by(dest, carrier) %>%  
  summarize(ontime_pct = mean(ontime, na.rm  = TRUE)*100)
ggplot(ontime_drop, aes(y = ontime_pct, x = dest)) + geom_point() + coord_flip() + theme_minimal(base_size = 8)


## ---------------------------------
ontime_drop = ontime_1500 %>% group_by(dest, carrier) %>%  
  summarize(ontime_pct = mean(ontime, na.rm  = TRUE)*100,
            n_departed = sum(!is.na(ontime)))

ggplot(ontime_drop, aes(y = ontime_pct, x = dest, color = n_departed<10)) + geom_point() + coord_flip() + theme_minimal(base_size = 8)


## ---------------------------------
ontime_drop_10 = filter(ontime_drop, n_departed >= 10)
ggplot(ontime_drop_10, aes(y = ontime_pct, x = dest)) + geom_boxplot() + 
  coord_flip()  + theme_minimal(base_size = 8)


## ---------------------------------
 ontime_drop_10 = ontime_drop_10 %>% ungroup() %>% 
  mutate(dest = forcats::fct_reorder(dest, ontime_pct)) #<<
ggplot(ontime_drop_10, aes(x = dest, y = ontime_pct)) + geom_boxplot() + 
  coord_flip()  + theme_minimal(base_size = 8)


## ---------------------------------
flights %>% select(ends_with('delay'))


## ---------------------------------
flights %>% select(!contains('time'))


## ---------------------------------
flights %>% select(where(is.character))


## ---------------------------------
flights %>% select(where(~!any(is.na(.x))))


## ---- include = FALSE-------------
nycbnb = read_csv("https://urmc-bst.github.io/bst430-fall2021-site/hw_lab_instruction/hw-01-airbnb/data/nylistings.csv")


## ---------------------------------
nycbnb


## ----nights-plot, fig.show='hide'----
availability = nycbnb %>% group_by(neighborhood) %>% 
  summarize(across(c(maximum_nights, minimum_nights, availability_90), median)) #<<
ggplot(availability, 
       aes(x = neighborhood, 
           ymin = minimum_nights,  
           y = availability_90, 
           ymax = maximum_nights)) + 
  geom_pointrange() + coord_flip() + theme_minimal() +  
  labs(x = NULL, y = 'Availability (minimum, 90 day, maximum)')


## ---- ref.label='nights-plot',  echo = FALSE, out.width="75%"----


## ---------------------------------
median_mad = nycbnb %>% group_by(neighborhood) %>% 
  summarise(across(where(is.numeric), #<<
                   list(median = median, mad = mad))) #<<
glimpse(median_mad)

