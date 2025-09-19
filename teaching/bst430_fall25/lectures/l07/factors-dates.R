## ----child = "setup.Rmd"-----------------

## ----setup, include=FALSE----------------
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


## ----------------------------------------
x = factor(c("BS", "MS", "PhD", "MS"))
x


## ----------------------------------------
typeof(x)


## ----------------------------------------
class(x)


## ----------------------------------------
glimpse(x)
as.integer(x)


## ----------------------------------------
library(lubridate)
y = ymd("2020-01-01")
y
typeof(y)
class(y)


## ----------------------------------------
as.integer(y)
as.integer(y) / 365 # roughly 50 yrs


## ----include=FALSE-----------------------
library(nycflights13)
flights_sml = flights %>% 
  select(origin:dest, year:dep_time, hour:minute) 
flights10 = flights_sml %>%
  semi_join(flights %>% count(dest) %>%
              mutate(rank = min_rank(-n)) %>% filter(rank<=15)) 


## ----------------------------------------
glimpse(flights10)


## ----out.width="60%"---------------------
ggplot(flights10, mapping = aes(x = dest)) +
  geom_bar() + 
  theme_minimal() + 
  coord_flip()


## ----out.width="55%"---------------------
library(forcats)
plt = flights10 %>% # assign plot to object #<<
  mutate(dest_fct = fct_infreq(dest)) %>% #<<
  ggplot(mapping = aes(x = dest_fct)) +
  geom_bar()  + 
  coord_flip() + theme_minimal()
plt #print it #<<


## ----------------------------------------
plt %+% # #<< Replace plot data 
  (flights_sml %>%
  mutate(dest_fct = fct_lump_n(dest, 15) %>% #<<
           fct_infreq))


## ----------------------------------------
plt %+%
  (flights_sml %>%
  mutate(dest_fct = fct_recode(dest, 'Western NY' = 'ROC', #<<
                               'Western NY' = 'BUF',#<<
                               'Western NY' = 'SYR') %>% #<<
           fct_lump_n(15) %>% fct_infreq))


## ----echo=FALSE, out.width="65%", fig.align="center"----
knitr::include_graphics("l07/img/lubridate-not-part-of-tidyverse.png")


## ----output.lines=7----------------------
library(glue)

flights %>%
  mutate(
    date = glue("{year} {month} {day}") #<<
    ) %>% 
  relocate(date)


## ----------------------------------------
flights %>%
  mutate(date = glue("{year} {month} {day}")) %>%
  count(date)


## ----out.width="80%", fig.asp = 0.4------
flights %>%
  mutate(date = glue("{year} {month} {day}")) %>%
  count(date) %>%
  ggplot(aes(x = date, y = n, group = 1)) +
  geom_line()


## ----out.width="80%", echo=FALSE, fig.asp = 0.4----
flights %>%
  mutate(date = glue("{year} {month} {day}")) %>%
  count(date) %>%
  slice(1:7) %>%
  ggplot(aes(x = date, y = n, group = 1)) +
  geom_line()


## ----output.lines=7----------------------
library(lubridate)

flights %>%
  mutate(
    date = ymd(glue("{year} {month} {day}")) #<<
    ) %>% 
  relocate(date)


## ----------------------------------------
flights %>%
  mutate(date = ymd(glue("{year} {month} {day}"))) %>% 
  count(date)


## ----out.width="80%", fig.asp = 0.4------
flights %>%
  mutate(date = ymd(glue("{year} {month} {day}"))) %>% 
  count(date) %>%
  ggplot(aes(x = date, y = n, group = 1)) +
  geom_line()


## ----------------------------------------
dmy_hms('22-Sep-2021 11:00:00')


## ----------------------------------------
dmy_hms('22-Sep-2021 11:00:00', tz = 'America/New_York')


## ----------------------------------------
flights_sml = flights_sml %>% 
  mutate(time = hm(glue("{hour} {minute}"))) %>% #<<
  relocate(time)
flights_sml


## ----------------------------------------
ggplot(flights_sml, aes(x = time, fill = origin))


## ----------------------------------------
ggplot(flights_sml, aes(x = time, fill = origin))+ 
  geom_density(alpha = .5) + 
  scale_x_time() #<<


## ----departure-time-plot, fig.show = 'hide'----
plt = ggplot(flights_sml, aes(x = time, after_stat(count), fill = origin)) + 
  geom_density(alpha = .5, bw = 1800) + # 30*60 seconds #<<
  scale_x_time() + 
  scale_y_continuous(sec.axis = 
        sec_axis(trans = ~ .x/365*3600,  name = 'Departures/day/hour')) + #<<
  theme_minimal() +
  labs(y = "Departures/year/second")
plt


## ---- ref.label='departure-time-plot',  echo = FALSE, out.width="75%"----


## ----departure-region-plot, fig.show = 'hide'----
flights_jn = flights_sml %>% 
  left_join(airports, c('dest' = 'faa')) %>% 
  filter(!is.na(lon)) %>% #missing puerto rico
  mutate(region = cut(lon, #<<
                      breaks = c(-158, -124, -104, -83, -66), # 5 breakpoints #<<
                      labels = c('HI/AK', 'West', 'Central', 'East'))) # 4 groups #<<

plt %+% flights_jn + 
  facet_wrap(~region, scales = 'free_y') + 
  theme(axis.text.x = element_text(angle = 90))


## ---- ref.label='departure-region-plot',  echo = FALSE, out.width="75%"----

