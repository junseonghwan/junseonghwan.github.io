## ----packages, echo=FALSE, message=FALSE, warning=FALSE--------
#remotes::install_github("hadley/emo") 
###how to install the emoji library

library(tidyverse)
# library(magick)
library(knitr)
library(emo)
# library(mosaicData)
# library(openintro)
library(DT)
# knitr::opts_chunk$set(cache = TRUE, autodep = TRUE)


## ----setup, include=FALSE--------------------------------------
# R options
options(
  htmltools.dir.version = FALSE, # for blogdown
  show.signif.stars = FALSE,     # for regression output
  warn = 1,
  pillar.print_max = 8,
  pillar.print_min = 4,
  pillar.max_footer_lines = 3,
  pillar.width = 60
  )
# Set dpi and height for images
knitr::opts_chunk$set(fig.height = 2.65, dpi = 300, warning=FALSE) 
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


## ----eval=FALSE------------------------------------------------
## lock(ride(find(who=tanzy, what = "helmet"), to = "campus"))


## ----eval=FALSE------------------------------------------------
## tanzy %>% find(what = "helmet") %>%
##   ride(to = "campus") %>%
##   lock()


## ----fig.height=1.8, fig.width = 3.75--------------------------
starwars %>%
  filter(species == "Human") %>%
  lm(mass ~ height, data = .)


## --------------------------------------------------------------
library(nycflights13)


## --------------------------------------------------------------
glimpse(flights)


## --------------------------------------------------------------
flights %>% filter(month == 4)


## --------------------------------------------------------------
flights %>% filter(month == 4 & origin == "JFK")


## --------------------------------------------------------------
flights %>% filter(month == 4, origin == "JFK")


## --------------------------------------------------------------
unique(flights$origin)
table(flights$origin, exclude = NULL)


## ---- eval = FALSE---------------------------------------------
## filter(flights, month == 11 | month == 12)


## --------------------------------------------------------------
dfdat = tibble(x = c(1, 2, NA),
                    y = c('A', 'B', 'C'))
dfdat
dfdat %>% filter(x == 1)


## --------------------------------------------------------------
dfdat %>% filter(x == 1 | is.na(x))


## --------------------------------------------------------------
NA > 5; 10 == NA; NA + 10


## --------------------------------------------------------------
x = NA
x == NA
is.na(x)


## ---- echo = FALSE, fig.height=4.5, fig.width=8----------------
    suppressPackageStartupMessages(library(gridExtra))
    plot_venn <- function(col1, col2, col3, col4, title, alpha = 0.5) {
        x1 <- seq(-1, 1, length = 300)
        y1 <- sqrt(1 - x1 ^ 2)
        y2 <- -sqrt(1 - x1 ^ 2)
        x2 <- x1 + 0.5
        x1 <- x1 - 0.5
        
        circdf1 <- data.frame(y = c(y1, y2[length(y2):1]), x = c(x1, x1[length(x1):1]))
        circdf2 <- data.frame(y = c(y1[length(y1):1], y2), x = c(x2[length(x2):1], x2))
        
        poly1 <- data.frame(x = c(x1[x1 < 0],
                                  x2[x2 < 0][sum(x2 < 0):1], 
                                  x2[x2 < 0], 
                                  x1[x1 < 0][sum(x1 < 0):1]),
                            y = c(y1[x1 < 0], 
                                  y1[length(y1):1][x2 < 0][sum(x2 < 0):1],
                                  y2[length(y2):1][x2 < 0],
                                  y2[x1 < 0][sum(x1 < 0):1]))
        poly2 <- poly1
        poly2$x <- poly1$x * -1
        
        poly3 <- data.frame(x = c(x2[x2 < 0][sum(x2 < 0):1], 
                                  x2[x2 < 0]),
                            y = c(y1[length(y1):1][x2 < 0][sum(x2 < 0):1],
                                  y2[length(y2):1][x2 < 0]))
        tempdf <- poly3[nrow(poly3):1, ]
        tempdf$x <- tempdf$x * -1
        poly3 <- rbind(poly3, tempdf)
        ggplot() +
          geom_polygon(data = poly1, 
                       mapping = aes(x = x, y = y),
                       fill = col1,
                       color = "black",
                       alpha = alpha) +
          geom_polygon(data = poly2, 
                       mapping = aes(x = x, y = y),
                       fill = col3,
                       color = "black",
                       alpha = alpha) +
          geom_polygon(data = poly3, 
                       mapping = aes(x = x, y = y),
                       fill = col2,
                       color = "black", 
                       alpha = alpha) +
          theme_void() +
          ggtitle(title) +
          theme(plot.title = element_text(hjust = 0.5), 
                plot.background = element_rect(color = col4)) +
          annotate(geom = "text", x = -0.8, y = 0, label = "x") +
          annotate(geom = "text", x = 0.8, y = 0, label = "y") ->
          pl
      return(pl)
    }
    
    aval <- 1
    col <- "#b1d9ef"
    grid.arrange(
      plot_venn(col, "white", "white", "white", "x & !y", alpha = aval),
      plot_venn("white", col, "white", "white", "x & y", alpha = aval),
      plot_venn("white", "white", col, "white", "!x & y", alpha = aval),
      plot_venn(col, col, "white", "white", "x", alpha = aval),
      plot_venn(col, "white", col, "white", "xor(x, y)", alpha = aval),
      plot_venn("white", col, col, "white", "y", alpha = aval),
      plot_venn(col, col, col, "white", "x | y", alpha = aval)
    )


## --------------------------------------------------------------
temp=flights %>% mutate(gain = dep_delay - arr_delay,
                 speed = distance / air_time * 60,
                 .before = year) #add before `year` variable
colnames(temp)
temp[,c("gain","speed")]


## ---- warning = FALSE------------------------------------------
ggplot(flights, aes(x = dep_time)) + geom_histogram(binwidth = 10)


## --------------------------------------------------------------
101 / 10
101 %/% 10
101 %% 10


## --------------------------------------------------------------
flights %>% mutate(dep_h = dep_time %/% 100,
            dep_m = dep_time %% 100,
            dep_elapsed_min = dep_h * 60 + dep_m,
            .after=dep_time)


## --------------------------------------------------------------
flights %>%
      transmute(gain = dep_delay - arr_delay,
                hours = air_time / 60,
                gain_per_hour = gain / hours)


## --------------------------------------------------------------
flights = flights %>% 
  mutate(delay_type = 
           case_when(dep_delay > 0 & arr_delay < dep_delay ~ 'made up time',
                    dep_delay > 0 & arr_delay >= dep_delay ~ 'lost time',
                    dep_delay <= 0 & arr_delay > 0 ~ 'headwind?', #<<
                    dep_delay <= 0 & arr_delay <= 0 ~ 'life is good'))#<<
table(flights$delay_type, exclude= NULL)


## --------------------------------------------------------------
flights = flights %>% 
  mutate(delay_type = 
           case_when(dep_delay > 0 & arr_delay < dep_delay ~ 'made up time',
                    dep_delay > 0 & arr_delay >= dep_delay ~ 'lost time',
                    arr_delay > 0 ~ 'headwind?', #<<
                    arr_delay <= 0 ~ 'life is good')) #<<
table(flights$delay_type, exclude= NULL)


## --------------------------------------------------------------
flights %>%
  select(carrier, origin) %>%
  table()


## --------------------------------------------------------------
flights %>%
  select(-year)


## --------------------------------------------------------------
flights %>%
  select(carrier:dest)


## --------------------------------------------------------------
flights_rn =  flights %>%
  rename(day_of_month = day)


## --------------------------------------------------------------
names(flights_rn)


## --------------------------------------------------------------
flights %>%
  summarize(mean_delay = mean(dep_delay, na.rm = TRUE),
            median_delay = median(dep_delay, na.rm = TRUE))


## --------------------------------------------------------------
flights %>%
  summarize(mean_delay = mean(dep_delay, na.rm = FALSE),
            median_delay = median(dep_delay, na.rm = FALSE))


## --------------------------------------------------------------
filter(flights, is.na(dep_delay))


## --------------------------------------------------------------
flights %>% 
  slice_head(n = 5)


## --------------------------------------------------------------
flights %>%
  slice_tail(n = 3)


## --------------------------------------------------------------
flights_n5 = flights %>%
  sample_n(5, replace = FALSE)
dim(flights_n5)


## --------------------------------------------------------------
flights_perc20 = flights %>%
  sample_frac(0.2, replace = FALSE)
dim(flights_perc20)


## --------------------------------------------------------------
flights %>% arrange(dep_delay) %>% relocate(dep_delay)


## --------------------------------------------------------------
flights %>% arrange(desc(arr_delay)) %>%
  relocate(arr_delay)


## --------------------------------------------------------------
flights %>% select(sched_dep_time, origin, dest, day, carrier, flight) %>%
  arrange(origin, dest,sched_dep_time, day)


## --------------------------------------------------------------
flights %>% 
  select(origin, dest) %>% 
  distinct() %>% 
  arrange(origin, dest)


## --------------------------------------------------------------
flights %>% slice_head(n = 5) %>% pull(dest)


## --------------------------------------------------------------
flights %>% slice_head(n = 5) %>% select(dest)


## --------------------------------------------------------------
ontime = flights %>% mutate(ontime = arr_delay <= 0)  
ontime %>% summarize(ontime_pct = mean(ontime, na.rm = TRUE)*100)


## --------------------------------------------------------------
ontime %>% 
  group_by(carrier) %>% #<<
  summarize(ontime_pct = mean(ontime, na.rm = TRUE)*100) %>%
  arrange(ontime_pct)

