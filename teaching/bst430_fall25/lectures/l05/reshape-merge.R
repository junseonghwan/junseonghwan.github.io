## ----child = "setup.Rmd"-----------------------------------------------

## ----setup, include=FALSE----------------------------------------------
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



## ----include=FALSE-----------------------------------------------------
library(tidyverse)
library(knitr)
customers = read_csv("l05a/data/sales/customers.csv")
prices = read_csv("l05a/data/sales/prices.csv")


## ----echo=FALSE--------------------------------------------------------
customers


## ----echo=FALSE--------------------------------------------------------
customers %>%
  pivot_longer(cols = item_1:item_3, names_to = "item_no", values_to = "item")


## ----dplyr-part-of-tidyverse, echo=FALSE, out.width="60%", caption = "tidyr is part of the tidyverse"----
include_graphics("l05a/img/tidyr-part-of-tidyverse.png")


## ----echo=FALSE,out.width="70%"----------------------------------------
include_graphics("l05a/img/pivot.gif")


## ----echo=FALSE, out.width="45%", out.extra ='style="background-color: #FDF6E3"'----
include_graphics("l05a/img/tidyr-longer-wider.gif")


## ----echo=FALSE--------------------------------------------------------
customers


## ----echo=FALSE--------------------------------------------------------
customers %>%
  pivot_longer(cols = item_1:item_3, names_to = "item_no", values_to = "item")


## ----eval=FALSE--------------------------------------------------------
## pivot_longer(
##   data, #<<
##   cols,
##   names_to = "name",
##   values_to = "value"
##   )


## ----eval=FALSE--------------------------------------------------------
## pivot_longer(
##   data,
##   cols, #<<
##   names_to = "name",
##   values_to = "value"
##   )


## ----eval=FALSE--------------------------------------------------------
## pivot_longer(
##   data,
##   cols,
##   names_to = "name", #<<
##   values_to = "value"
##   )


## ----eval=FALSE--------------------------------------------------------
## pivot_longer(
##   data,
##   cols,
##   names_to = "name",
##   values_to = "value" #<<
##   )


## ----------------------------------------------------------------------
purchases = customers %>%
  pivot_longer( #<<
    cols = item_1:item_3,  # variables item_1 to item_3 #<<
    names_to = "item_no",  # column names -> new column called item_no #<<
    values_to = "item"     # values in columns -> new column called item #<<
    ) #<<

purchases


## ----------------------------------------------------------------------
prices


## ----------------------------------------------------------------------
purchases %>%
  left_join(prices) #<<


## ----------------------------------------------------------------------
purchases %>%
  pivot_wider( #<<
    names_from = item_no, #<<
    values_from = item #<<
  ) #<<


## ----echo=FALSE, out.width="70%"---------------------------------------
knitr::include_graphics("l05a/img/biden-approval.png")


## ----include=FALSE-----------------------------------------------------
biden = read_csv("l05a/data/trump/biden.csv")


## ----------------------------------------------------------------------
biden


## ----echo=FALSE, out.width="100%"--------------------------------------
biden %>%
  pivot_longer(
    cols = c(approval, disapproval),
    names_to = "rating_type",
    values_to = "rating_value"
  ) %>%
  ggplot(aes(x = date, y = rating_value, 
             color = rating_type, group = rating_type)) +
  geom_line() +
  facet_wrap(~ subgroup) +
  scale_color_manual(values = c("darkgreen", "orange")) + 
  labs( 
    x = "Date", y = "Rating", 
    color = NULL, 
    title = "How (un)popular is Joseph Biden?", 
    subtitle = "Estimates based on polls of all adults and polls of likely/registered voters", 
    caption = "Source: FiveThirtyEight modeling estimates" 
  ) + 
  theme_minimal() +
  theme(legend.position = "bottom")


## ----output.lines=11---------------------------------------------------
biden_longer = biden %>%
  pivot_longer(
    cols = c(approval, disapproval),
    names_to = "rating_type",
    values_to = "rating_value"
  )

biden_longer


## ----fig.asp = 0.5-----------------------------------------------------
ggplot(biden_longer, 
       aes(x = date, y = rating_value, color = rating_type, group = rating_type)) +
  geom_line() +
  facet_wrap(~ subgroup)


## ----"biden-plot", fig.show="hide"-------------------------------------
ggplot(biden_longer, 
       aes(x = date, y = rating_value, 
           color = rating_type, group = rating_type)) +
  geom_line() +
  facet_wrap(~ subgroup) +
  scale_color_manual(values = c("darkgreen", "orange")) + #<<
  labs( #<<
    x = "Date", y = "Rating", #<<
    color = NULL, #<<
    title = "How (un)popular is Joseph Biden?", #<<
    subtitle = "Estimates based on polls of all adults and polls of likely/registered voters", #<<
    caption = "Source: FiveThirtyEight modeling estimates" #<<
  ) #<<


## ----ref.label="biden-plot", echo = FALSE, out.width="75%"-------------


## ----"biden-plot-2", fig.show="hide"-----------------------------------
ggplot(biden_longer, 
       aes(x = date, y = rating_value, 
           color = rating_type, group = rating_type)) +
  geom_line() +
  facet_wrap(~ subgroup) +
  scale_color_manual(values = c("darkgreen", "orange")) + 
  labs( 
    x = "Date", y = "Rating", 
    color = NULL, 
    title = "How (un)popular is Joseph Biden?", 
    subtitle = "Estimates based on polls of all adults and polls of likely/registered voters", 
    caption = "Source: FiveThirtyEight modeling estimates" 
  ) + 
  theme_minimal() + #<<
  theme(legend.position = "bottom") #<<


## ----ref.label="biden-plot-2", echo = FALSE, out.width="75%", fig.width=6----


## ----------------------------------------------------------------------
table2


## ----------------------------------------------------------------------
table4a


## ---- echo = FALSE, include = FALSE------------------------------------
sales = tibble::tribble(
  ~quarter, ~year, ~sales,
  "Q1",    2000,    66013,
  "Q2",      NA,    69182,
  "Q3",      NA,    53175,
  "Q4",      NA,    21001,
  "Q1",    2001,    46036,
  "Q2",      NA,    58842,
  "Q3",      NA,    44568,
  "Q4",      NA,    50197,
  "Q1",    2002,    39113,
  "Q2",      NA,    41668,
  "Q3",      NA,    30144,
  "Q4",      NA,    52897,
  "Q1",    2004,    32129,
  "Q2",      NA,    67686,
  "Q3",      NA,    31768,
  "Q4",      NA,    49094
)
options(pillar.print_max = 12, pillar.print_min = 12)


## ----------------------------------------------------------------------
sales


## ----------------------------------------------------------------------
sales %>% fill(year)


## ---- echo = FALSE, include = FALSE------------------------------------
df = tibble(
  group = c(1:2, 1),
  item_id = c(1:2, 2),
  value = 1:3
)


## ----------------------------------------------------------------------
df


## ----------------------------------------------------------------------
df %>% complete(group,item_id)



## ---- include = FALSE--------------------------------------------------
df = tibble(
  x = 1:3,
  y = c("a", "d,e,f", "g,h"),
  z = c("1", "2,3,4", "5,6")
)


## ----------------------------------------------------------------------
df


## ----------------------------------------------------------------------
separate_rows(df, y, z, convert = TRUE)


## ----------------------------------------------------------------------
df


## ----------------------------------------------------------------------
separate(df, y, c('y1', 'y2', 'y3'))


## ----include=FALSE-----------------------------------------------------
professions = read_csv("l05a/data/scientists/professions.csv")
dates = read_csv("l05a/data/scientists/dates.csv")
works = read_csv("l05a/data/scientists/works.csv")


## ----echo=FALSE--------------------------------------------------------
professions %>% select(name) %>% kable()


## ----------------------------------------------------------------------
professions


## ----------------------------------------------------------------------
dates


## ----------------------------------------------------------------------
works


## ----echo=FALSE--------------------------------------------------------
professions %>%
  left_join(dates) %>%
  left_join(works)


## ----------------------------------------------------------------------
names(professions)
names(dates)
names(works)


## ----------------------------------------------------------------------
nrow(professions)
nrow(dates)
nrow(works)


## ----eval=FALSE--------------------------------------------------------
## something_join(x, y)


## ----echo=FALSE--------------------------------------------------------
x = tibble(
  id = c(1, 2, 3),
  value_x = c("x1", "x2", "x3")
  )

## ----------------------------------------------------------------------
x


## ----echo=FALSE--------------------------------------------------------
y = tibble(
  id = c(1, 2, 4),
  value_y = c("y1", "y2", "y4")
  )

## ----------------------------------------------------------------------
y


## ----echo=FALSE, out.width="80%", out.extra ='style="background-color: #FDF6E3"'----
include_graphics("l05a/img/left-join.gif")


## ----------------------------------------------------------------------
left_join(x, y)


## ----------------------------------------------------------------------
professions %>%
  left_join(dates) #<<


## ----echo=FALSE, out.width="80%", out.extra ='style="background-color: #FDF6E3"'----
include_graphics("l05a/img/right-join.gif")


## ----------------------------------------------------------------------
right_join(x, y)


## ----------------------------------------------------------------------
professions %>%
  right_join(dates) #<<


## ----echo=FALSE, out.width="80%", out.extra ='style="background-color: #FDF6E3"'----
include_graphics("l05a/img/full-join.gif")


## ----------------------------------------------------------------------
full_join(x, y)


## ----------------------------------------------------------------------
dates %>%
  full_join(works) #<<


## ----echo=FALSE, out.width="80%", out.extra ='style="background-color: #FDF6E3"'----
include_graphics("l05a/img/inner-join.gif")


## ----------------------------------------------------------------------
inner_join(x, y)


## ----------------------------------------------------------------------
dates %>%
  inner_join(works) #<<


## ----echo=FALSE, out.width="80%", out.extra ='style="background-color: #FDF6E3"'----
include_graphics("l05a/img/semi-join.gif")


## ----------------------------------------------------------------------
semi_join(x, y)


## ----------------------------------------------------------------------
dates %>%
  semi_join(works) #<<


## ----echo=FALSE, out.width="80%", out.extra ='style="background-color: #FDF6E3"'----
include_graphics("l05a/img/anti-join.gif")


## ----------------------------------------------------------------------
anti_join(x, y)


## ----------------------------------------------------------------------
dates %>%
  anti_join(works) #<<


## ----------------------------------------------------------------------
professions %>%
  left_join(dates) %>%
  left_join(works)


## ----include=FALSE-----------------------------------------------------
enrollment = read_csv("l05a/data/students/enrolment.csv")
survey = read_csv("l05a/data/students/survey.csv")


## ----message = FALSE---------------------------------------------------
enrollment


## ----message = FALSE---------------------------------------------------
survey


## ----------------------------------------------------------------------
enrollment %>% 
  left_join(survey, by = "id") #<<


## ----------------------------------------------------------------------
enrollment %>% 
  anti_join(survey, by = "id") #<<


## ----------------------------------------------------------------------
survey %>% 
  anti_join(enrollment, by = "id") #<<


## ---- message = TRUE---------------------------------------------------
left_join(enrollment, survey)


## ---- message = TRUE---------------------------------------------------
left_join(enrollment, survey, by = 'id')


## ----------------------------------------------------------------------
survey_rn = survey %>% rename(survey_id = id, first_name = name)
survey_rn


## ----------------------------------------------------------------------
left_join(enrollment, survey_rn, by = c(id = "survey_id"))


## ----------------------------------------------------------------------
right_join(survey_rn, enrollment, by = c(survey_id = "id"))


## ----include=FALSE-----------------------------------------------------
purchases = read_csv("l05a/data/sales/purchases.csv")
prices = read_csv("l05a/data/sales/prices.csv")


## ----message = FALSE---------------------------------------------------
purchases


## ----message = FALSE---------------------------------------------------
prices


## ----------------------------------------------------------------------
purchases %>% 
  left_join(prices) #<<


## ----------------------------------------------------------------------
purchases %>% 
  left_join(prices) %>%
  summarise(total_revenue = sum(price)) #<<


## ----------------------------------------------------------------------
purchases %>% 
  left_join(prices)


## ----------------------------------------------------------------------
purchases %>% 
  left_join(prices) %>%
  group_by(customer_id) %>% #<<
  summarise(total_revenue = sum(price))


## ---- include= FALSE---------------------------------------------------
library(nycflights13)
options(pillar.print_max = 6, pillar.print_min = 6)



## ----------------------------------------------------------------------
airlines


## ----------------------------------------------------------------------
airports


## ----------------------------------------------------------------------
planes


## ----------------------------------------------------------------------
weather


## ----------------------------------------------------------------------
planes %>% 
  count(tailnum) %>% 
  filter(n > 1)

weather %>% 
  count(year, month, day, hour, origin) %>% 
  filter(n > 1)


## ----------------------------------------------------------------------
flights %>% 
  count(year, month, day, flight) %>% 
  filter(n > 1)


## ----------------------------------------------------------------------
flights %>% 
  count(year, month, day, tailnum) %>% 
  filter(n > 1)


## ----------------------------------------------------------------------
flights %>% 
  count(year, month, day, flight, origin, carrier) %>% 
  filter(n > 1)

