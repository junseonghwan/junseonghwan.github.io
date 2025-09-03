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



## ----packages, echo=FALSE, message=FALSE, warning=FALSE----
library(tidyverse)
library(readxl)
library(skimr)
library(knitr)
library(DT)
library(here)


## ----echo=FALSE, out.width="80%"------------
knitr::include_graphics("l09/img/readr.png")


## ----echo=FALSE, out.width="80%"------------
knitr::include_graphics("l09/img/readxl.png")


## -------------------------------------------
nobel = read_csv(file = "l09/data/nobel.csv")
nobel


## ----cache=TRUE-----------------------------
df = tribble(
  ~x, ~y,
  1,  "a",
  2,  "b",
  3,  "c"
)

write_csv(df, file = "l09/data/df.csv")


## -------------------------------------------
read_csv("l09/data/df.csv")


## ----message=FALSE--------------------------
edibnb_badnames = read_csv("l09/data/edibnb-badnames.csv")
names(edibnb_badnames)


## ----error=TRUE-----------------------------
ggplot(edibnb_badnames, aes(x = Number of bathrooms, y = Price)) +
  geom_point()


## -------------------------------------------
edibnb_col_names =
  read_csv(
    "l09/data/edibnb-badnames.csv",
    col_names = c("id","price", "neighbourhood", #<<
      "accommodates", "bathroom", "bedroom",
      "bed", "review_scores_rating",
      "n_reviews", "url"))
names(edibnb_col_names)


## ----warning=FALSE--------------------------
edibnb_clean_names = read_csv("l09/data/edibnb-badnames.csv") %>%
  janitor::clean_names()

names(edibnb_clean_names)


## ----echo=FALSE, out.width="100%"-----------
knitr::include_graphics("l09/img/df-na.png")


## ----eval=FALSE-----------------------------
## read_csv("l09/data/df-na.csv")


## ----echo=FALSE-----------------------------
read_csv("l09/data/df-na.csv") %>% print(n = 10)


## ----eval=FALSE-----------------------------
## read_csv("l09/data/df-na.csv",
##          na = c("", "NA", ".", "9999", "Not applicable"))


## ----echo=FALSE, out.width="100%"-----------
knitr::include_graphics("l09/img/df-na.png")


## ----echo=FALSE,message=FALSE---------------
read_csv("l09/data/df-na.csv", 
  na = c("", "NA", ".", "9999",
         "Not applicable")) %>% 
  print(n = 10)


## ----eval=FALSE-----------------------------
## read_csv("l09/data/df-na.csv", col_types = list(col_double(),
##                                             col_character(),
##                                             col_character()))


## ----echo=FALSE-----------------------------
read_csv("l09/data/df-na.csv", col_types = list(col_double(), 
                                            col_character(), 
                                            col_character())) %>%
  print(n = 10)


## ----message=TRUE, output.lines=7-----------
data = read_csv("l09/data/df-na.csv")
spec(data)


## ----echo=FALSE-----------------------------
knitr::include_graphics("l09/img/fav-food/fav-food.png")


## -------------------------------------------
fav_food = read_excel("l09/data/favourite-food.xlsx") #<<

fav_food


## ----echo=FALSE-----------------------------
knitr::include_graphics("l09/img/fav-food/fav-food-names.png")


## ----warning=FALSE--------------------------
fav_food = read_excel("l09/data/favourite-food.xlsx") %>%
  janitor::clean_names() #<<

fav_food 


## ----echo=FALSE-----------------------------
knitr::include_graphics("l09/img/fav-food/fav-food-nas.png")


## ----warning=FALSE--------------------------
fav_food = read_excel("l09/data/favourite-food.xlsx",
                       na = c("N/A", "99999")) %>% #<<
  janitor::clean_names()

fav_food 


## ----warning=FALSE--------------------------
fav_food = fav_food %>%
  mutate( #<<
    age = if_else(age == "five", "5", age), #<<
    age = as.numeric(age) #<<
    ) #<<

glimpse(fav_food) 


## ----echo=FALSE-----------------------------
knitr::include_graphics("l09/img/fav-food/fav-food-age.png")


## -------------------------------------------
fav_food %>%
  count(ses)


## ----echo=FALSE-----------------------------
knitr::include_graphics("l09/img/fav-food/fav-food-ses.png")


## ----warning=FALSE--------------------------
fav_food = fav_food %>%
  mutate(ses = fct_relevel(ses, "Low", "Middle", "High")) #<<

fav_food %>%
  count(ses)


## ----warning=FALSE--------------------------
fav_food = read_excel("l09/data/favourite-food.xlsx", na = c("N/A", "99999")) %>%
  janitor::clean_names() %>%
  mutate(
    age = if_else(age == "five", "5", age), 
    age = as.numeric(age),
    ses = fct_relevel(ses, "Low", "Middle", "High")
  )

fav_food


## -------------------------------------------
write_csv(fav_food, file = "l09/data/fav-food-clean.csv")

fav_food_clean = read_csv("l09/data/fav-food-clean.csv")


## -------------------------------------------
fav_food_clean %>%
  count(ses)


## ----eval=FALSE-----------------------------
## readRDS(path)
## saveRDS(x, path)


## -------------------------------------------
saveRDS(fav_food, file = "l09/data/fav-food-clean.rds")

fav_food_clean = readRDS("l09/data/fav-food-clean.rds")

fav_food_clean %>%
  count(ses)


## ----echo=FALSE-----------------------------
sales = read_excel("l09/data/sales.xlsx", skip = 3, col_names = c("id", "n"))
sales


## ----echo=FALSE-----------------------------
sales %>%
  mutate(
    is_brand_name = str_detect(id, "Brand"),
    brand = if_else(is_brand_name, id, NA_character_)
  ) %>%
  fill(brand) %>%
  filter(!is_brand_name) %>%
  select(brand, id, n) %>%
  mutate(
    id = as.numeric(id),
    n = as.numeric(n)
  )

