## ----child = "setup.Rmd"-----

## ----setup, include=FALSE----
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



## ----include=FALSE-----------
library(nycflights13)
library(tidyverse)
knitr::opts_chunk$set(cache = FALSE, warning = FALSE)
theme_set(theme_minimal())


## ----tabulate-delay----------
delay_stats = flights %>%  
  filter(month %in% c(3, 6)) %>%
  mutate(day = factor(day), 
         hour = factor(hour, levels = 0:23), 
         origin = factor(origin), 
         month= factor(month)) %>%
  group_by(day, hour, origin, month) %>% 
  mutate(n = n(), pos_delay =  pmax(0, dep_delay)) %>%
  group_by(day, hour, origin, month, .drop = FALSE) %>% 
  summarize(n = n(), 
            total_delay = ifelse(n>0, sum(pos_delay, na.rm = TRUE), NA_real_),
            total_delay_5 = sum(dep_delay > 5, na.rm = TRUE),
            prop_delay_5 = total_delay_5/n,
            mean_delay = total_delay/n)%>%
  group_by(hour, origin, month) %>%
  mutate(residual_mean_delay = mean_delay - mean(mean_delay, na.rm = TRUE),
         residual_total_delay = total_delay - mean(total_delay, na.rm = TRUE))

delay_june = filter(delay_stats, month == "6") %>% 
  select(day:origin, total_delay)


## ----------------------------
delay_june %>% slice_max(n = 1, order_by = total_delay)


## ---- warning=FALSE----------
dstat = ggplot(delay_june, aes(x = as.numeric(hour), y = total_delay, color = origin)) + 
  geom_point() + geom_smooth() 
dstat


## ---- warning = FALSE--------
delay_june = delay_june %>% 
  group_by(hour, origin, month) %>%
  mutate(residual_total_delay = total_delay - mean(total_delay, na.rm = TRUE))
(dstat %+% delay_june) + aes(y = residual_total_delay)


## ----------------------------
delay_wide = delay_june %>% 
  pivot_wider(id_cols = day:hour, names_from = origin, values_from = residual_total_delay) %>% ungroup()

delay_wide %>% slice(10:15)


## ----------------------------
airport_delay_plot = ggplot(delay_wide, aes(x = EWR, y = JFK)) + 
  geom_point(aes(color = as.numeric(hour))) + geom_smooth() + 
  scale_color_viridis_c() 
airport_delay_plot + labs(title = "Resid. delay (minutes) on each hour")


## ----delay-cow, fig.show='hide', results = 'hide', warning=FALSE, message=FALSE----
airport_delay_plot_simple = airport_delay_plot + guides(color = 'none')
cowplot::plot_grid(
  airport_delay_plot_simple + aes(x = JFK, y = dplyr::lag(JFK, n = 1)),
   airport_delay_plot_simple + aes(x = JFK, y = dplyr::lag(JFK, n = 2)),
  airport_delay_plot_simple + aes(x = JFK, y = dplyr::lag(JFK, n = 4)),
  airport_delay_plot_simple + aes(x = JFK, y = dplyr::lag(JFK, n = 8)))
  


## ---- echo = FALSE, ref.label="delay-cow",  warning=FALSE, message=FALSE, fig.asp = 1, out.width = '500px'----


## ---- R.options=list(digits = 2)----
library(reshape2)
delay_matrix = acast(delay_june, day + hour ~ origin, value.var = 'residual_total_delay')
delay_matrix


## ---- R.options=list(digits = 2, width = 50)----
delay_matrix2 = delay_wide %>% select(EWR:LGA) %>% as.matrix
delay_matrix2


## ----------------------------
cor(delay_matrix) 


## ---- R.options=list(digits = 2)----
cor(delay_matrix, use = 'complete')


## ---- error = TRUE-----------
delay_matrix$EWR


## ---- output.lines = 4-------
delay_matrix[,'EWR'] # or delay_matrix[,1]


## ----------------------------
## 10th row, 9am
delay_matrix[10,]
## row titled "10_14", June 10, 2PM.
delay_matrix["10_14",]
## exclude first 7 rows
delay_matrix[-(1:7),]


## ---- R.options=list(digits = 2)----
cor(delay_matrix[-nrow(delay_matrix),], 
    delay_matrix[-1,],
    use = 'pairwise')


## ---- R.options=list(digits = 2)----
cor(delay_matrix[1:(nrow(delay_matrix)-2),],
    delay_matrix[-(1:2),],
    use = 'pairwise')


## ---- fig.width = 6----------
acf_arr = acf(delay_matrix, na.action = na.pass, lag.max = 12, plot = FALSE)
plot(acf_arr)


## ----start_indexing----------
x = 3 * 4
x
length(x)


## ----------------------------
x[2] = 100
x


## ----------------------------
x[5] = 3
x
x[11]
x[0]


## ---- R.options=list(digits = 3, scipen = 100)----
set.seed(2021)
rnorm(5, mean = 10^(1:5))
rnorm(5, sd = 10^(1:5))


## ----------------------------
x = c(7, 8, 10, 20)
y = c(-7, -8, -10, -20)
x + y
x * y


## ----------------------------
x > 9


## ----------------------------
(x > 9) & (x < 20)


## ----------------------------
x == -y
identical(x, -y)
u = c(0.5-0.3,0.3-0.1)
v = c(0.3-0.1,0.5-0.3)
identical(u,v)
identical(u,v[2:1])


## ----------------------------
all.equal(u, v)
all.equal(u,v, tolerance = 0)
near(u,v)


## ----warning=TRUE------------
(y = 1:3)
(z = 3:7)
y + z


## ----warning=TRUE------------
(y = 1:10)
(z = 3:7)
y + z


## ----------------------------
z + 1


## ----------------------------
str(c("hello", "world"))
str(c(1:3, 100, 150))


## ----------------------------
n = 8
set.seed(1)
(w = round(rnorm(n), 2)) # numeric floating point
(x = 1:n) # numeric integer
(y = LETTERS[1:n]) # character
(z = runif(n) > 0.3) # logical


## ----------------------------
w
names(w) = letters[seq_along(w)]
w
w[c('a', 'b', 'd')]


## ----------------------------
w < 0
which(w < 0)
w[w < 0]


## ----------------------------
seq(from = 1, to = length(w), by = 2)
w[seq(from = 1, to = length(w), by = 2)]
w[-c(2, 5)]
w[c('c', 'a', 'f')]


## ----------------------------
## earlier: a = c("cabbage", pi, TRUE, 4.3)
(a = list("cabbage", pi, TRUE, c(4.3,3,2.1,10)))


## ----------------------------
names(a)
names(a) = c("veg", "dessert", "my_aim", "number")
a


## ----------------------------
a = list(veg = "cabbage", dessert = pi, my_aim = TRUE, numbers = c(4.3,10))
a


## ----------------------------
(a = list(veg = c("cabbage", "eggplant"),
           t_num = c(pi, exp(1), sqrt(2)),
           my_aim = TRUE,
           joe_num = 2:6))


## ----------------------------
a[[2]] # index with a positive integer
a$my_aim # use dollar sign and element name
a[["t_num"]] # index with length 1 character vector


## ----------------------------
i_want_this = "joe_num" # indexing with length 1 character object
a[[i_want_this]] # we get joe_num itself, a length 5 integer vector


## ---- error = TRUE-----------
a[[c("joe_num", "veg")]] 


## ----------------------------
names(a)
str(a[c("t_num", "veg")]) # returns list of length 2
str(a["veg"])# returns list of length 1
length(a["veg"][[1]]) # contrast with length of the veg vector itself


## ---- tidy = FALSE-----------
n = 8
(j_dat = tibble(w = rnorm(n),
                x = 1:n,
                y = LETTERS[1:n],
                z = runif(n) > 0.3))


## ---- tidy = FALSE-----------
is.list(j_dat) # data.frames are lists
j_dat[[4]] # this works but I prefer ...
j_dat$z # using dollar sign and name, when possible
namez=c("z")
j_dat[[namez]] # using a character vector of names
#namez=c("z","w")
#j_dat[[namez]] # does not work: Error


## ---- tidy = FALSE-----------
str(j_dat[c("x", "z")]) # get multiple variables
select(j_dat, x, z) # better in interactive work
identical(select(j_dat, x, z), j_dat[c("x", "z")])


## ----------------------------
## don't worry if the construction of this matrix confuses you; just focus on
## the product
j_mat = outer(as.character(1:4), as.character(1:4),
              function(x, y) {
                paste0('x', x, y)
                })
j_mat


## ----------------------------
str(j_mat)


## ----------------------------
dim(j_mat)
length(j_mat)
nrow(j_mat)
ncol(j_mat)


## ----------------------------
rownames(j_mat)
rownames(j_mat) = str_c("row", seq_len(nrow(j_mat)))
colnames(j_mat) = str_c("col", seq_len(ncol(j_mat)))
dimnames(j_mat) # also useful for assignment
j_mat


## ----------------------------
j_mat[2, 3]
j_mat[2, ] # getting row 2
is.vector(j_mat[2, ]) # we get row 2 as an atomic vector
j_mat[ , 3, drop = FALSE] # getting column 3
dim(j_mat[ , 3, drop = FALSE]) # we get column 3 as a 4 x 1 matrix



## ----------------------------
j_mat[c("row1", "row4"), c("col2", "col3")]
j_mat[-c(2, 3), c(TRUE, TRUE, FALSE, FALSE)] # wacky but possible


## ---- echo = FALSE, fig.width=4----
knitr::include_graphics('l11/img/major-order.png') 


## ----------------------------
j_mat[7]
j_mat


## ----------------------------
j_mat["row1", 2:3] = c("HEY!", "THIS IS NUTS!")
j_mat


## ----------------------------
(norm_mat = matrix(rnorm(6), nrow = 3))
rowMeans(norm_mat)


## ----------------------------
(center_mat = norm_mat - rowMeans(norm_mat))


## ----------------------------
matrix(1:15, nrow = 5)
matrix(1:15, nrow = 5, byrow = TRUE)


## ----------------------------
matrix(c("yo!", "foo?"), nrow = 3, ncol = 4)


## ----------------------------
matrix(1:15, nrow = 5,
       dimnames = list(paste0("row", 1:5),
                       paste0("col", 1:3)))


## ----------------------------
vec1 = 5:1
vec2 = 2^(1:5)
cbind(vec1, vec2)


## ----------------------------
rbind(vec1, vec2)


## ---- tidy = FALSE-----------
(vecDat = tibble(vec1 = 5:1,
                vec2 = 2^(1:5)))
vecMat = as.matrix(vecDat)
str(vecMat)


## ---- tidy = FALSE-----------
multiDat = tibble(vec1 = 5:1,
                  vec2 = paste0("hi", 1:5))
(multiMat = as.matrix(multiDat))


## ----------------------------
(six_sevens = matrix(rep(7,6), ncol=3))
(z_mat = matrix(c(40,1,60,3), nrow=2))
z_mat %*% six_sevens # [2x2] * [2x3]


## ----------------------------
rowSums(z_mat)
apply(z_mat, 1, sum)


## ----------------------------
diag(z_mat)


## ----------------------------
diag(z_mat) = c(35,4)
z_mat


## ----------------------------
diag(c(3,4))
diag(2)


## ----------------------------
t(z_mat)


## ----------------------------
det(z_mat)


## ----------------------------
solve(z_mat)
z_mat %*% solve(z_mat)


## ---- output.lines = 6-------
j_dat
j_dat$z
i_want_this = "z" 
(j_dat[[i_want_this]]) # atomic


## ---- output.lines = 6-------
j_dat["y"]
i_want_this = c("w", "z")
j_dat[i_want_this] # index with a vector of variable names


## ----end_indexing, tidy = FALSE,  output.lines = 6----
j_dat[ , "z"] #For tibbles, drop = FALSE by default.
j_dat[ , "z", drop = TRUE]


## ---- tidy = FALSE,  output.lines = 6----
j_dat[c(2, 4, 7), c(1, 4)] # awful and arbitrary but syntax works
j_dat[j_dat$z, ]


## ---- ref.label='tabulate-delay', echo = TRUE, eval = FALSE----
## NA

