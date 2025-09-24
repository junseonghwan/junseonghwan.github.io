Text munging
================
Andrew McDavid
30 Sep 2021

``` r
library(tidyverse)
knitr::opts_chunk$set(error = TRUE)
```

## Text munging

Consider the data in `data/sample_sheet2.csv`.

1.  Read this file in as a .csv.

``` r
data = ...
```

    ## Error in eval(expr, envir, enclos): '...' used in an incorrect context

It should look like this:

![](img/sample_sheet.png)

2.  Split the `folder_name` field by the ’\_’ character. This will give
    you a character `matrix`. Take a look at it with view.

``` r
char_matrix = SOME_COMMAND(data, pattern = SOME_CHARACTER, n = SOME_INTEGER)
```

    ## Error in SOME_COMMAND(data, pattern = SOME_CHARACTER, n = SOME_INTEGER): could not find function "SOME_COMMAND"

3.  You can extract various columns using
    `char_matrix[,integer_of_the_column_you_want]`. Using this and
    `str_c`, recreate the fields `treatment`, `sample_ID`,
    `tissue_source`.

``` r
recreated_data = data %>% select(folder_name) %>% 
  mutate(treatment = SOMETHING,
         sample_ID = str_c(SOMETHING, "something", OTHERTHING),
         tissue_source = SOMETHING)
```

    ## Error in UseMethod("select"): no applicable method for 'select' applied to an object of class "function"

4.  Recreate the field `dataset` by concatenating various fields.

5.  Convert this field to be entirely lowercase.

6.  Collapse sample\_ID field into a single character vector (length 1)
    separated by semicolons “;” using `str_c(..., collapse = ...)`.

It should look like this when you are done:

> 300\_0150; 300\_0150; 300\_0171; 300\_0171; 300\_0173; 300\_0173;
> 300\_0174; 300\_0174; 300\_0392; 300\_0392; 300\_0410; 300\_0410;
> 300\_0414; 300\_0414; 300\_0415; 300\_0416; 300\_1883; 300\_1930;
> 300\_1930; 301\_0174; 301\_0174; 301\_0174; 301\_0270; 301\_0270
