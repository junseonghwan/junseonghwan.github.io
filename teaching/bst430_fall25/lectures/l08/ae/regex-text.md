Regular expressions
================
Andrew McDavid
30 Sep 2021

``` r
library(tidyverse)
knitr::opts_chunk$set(error = TRUE)
```

## Words

We’ll use the dataset `words`, which is loaded automatically when you
load the `stringr` package. It contains 980.

``` r
word_df = tibble(word = words)
```

1.  How many words contain an “x” anywhere in them? List them.

``` r
filter(word_df, str_detect(word, PATTERN))
```

    ## Error: Problem with `filter()` input `..1`.
    ## ℹ Input `..1` is `str_detect(word, PATTERN)`.
    ## x object 'PATTERN' not found

2.  How many words end in “x”? List them. Use `$` to match the end of
    the string.

3.  Do any words start with “x”? Use `^` to match the start of the
    string.

4.  Using wildcards `.` and quantifiers `+` (rather than the results of
    the previous exercises), find all the words that contain “x” in the
    interior (but not at the start or end). Check that the number of
    results from 1-4 are coherent.

``` r
filter(word_df, str_detect(word, '.+x.+'))
```

    ## # A tibble: 13 × 1
    ##    word      
    ##    <chr>     
    ##  1 exact     
    ##  2 example   
    ##  3 except    
    ##  4 excuse    
    ##  5 exercise  
    ##  6 exist     
    ##  7 expect    
    ##  8 expense   
    ##  9 experience
    ## 10 explain   
    ## 11 express   
    ## 12 extra     
    ## 13 next

5.  On average, how many vowels are there per word? (Hint: use
    `str_count` and `[]` to define a character class). What is the
    average vowel-per-letter (\# of vowels normalized per length)

6.  List all the words with three or more vowels in a row. Use
    `{min_matches,max_matches}` as a quantifier.

## Sentences

Now, consider the in the sentences dataset:

``` r
sentence_df = tibble(sentence  = sentences)
```

7.  Extract the first word from each sentence. Hint: negate the space
    character class “\[ \]”.

8.  Return all the sentences that contain the colors “red” “blue” or
    “green”. Use the `|` disjunction.

9.  Extract the first word ending in “s”. Use a capture group `()`,
    `str_match()` and the `[:alnum:]` character class.

``` r
sentence_df = sentence_df %>% mutate(second = str_match(sentence, "([[:alnum:]]*s)\\b")[,2])
```

10. (Stretch goal) Notice that two questions ago, we also matched the
    sentence

> The colt reared and threw the tall rider.

because “reared” contains “red”. Fix the regular expression so it only
matches the complete words, not just a fragment using the “&lt;” word
start marker. Hint: use <code>r“(…)”</code> to construct a “raw” string
– this protects the backslash from being used as an escape character.
