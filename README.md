
# tazinhomiscellaneous

[![Travis-CI Build
Status](https://travis-ci.org/Tazinho/tazinhomiscellaneous.svg?branch=master)](https://travis-ci.org/Tazinho/tazinhomiscellaneous)
[![Coverage
status](https://codecov.io/gh/Tazinho/tazinhomiscellaneous/branch/master/graph/badge.svg)](https://codecov.io/github/Tazinho/tazinhomiscellaneous?branch=master)

## Overview

Miscellaneous helper functions.

### Installation

``` r
devtools::install_github("Tazinho/tazinhomiscellaneous")
```

## `prepare_factors`

`prepare_factors` prepares character vectors for supervised machine
learning. Rare levels in the training data are marked as rare in the
whole dataset and levels that only occur in the test data are marked as
new. Other packages that use this approach are the **vtreat** and the
**recipes** pkg. However, in a very basic workflow, i.e. just using
factor encoding, this might already suffice.

``` r
library(tazinhomiscellaneous)
library(dplyr)

train <- tibble(stay_same  = c('a','a','a','a','a','b','b','b','b','b'),
                some_rare  = c('a','b','c','d','e', NA, NA, NA, NA, NA),
                some_new   = c('a','b','c','d','e','f','f','f','f','f'),
                only_new   = c('a','a','a','a','a','b','b','b','b','b'),
                integer    = c( 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L,10L),
                logical    = c(  T,  F,  T,  F,  T,  F,  T,  F,  T,  F))
test  <- tibble(stay_same  = c('a','b','a','b', NA),
                some_rare  = c('a','a','a','a','a'),
                some_new   = c('g','h','i','a','f'),
                only_new   = c('c','c','c','c','c'),
                integer    = c( 1L, 2L, 3L, 4L, 5L),
                logical    = c(  T,  F,  T,  F,  T))

train_test <- bind_rows(train, test)

i_train <- 1:nrow(train)

train_test %>% 
  mutate_if(is.character,
            ~ prepare_factors(.x[i_train], .x[-i_train], output_type = "character")) %>% 
  print(n = nrow(.))
## # A tibble: 15 x 6
##    stay_same some_rare some_new only_new integer logical
##    <chr>     <chr>     <chr>    <chr>      <int> <lgl>  
##  1 a         rare      rare     a              1 TRUE   
##  2 a         rare      rare     a              2 FALSE  
##  3 a         rare      rare     a              3 TRUE   
##  4 a         rare      rare     a              4 FALSE  
##  5 a         rare      rare     a              5 TRUE   
##  6 b         <NA>      f        b              6 FALSE  
##  7 b         <NA>      f        b              7 TRUE   
##  8 b         <NA>      f        b              8 FALSE  
##  9 b         <NA>      f        b              9 TRUE   
## 10 b         <NA>      f        b             10 FALSE  
## 11 a         rare      other    other          1 TRUE   
## 12 b         rare      other    other          2 FALSE  
## 13 a         rare      other    other          3 TRUE   
## 14 b         rare      rare     other          4 FALSE  
## 15 <NA>      rare      f        other          5 TRUE

train_test %>% 
  mutate_if(is.character,
            ~ prepare_factors(.x[i_train], .x[-i_train], output_type = "integer")) %>% 
  print(n = nrow(.))
## # A tibble: 15 x 6
##    stay_same some_rare some_new only_new integer logical
##        <int>     <int>    <int>    <int>   <int> <lgl>  
##  1         1         1        2        1       1 TRUE   
##  2         1         1        2        1       2 FALSE  
##  3         1         1        2        1       3 TRUE   
##  4         1         1        2        1       4 FALSE  
##  5         1         1        2        1       5 TRUE   
##  6         2        NA        1        2       6 FALSE  
##  7         2        NA        1        2       7 TRUE   
##  8         2        NA        1        2       8 FALSE  
##  9         2        NA        1        2       9 TRUE   
## 10         2        NA        1        2      10 FALSE  
## 11         1         1        3        4       1 TRUE   
## 12         2         1        3        4       2 FALSE  
## 13         1         1        3        4       3 TRUE   
## 14         2         1        2        4       4 FALSE  
## 15        NA         1        1        4       5 TRUE
```
