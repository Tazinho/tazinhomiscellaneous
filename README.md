
# tazinhomiscellaneous

[![Travis-CI Build
Status](https://travis-ci.org/Tazinho/tazinhomiscellaneous.svg?branch=master)](https://travis-ci.org/Tazinho/tazinhomiscellaneous)
[![Coverage
status](https://codecov.io/gh/Tazinho/tazinhomiscellaneous/branch/master/graph/badge.svg)](https://codecov.io/github/Tazinho/tazinhomiscellaneous?branch=master)

## Overview

Miscellaneous helper
    functions.

### Installation

``` r
devtools::install_github("Tazinho/tazinhomiscellaneous")
```

    ## Skipping install of 'tazinhomiscellaneous' from a github remote, the SHA1 (e0a587ca) has not changed since last install.
    ##   Use `force = TRUE` to force installation

## `prepare_factors`

`prepare_factors` prepares character vectors for supervised machine
learning. Rare levels in the training data are marked as rare in the
whole dataset and levels that only occur in the test data are marked as
new. Other packages that use this approach are the **vtreat** and the
**recipes** pkg. However, in a very basic workflow, i.e. just using
factor encoding, this might already suffice.

``` r
library(tazinhomiscellaneous)
library(purrr)
library(dplyr)

train <- tibble(stay_same  = c('a','a','a','a','a','b','b','a','b','b','b', NA, NA),
                some_rare  = c('c','d','e','e','a','b','b','a','b','b','b', NA, NA),
                some_new   = c('c','d','e','e','a','b','b','a','b','b','b', NA, NA),
                integer    = c( 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L,10L,11L,12L,13L),
                logical    = c(  T,  F,  T,  F,  T,  F,  T,  F,  T,  F,  T,  F,  T))
test  <- tibble(stay_same  = c('a','a','a','a','a','b','b','a','b','b','b', NA, NA),
                some_rare  = c('c','c','c','c','c','d','b','a','b','b','b', NA, NA),
                some_new   = c('f','a','a','g','a','b','b','a','b','b','b', NA, NA),
                integer    = c( 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L,10L,11L,12L,13L),
                logical    = c(  T,  F,  T,  F,  T,  F,  T,  F,  T,  F,  T,  F,  T))

train_test <- bind_rows(train, test)

i_train <- 1:nrow(train)

train_test %>% 
  map_if(is.character, ~ prepare_factors(.x[i_train], .x[-i_train])) %>% 
  as_tibble() %>% print(n = nrow(.))
## # A tibble: 26 x 5
##    stay_same some_rare some_new integer logical
##    <chr>     <chr>     <chr>      <int> <lgl>  
##  1 a         rare      rare           1 TRUE   
##  2 a         rare      rare           2 FALSE  
##  3 a         rare      rare           3 TRUE   
##  4 a         rare      rare           4 FALSE  
##  5 a         rare      rare           5 TRUE   
##  6 b         b         b              6 FALSE  
##  7 b         b         b              7 TRUE   
##  8 a         rare      rare           8 FALSE  
##  9 b         b         b              9 TRUE   
## 10 b         b         b             10 FALSE  
## 11 b         b         b             11 TRUE   
## 12 <NA>      <NA>      <NA>          12 FALSE  
## 13 <NA>      <NA>      <NA>          13 TRUE   
## 14 a         rare      other          1 TRUE   
## 15 a         rare      rare           2 FALSE  
## 16 a         rare      rare           3 TRUE   
## 17 a         rare      other          4 FALSE  
## 18 a         rare      rare           5 TRUE   
## 19 b         rare      b              6 FALSE  
## 20 b         b         b              7 TRUE   
## 21 a         rare      rare           8 FALSE  
## 22 b         b         b              9 TRUE   
## 23 b         b         b             10 FALSE  
## 24 b         b         b             11 TRUE   
## 25 <NA>      <NA>      <NA>          12 FALSE  
## 26 <NA>      <NA>      <NA>          13 TRUE
```
