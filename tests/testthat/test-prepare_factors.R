context("prepare_factors")

test_that("initial poc",{
  
  train <- dplyr::tibble(stay_same  = c('a','a','a','a','a','b','b','a','b','b','b', NA, NA),
                         some_rare  = c('c','d','e','e','a','b','b','a','b','b','b', NA, NA),
                         some_new   = c('c','d','e','e','a','b','b','a','b','b','b', NA, NA),
                         integer    = c( 1L, 2L, 3L, 4L, 6L, 7L, 8L, 9L,10L,11L,12L,13L,14L),
                         logical    = c(  T,  F,  T,  F,  T,  F,  T,  F,  T,  F,  T,  F,  T))
  test  <- dplyr::tibble(stay_same  = c('a','a','a','a','a','b','b','a','b','b','b', NA, NA),
                         some_rare  = c('c','c','c','c','c','d','b','a','b','b','b', NA, NA),
                         some_new   = c('f','a','a','g','a','b','b','a','b','b','b', NA, NA),
                         integer    = c( 1L, 2L, 3L, 4L, 6L, 7L, 8L, 9L,10L,11L,12L,13L,14L),
                         logical    = c(  T,  F,  T,  F,  T,  F,  T,  F,  T,  F,  T,  F,  T))
  
  expect_equal(prepare_factors(train$some_new, test$some_new),
               c("rare", "rare", "rare", "rare", "rare", "b", "b", "rare", "b", 
                 "b", "b", NA, NA, "other", "rare", "rare", "other", "rare", "b", 
                 "b", "rare", "b", "b", "b", NA, NA)
  ) 
})

test_that("check that order of integers doesn't change",{
  train <- c(rep("a", 5), rep("b", 2))
  test  <- c(rep("a", 5))
  
  expect_equal(prepare_factors(train, test, output_type = "character"),
               c("a", "a", "a", "a", "a", "rare", "rare", "a", "a", "a", "a", "a")
               )
  expect_equal(prepare_factors(train, test, output_type = "factor"),
               structure(c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 1L, 1L, 1L, 1L, 1L), 
                         .Label = c("a", "rare", "other"), class = c("ordered", "factor"))
               )
  expect_equal(prepare_factors(train, test, output_type = "integer"),
               c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 1L, 1L, 1L, 1L, 1L)
               )
  
  
  train <- c(rep("a", 5), rep("b", 2))
  test  <- c(rep("c", 3), rep("a", 5), rep("b", 5))
  
  expect_equal(prepare_factors(train, test, output_type = "character"),
               c("a", "a", "a", "a", "a", "rare", "rare", "other", "other", 
                 "other", "a", "a", "a", "a", "a", "rare", "rare", "rare", "rare", 
                 "rare")
               )
  expect_equal(prepare_factors(train, test, output_type = "factor"),
               structure(c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 3L, 3L, 3L, 1L, 1L, 1L, 
                           1L, 1L, 2L, 2L, 2L, 2L, 2L),
                         .Label = c("a", "rare", "other"), class = c("ordered", "factor"))
               )
  expect_equal(prepare_factors(train, test, output_type = "integer"),
               c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 3L, 3L, 3L, 1L, 1L, 1L, 1L, 1L, 
                 2L, 2L, 2L, 2L, 2L)
               )
})
