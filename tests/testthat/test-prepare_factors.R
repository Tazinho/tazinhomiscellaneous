context("prepare_factors")

test_that("initial poc",{
  
  train <- tibble::tibble(stay_same  = c('a','a','a','a','a','b','b','a','b','b','b', NA, NA),
                         some_rare  = c('c','d','e','e','a','b','b','a','b','b','b', NA, NA),
                         some_new   = c('c','d','e','e','a','b','b','a','b','b','b', NA, NA),
                         integer    = c( 1L, 2L, 3L, 4L, 6L, 7L, 8L, 9L,10L,11L,12L,13L,14L),
                         logical    = c(  TRUE,  FALSE,  TRUE,  FALSE,  TRUE,  FALSE,  TRUE,  FALSE,  TRUE,  FALSE,  TRUE,  FALSE,  TRUE))
  test  <- tibble::tibble(stay_same  = c('a','a','a','a','a','b','b','a','b','b','b', NA, NA),
                         some_rare  = c('c','c','c','c','c','d','b','a','b','b','b', NA, NA),
                         some_new   = c('f','a','a','g','a','b','b','a','b','b','b', NA, NA),
                         integer    = c( 1L, 2L, 3L, 4L, 6L, 7L, 8L, 9L,10L,11L,12L,13L,14L),
                         logical    = c(  TRUE,  FALSE,  TRUE,  FALSE,  TRUE,  FALSE,  TRUE,  FALSE,  TRUE,  FALSE,  TRUE,  FALSE,  TRUE))
  
  expect_equal(prepare_factors(train$some_new, test$some_new, rare_count = 5L),
               c("rare", "rare", "rare", "rare", "rare", "b", "b", "rare", "b", 
                 "b", "b", NA, NA, "other", "rare", "rare", "other", "rare", "b", 
                 "b", "rare", "b", "b", "b", NA, NA)
  ) 
})

test_that("check that order of integers doesn't change",{
  train <- c(rep("a", 5), rep("b", 2))
  test  <- c(rep("a", 5))
  
  expect_equal(prepare_factors(train, test, output_type = "character", rare_count = 5L),
               c("a", "a", "a", "a", "a", "rare", "rare", "a", "a", "a", "a", "a")
               )
  expect_equal(prepare_factors(train, test, output_type = "factor", rare_count = 5L),
               structure(c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 1L, 1L, 1L, 1L, 1L), 
                         .Label = c("a", "rare", "other"), class = c("ordered", "factor"))
               )
  expect_equal(prepare_factors(train, test, output_type = "integer", rare_count = 5L),
               c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 1L, 1L, 1L, 1L, 1L)
               )
  
  
  train <- c(rep("a", 5), rep("b", 2))
  test  <- c(rep("c", 3), rep("a", 5), rep("b", 5))
  
  expect_equal(prepare_factors(train, test, output_type = "character", rare_count = 5L),
               c("a", "a", "a", "a", "a", "rare", "rare", "other", "other", 
                 "other", "a", "a", "a", "a", "a", "rare", "rare", "rare", "rare", 
                 "rare")
               )
  expect_equal(prepare_factors(train, test, output_type = "factor", rare_count = 5L),
               structure(c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 3L, 3L, 3L, 1L, 1L, 1L, 
                           1L, 1L, 2L, 2L, 2L, 2L, 2L),
                         .Label = c("a", "rare", "other"), class = c("ordered", "factor"))
               )
  expect_equal(prepare_factors(train, test, output_type = "integer", rare_count = 5L),
               c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 3L, 3L, 3L, 1L, 1L, 1L, 1L, 1L, 
                 2L, 2L, 2L, 2L, 2L)
               )
  
  
  expect_equal(prepare_factors(c("a", "a", "a")),
               c("rare", "rare", "rare")
               )
  expect_equal(prepare_factors(c("a", "a", "a", "a", "a")),
               c("a", "a", "a", "a", "a")
               )
  expect_equal(prepare_factors("a", NA_character_),
               c("rare", NA)
               )

})

test_that("Input checks",{

  expect_error(prepare_factors(TRUE),
               "`train` must be a character.",
               fixed = TRUE)
  expect_error(prepare_factors("a", TRUE),
               '`test` must be a character when it is provided.',
               fixed = TRUE)
  expect_error(prepare_factors("a", rare_count = TRUE),
               '`rare_count` must be an integer.',
               fixed = TRUE)
  expect_error(prepare_factors("a", rare_level = TRUE),
               '`rare_level` must be a character.',
               fixed = TRUE)
  expect_error(prepare_factors("a", new_level = TRUE),
               '`new_level` must be a character.',
               fixed = TRUE)
  expect_error(prepare_factors("a", ignore_na = "hat"),
               '`ignore_na` must be a logical.',
               fixed = TRUE)
  expect_error(prepare_factors("a", encoding = "hat"),
               '`encoding` is not implemented at the moment.',
               fixed = TRUE)
  expect_error(prepare_factors("a", return_mapping = "bla"),
               '`return_mapping` must be a logical.',
               fixed = TRUE)
  expect_warning(prepare_factors("other"),
                 '`new_level` is already a level in `train`.',
               fixed = TRUE)
  expect_warning(prepare_factors("a", "other"),
                 '`new_level` is already a level in `test`.',
               fixed = TRUE)
  expect_warning(prepare_factors("rare", "a"),
                 '`rare_level` is already a level in `train`.',
                 fixed = TRUE)
  expect_warning(prepare_factors("a", "rare"),
                 '`rare_level` is already a level in `test`.',    
                 fixed = TRUE)
  }
)

