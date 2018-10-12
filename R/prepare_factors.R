#' Preparing data for machine learning
#' 
#' Function to convert rare and new levels in test data
#'
#' @param train Character. Possibly a feature column from a training data set.
#' @param test Character. Possibly a feature column from a test data set.
#' @param rare_count Integer. Levels with less manifestations than \code{rare_count} will get the \code{rare_level}.
#' @param rare_level Character. Specification of the new name for rare levels.
#' @param new_level Character. The level for manifestations that didn't show up in the training data.
#' @param ignore_na Logical. Should \code{NA}s \code{NA}?
#' @param output_type Character ("character", "factor" or "integer"). If \code{"factor"} the output always gets the levels \code{rare_level} and \code{other_level}.
#' @param encoding Character. Specifies the sorting of the levels / integers (when the \code{output_type} is not \code{"character"}.
#' 
#' @return A contatenated vector of `train` and `test` transformed and converted regarding the \code{output_type}.
#' 
#' @examples
#' ### data
#' train <- dplyr::tibble(stay_same  = c('a','a','a','a','a','b','b','a','b','b','b', NA, NA),
#'                        some_rare  = c('c','d','e','e','a','b','b','a','b','b','b', NA, NA),
#'                        some_new   = c('c','d','e','e','a','b','b','a','b','b','b', NA, NA),
#'                        integer    = c( 1L, 2L, 3L, 4L, 6L, 7L, 8L, 9L,10L,11L,12L,13L,14L),
#'                        logical    = c(  T,  F,  T,  F,  T,  F,  T,  F,  T,  F,  T,  F,  T))
#' test  <- dplyr::tibble(stay_same  = c('a','a','a','a','a','b','b','a','b','b','b', NA, NA),
#'                        some_rare  = c('c','c','c','c','c','d','b','a','b','b','b', NA, NA),
#'                        some_new   = c('f','a','a','g','a','b','b','a','b','b','b', NA, NA),
#'                        integer    = c( 1L, 2L, 3L, 4L, 6L, 7L, 8L, 9L,10L,11L,12L,13L,14L),
#'                        logical    = c(  T,  F,  T,  F,  T,  F,  T,  F,  T,  F,  T,  F,  T))
#' ### preparation
#' prepare_factors(train$some_new, test$some_new, output_type = "factor") 
#' 
#'
#' @export
#' 
prepare_factors <- function(train, test = NULL,
                            rare_count = 5L, rare_level = "rare",
                            new_level = "other", ignore_na = TRUE, 
                            output_type = c("character", "factor", "integer"),
                            encoding = NULL, return_mapping = FALSE) {
  # Check arguments
  if (!is.character(train)) stop("`train` must be a character.")
  if (!is.character(test) | is.null(test)) stop("`test` must be a character when it is provided.")
  if (!is.integer(rare_count)) stop("`rare_count` must be an integer.")
  if (!is.character(rare_level)) stop("`rare_level` must be a character.")
  if (!is.character(new_level)) stop("`new_level` must be a character.")
  if (!is.logical(ignore_na)) stop("`ignore_na` must be a logical.")
  output_type <- match.arg(output_type)
  if (!is.null(encoding)) stop("`encoding` is not implemented at the moment-")
  if (!is.logical(return_mapping)) stop("`return_mapping` must be a logical.")
  # Here should be a warning when `new_level` or `rare_level` already occur in the data
  train_test <- c(train, test)
  train_u <- unique(train)
  test_u  <- unique(test)
  train_test_u <- unique(train, test)
  if(new_level %in% train_u)  warning("`new_level` is already a level in `train`")
  if(new_level %in% test_u)  warning("`new_level` is already a level in `test`")
  if(rare_level %in% train_u)  warning("`rare_level` is already a level in `train`")
  if(rare_level %in% test_u)  warning("`rare_level` is already a level in `test`")
  # Count levels in the training data and change levels accordingly (while keeping NAs)
  df_train <- dplyr::tibble(train = train) %>% 
    dplyr::count(train) %>%
    dplyr::rename(count_train = n)
  df_train_test <- dplyr::tibble(train_test = train_test) %>% 
    dplyr::left_join(df_train, by = c("train_test" = "train")) %>% 
    dplyr::mutate(count_train = dplyr::if_else(is.na(count_train), 0L, count_train)) %>% 
    dplyr::mutate(output = dplyr::case_when(
      is.na(train_test)         ~ train_test,
      count_train == 0L         ~ new_level,
      count_train  < rare_count ~ rare_level,
      TRUE                      ~ train_test
    ))
  # convert output
  output <- df_train_test[["output"]]
  if (output_type != "character") {
    output_level <- df_train %>% dplyr::filter(count_train >= rare_count) %>%
      dplyr::pull(train) %>% c(., rare_level, new_level)
    output <- factor(output, ordered = TRUE, levels = output_level)
  }
  
  if(output_type == "integer") {
    output <- as.integer(output) 
  }
  # - possibly add `mapping` attribute
  # return
  output
}
