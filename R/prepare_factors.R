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
#' @param return_mapping Logical. Should a mapping between integers and original characters be returned.
#' 
#' @return A contatenated vector of `train` and `test` transformed and converted regarding the \code{output_type}.
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
  if (!(is.character(test) | is.null(test))) stop("`test` must be a character when it is provided.")
  if (!is.integer(rare_count)) stop("`rare_count` must be an integer.")
  if (!is.character(rare_level)) stop("`rare_level` must be a character.")
  if (!is.character(new_level)) stop("`new_level` must be a character.")
  if (!is.logical(ignore_na)) stop("`ignore_na` must be a logical.")
  output_type <- match.arg(output_type)
  if (!is.null(encoding)) stop("`encoding` is not implemented at the moment.")
  if (!is.logical(return_mapping)) stop("`return_mapping` must be a logical.")
  # Here should be a warning when `new_level` or `rare_level` already occur in the data
  train_test <- c(train, test)
  train_u <- unique(train)
  test_u  <- unique(test)
  if (new_level %in% train_u)  warning("`new_level` is already a level in `train`.")
  if (new_level %in% test_u)  warning("`new_level` is already a level in `test`.")
  if (rare_level %in% train_u)  warning("`rare_level` is already a level in `train`.")
  if (rare_level %in% test_u)  warning("`rare_level` is already a level in `test`.")
  # Count levels in the training data and change levels accordingly (while keeping NAs)
  df_train <- dplyr::tibble(train = train) %>% 
    dplyr::count(train) 
  
  n <- dplyr::tibble(train_test = train_test) %>% 
    dplyr::left_join(df_train, by = c("train_test" = "train")) %>% dplyr::pull(n)
  n[is.na(n)] <- 0L
  
  output <- train_test
  output[!is.na(output) & n < rare_count] <- rare_level
  output[!is.na(output) & n == 0L] <- new_level
  
  # convert output
  if (output_type != "character") {
    output_level <- c(df_train[df_train[["n"]] >= rare_count, ][["train"]], rare_level, new_level)
    output <- factor(output, ordered = TRUE, levels = output_level)
  }
  
  if (output_type == "integer") {
    output <- as.integer(output) 
  }
  # - possibly add `mapping` attribute
  # return
  output
}
