#' Splits FLUXNET dataset into training and test data
#'
#' @param data the whole dataset
#' @param prop the split ratio, usually 0.7
#'
#' @return list of dataframes: $train $test

split_train_test <- function (data, prop) {
  set.seed(1982)  # for reproducibility
  split <- rsample::initial_split(data, prop = prop, strata = "VPD_F")
  train <- rsample::training(split)
  test <- rsample::testing(split)
  return(list("train" = train, "test" = test))
}