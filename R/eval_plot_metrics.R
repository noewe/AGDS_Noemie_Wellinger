#' Predict and evaluate KNN model
#'
#' @param mod the model
#' @param test the test data
#' @param site_name the name of the second site
#' @param type "within-site" or "across-site" or "Across-site, pooled"
#'
#' @return model metrics table and plot
#'
eval_plot_metrics <- function(pp, train, test, site_name, type){
  set.seed(123)
  mod <- caret::train(pp,
                      data = train |> drop_na(),
                      method = "knn",
                      trControl = caret::trainControl(method = "cv", number = 10),
                      tuneGrid = data.frame(k = c(15, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 42, 44, 46)),
                      metric = "MAE")

  # add predictions to the data frames
  test <- test |>
    drop_na()
  test$fitted <- predict(mod, newdata = test)

  # get metrics tables
  metrics <- test |>
    yardstick::metrics(GPP_NT_VUT_REF, fitted)

  # extract values from metrics tables
  mae <- metrics |>
    filter(.metric == "mae") |>
    pull(.estimate)
  rsq <- metrics |>
    filter(.metric == "rsq") |>
    pull(.estimate)

  #extract the best k
  best_k <- mod$bestTune$k

  # visualise as a scatterplot
  # adding information of metrics as sub-titles
  plot_1 <- ggplot(data = test, aes(GPP_NT_VUT_REF, fitted)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
    labs(subtitle = bquote( italic(R)^2 == .(format(rsq, digits = 2)) ~~
                              MAE == .(format(mae, digits = 3)) ~~
                              k == .(best_k)),
         title = paste(type, ", ", site_name)) +
    theme_classic()

  return(list("plot" = plot_1, "metrics" = metrics))
}
