#' Evaluate model performance and plots it in a chosen way
#'
#' @param mod the model formulation
#' @param df_train the training data
#' @param df_test the testing data
#' @param plots specifies which plots should be shown. "scatter" shows the scatterplots, "temporal" shows the temporal plots
#'
#' @return plots of model performance, as specified by parameter 'plots'

eval_model <- function(mod, df_train, df_test, plots){

  # add predictions to the data frames
  df_train <- df_train |>
    drop_na()
  df_train$fitted <- predict(mod, newdata = df_train)

  df_test <- df_test |>
    drop_na()
  df_test$fitted <- predict(mod, newdata = df_test)

  # get metrics tables
  metrics_train <- df_train |>
    yardstick::metrics(GPP_NT_VUT_REF, fitted)

  metrics_test <- df_test |>
    yardstick::metrics(GPP_NT_VUT_REF, fitted)

  # extract values from metrics tables
  rmse_train <- metrics_train |>
    filter(.metric == "rmse") |>
    pull(.estimate)
  rsq_train <- metrics_train |>
    filter(.metric == "rsq") |>
    pull(.estimate)

  rmse_test <- metrics_test |>
    filter(.metric == "rmse") |>
    pull(.estimate)
  rsq_test <- metrics_test |>
    filter(.metric == "rsq") |>
    pull(.estimate)


  if(plots == "scatter"){
  # visualise as a scatterplot
  # adding information of metrics as sub-titles
  plot_1 <- ggplot(data = df_train, aes(GPP_NT_VUT_REF, fitted)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
    labs(subtitle = bquote( italic(R)^2 == .(format(rsq_train, digits = 2)) ~~
                              RMSE == .(format(rmse_train, digits = 3))),
         title = "Training set") +
    theme_classic()

  plot_2 <- ggplot(data = df_test, aes(GPP_NT_VUT_REF, fitted)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
    labs(subtitle = bquote( italic(R)^2 == .(format(rsq_test, digits = 2)) ~~
                              RMSE == .(format(rmse_test, digits = 3))),
         title = "Test set") +
    theme_classic()

  out <- cowplot::plot_grid(plot_1, plot_2)
  }
  if(plots == "temporal"){
    plot_1 <- ggplot(data = df_train, aes(TIMESTAMP, GPP_NT_VUT_REF)) +
      geom_point(alpha = 0.3) +
      geom_line(data = df_train, aes(y = fitted, color = "Train"), size = 0.3) +
      geom_line(data = df_test, aes(y = fitted, color = "Test"), size = 0.3) +
      geom_line(data = df_train, aes(y = zoo::rollmean(fitted - GPP_NT_VUT_REF, 30, na.pad = TRUE), color = "MAE Train"), size = 0.3) +
      geom_line(data = df_test, aes(y = zoo::rollmean(fitted - GPP_NT_VUT_REF, 30, na.pad = TRUE), color = "MAE Test"), size = 0.3) +
      labs(title = "Observed vs. Modelled",
           color = "Data") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
      theme_classic() +
      scale_color_manual(values = c("Train" = "red", "Test" = "blue", "MAE Train" = "salmon", "MAE Test" = "#339999"))
    out <- plot_1
  }

  return(out)
}
