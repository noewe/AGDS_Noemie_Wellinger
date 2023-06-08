#' Evaluate within and across-site KNN model out-of sample performance for two sites
#'
#' @param pp the recipe
#' @param site1_train the training data of site 1
#' @param site2_train the training data of site 2
#' @param site1_test the testing data of site 1
#' @param site2_test the testing data of site 2
#' @param site1_name the name of the first site
#' @param site2_name the name of the second site
#' @param out_of_sample specifies if models are tested out-of-sample (TRUE)  or on pooled training data from both sites (FALSE)
#'
#' @return model metrics table and plot

eval_model <- function(pp, site1_train, site2_train, site1_test, site2_test, site1_name, site2_name, out_of_sample){

  mod_site1 <- caret::train(pp,
                         data = site1_train |> drop_na(),
                         method = "knn",
                         trControl = caret::trainControl(method = "cv", number = 9),
                         tuneGrid = data.frame(k = c(2, 5, 10, 15, 20, 25, 30, 35, 40, 60, 100)),
                         metric = "MAE")

  mod_site2 <- caret::train(pp,
                         data = site2_train |> drop_na(),
                         method = "knn",
                         trControl = caret::trainControl(method = "cv", number = 9),
                         tuneGrid = data.frame(k = c(2, 5, 10, 15, 20, 25, 30, 35, 40, 60, 100)),
                         metric = "MAE")

  if(out_of_sample == TRUE) {
    # predict site 1 within
    plot_metrics_1_within <- plot_metrics(mod = mod_site1, site1_test, site_name = site1_name, type = "Within-site")

    # predict site 2 within
    plot_metrics_2_within <- plot_metrics(mod = mod_site2, site2_test, site_name = site2_name, type = "Within-site")

    # predict site 1 across
    plot_metrics_1_across <- plot_metrics(mod = mod_site2, site1_test, site_name = site1_name, type = "Across-site")

    # predict site 2 across
    plot_metrics_2_across <- plot_metrics(mod = mod_site1, site2_test, site_name = site2_name, type = "Across-site")

    out <- cowplot::plot_grid(plot_metrics_1_within$plot,
                              plot_metrics_2_within$plot,
                              plot_metrics_1_across$plot,
                              plot_metrics_2_across$plot)
  }
  else{
    # predict site 1 within
    plot_metrics_1_pool <- plot_metrics(mod = mod_site1, site1_test, site_name = site1_name, type = "Pooled training data")

    # predict site 2 within
    plot_metrics_2_pool <- plot_metrics(mod = mod_site2, site2_test, site_name = site2_name, type = "Pooled training data")

    out <- cowplot::plot_grid(plot_metrics_1_within$plot,
                              plot_metrics_2_within$plot)
  }

  return(out)
}
