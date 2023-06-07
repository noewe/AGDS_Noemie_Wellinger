#' Fits multiple KNN models on different k values and summarizes and visualizes the metrics
#'
#' @param recipe the model recipe
#' @param train the training data
#' @param k_vals the values of k in a numeric vector
#'
#' @return 

knn_tune_K <- function(recipe, train, test, k_vals){

  # Create empty vectors to store the evaluation metrics
  mae_train <- c()
  mae_test <- c()
  
  rmse_train <- c()
  rmse_test <- c()
  
  rsq_train <- c()
  rsq_test <- c()

  bestK <- 0
  lowest_mae = 1000000
  highest_R2 = 0
  
  for(i in k_vals){
    # Fit KNN models
    mod_knn <- caret::train(
      recipe, 
      train |> drop_na(), 
      method = "knn",
      trControl = caret::trainControl(method = "none"),
      tuneGrid = data.frame(k = i),
      metric = "RMSE")
    #print(i)
    
    # add predictions to the data frames
    df_train <- train |> 
      drop_na()
    df_train$fitted <- predict(mod_knn, newdata = df_train)
    
    df_test <- test |> 
      drop_na()
    df_test$fitted <- predict(mod_knn, newdata = df_test)
    
    # get metrics tables
    metrics_train <- df_train |> 
      yardstick::metrics(GPP_NT_VUT_REF, fitted)
    
    metrics_test <- df_test |> 
      yardstick::metrics(GPP_NT_VUT_REF, fitted)
    
    # extract values from metrics tables
    rmse_i_train <- metrics_train |> 
      filter(.metric == "rmse") |> 
      pull(.estimate)
    rmse_train <- c(rmse_train, rmse_i_train)
    
    rsq_i_train <- metrics_train |> 
      filter(.metric == "rsq") |> 
      pull(.estimate)
    rsq_train <- c(rsq_train, rsq_i_train)
    
    mae_i_train <- metrics_train |> 
      filter(.metric == "mae") |> 
      pull(.estimate)
    mae_train <- c(mae_train, mae_i_train)
    
    rmse_i_test <- metrics_test |> 
      filter(.metric == "rmse") |> 
      pull(.estimate)
    rmse_test <- c(rmse_test, rmse_i_test)
    
    rsq_i_test <- metrics_test |> 
      filter(.metric == "rsq") |> 
      pull(.estimate)
    rsq_test <- c(rsq_test, rsq_i_test)
    
    mae_i_test <- metrics_test |> 
      filter(.metric == "mae") |> 
      pull(.estimate)
    mae_test <- c(mae_test, mae_i_test)
    
    if(mae_i_test < lowest_mae && rsq_i_test > highest_R2){
      lowest_mae <- mae_i_test
      highest_R2 <- rsq_i_test
      bestK <- i
    }
    
  }


  # Create a data frame with the evaluation metrics
  metrics_table <- data.frame(k = k_vals, 
                              mae_train = mae_train, mae_test = mae_test, 
                              rmse_train = rmse_train, rmse_test = rmse_test, 
                              rsq_train = rsq_train, rsq_test = rsq_test)
  
  # Plot evaluation metrics against k
  library(ggplot2)
  plot <- ggplot(metrics_table, aes(x = k)) +
    geom_line(aes(y = mae_train, color = "MAE train", linetype = "MAE train")) +
    geom_line(aes(y = mae_test, color = "MAE test", linetype = "MAE test")) +
    geom_line(aes(y = rmse_train, color = "RMSE train", linetype = "RMSE train")) +
    geom_line(aes(y = rmse_test, color = "RMSE test", linetype = "RMSE test")) +
    geom_line(aes(y = rsq_train, color = "R^2 train", linetype = "R^2 train")) +
    geom_line(aes(y = rsq_test, color = "R^2 test", linetype = "R^2 test")) +
    labs(x = "k", y = "Evaluation Metric", color = "Metric", linetype = "Metric") +
    scale_color_manual(values = c("MAE train" = "red", "MAE test" = "#993377", 
                                  "RMSE train" = "green", "RMSE test" = "#116611", 
                                  "R^2 train" = "blue", "R^2 test" = "#223377")) +
    scale_linetype_manual(values = c("MAE train" = "dashed", "MAE test" = "solid",
                                     "RMSE train" = "dashed", "RMSE test" = "solid",
                                     "R^2 train" = "dashed", "R^2 test" = "solid")) +
    theme_classic()
  
  return(list("plot" = plot, "table" = metrics_table, "bestK" = bestK))
}