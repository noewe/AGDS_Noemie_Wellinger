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
                         trControl = caret::trainControl(method = "cv", number = 10),
                         tuneGrid = data.frame(k = c(2, 5, 10, 15, 20, 25, 30, 35, 40, 60, 100)),
                         metric = "MAE")
  
  mod_site2 <- caret::train(pp, 
                         data = site2_train |> drop_na(), 
                         method = "knn",
                         trControl = caret::trainControl(method = "cv", number = 10),
                         tuneGrid = data.frame(k = c(2, 5, 10, 15, 20, 25, 30, 35, 40, 60, 100)),
                         metric = "MAE")
  
  if(out_of_sample == TRUE) {
    # add predictions to the data frames
    # within-site predictions
    site1_test <- site1_test |> 
      drop_na()
    site1_test$fitted_within <- predict(mod_site1, newdata = site1_test)
    
    site2_test <- site2_test |> 
      drop_na()
    site2_test$fitted_within <- predict(mod_site2, newdata = site2_test)
    
    # across-site predictions
    site1_test <- site1_test |> 
      drop_na()
    site1_test$fitted_across <- predict(mod_site2, newdata = site1_test) #predict site 1 with model of site 2
    
    site2_test <- site2_test |> 
      drop_na()
    site2_test$fitted_within <- predict(mod_site1, newdata = site2_test) #predict site 2 with model of site 1
    
    
    # get metrics tables
    metrics1_within <- site1_test |> 
      yardstick::metrics(GPP_NT_VUT_REF, fitted_within)
    
    metrics2_within <- site2_test |> 
      yardstick::metrics(GPP_NT_VUT_REF, fitted_within)
    
    metrics1_across <- site1_test |> 
      yardstick::metrics(GPP_NT_VUT_REF, fitted_across)
    
    metrics2_across <- site2_test |> 
      yardstick::metrics(GPP_NT_VUT_REF, fitted_across)
    
    
    # extract values from metrics tables
    rmse_1_within <- metrics1_within |> 
      filter(.metric == "rmse") |> 
      pull(.estimate)
    rsq_1_within <- metrics1_within |> 
      filter(.metric == "rsq") |> 
      pull(.estimate)
    
    rmse_2_within <- metrics2_within |> 
      filter(.metric == "rmse") |> 
      pull(.estimate)
    rsq_2_within <- metrics2_within |> 
      filter(.metric == "rsq") |> 
      pull(.estimate)
    
    rmse_1_across <- metrics1_across |> 
      filter(.metric == "rmse") |> 
      pull(.estimate)
    rsq_1_across <- metrics1_across |> 
      filter(.metric == "rsq") |> 
      pull(.estimate)
    
    rmse_2_across <- metrics2_across |> 
      filter(.metric == "rmse") |> 
      pull(.estimate)
    rsq_2_across <- metrics2_across |> 
      filter(.metric == "rsq") |> 
      pull(.estimate)
    
    # visualise as a scatterplot
    # adding information of metrics as sub-titles
    plot_1 <- ggplot(data = site1_test, aes(GPP_NT_VUT_REF, fitted_within)) +
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
    
    out <- cowplot::plot_grid(plot_1, plot_2, plot_3, plot_4)
  }
  
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
      geom_line(data=df_train, aes(y=fitted), size=0.3, color="red")+
      geom_line(data=df_test, aes(y=fitted), size=0.3, color="blue")+
      geom_line(data=df_train, aes(y=zoo::rollmean(fitted-GPP_NT_VUT_REF, 30, na.pad=TRUE)),color = "red",size = 0.3) +
      geom_line(data=df_test, aes(y=zoo::rollmean(fitted-GPP_NT_VUT_REF, 30, na.pad=TRUE)),color = "blue",size = 0.3) +
      labs(title = "Observed vs. modelled") +
      geom_hline(yintercept=0, linetype="dashed", 
                 color = "black", linewidth=0.5)+
      theme_classic()
    out <- plot_1
  }
  
  return(out)
}
