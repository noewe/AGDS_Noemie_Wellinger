#' Perform stepwise multivariate linear regression
#'
#' @param preds the predictors, dataframe with numeric values
#' @param data the whole dataset
#' @param target_name the name of the target variable
#'
#' @return object of class "lm"

stepwise_regression <- function (preds, data, target_name) {
  #store the predictors in a list of numeric vectors, the variable 
  remaining_candidates <- as.list(preds)
  
  #initialize the vector where the selected predictors will be going
  all_preds_selected <- c()
  
  #initialize AIC values
  AIC_last = 999999 #set AIC big enough, otherwise it stops after the first while loop
  AIC_current = 999999
  
  linmod_current <- lm(1~1)
  
  while(length(remaining_candidates) != 0){
    
    AIC_last <- AIC_current
    r_squared <- data.frame(pred = "init", rsq_values = 0)
    
    linmod_best <- linmod_current
    
    for (pred_candidate in names(remaining_candidates)){
      
      #initialize candidate models
      linmod_candidates <- c(all_preds_selected, pred_candidate)
      
      lm <- lm(formula = as.formula(paste(target_name, " ~", paste(linmod_candidates, collapse = "+"))), 
               data = data)
      
      #extract R-squared
      r_squared <- r_squared |> add_row(pred = pred_candidate, 
                                        rsq_values = summary(lm)$r.squared)
    }
    
    #determine max R-squared and corresponding variable name
    max_rsq_pred <- r_squared$pred[which.max(r_squared$rsq_values)]
    # print(paste("Highest r-squared: ", max_rsq_pred, max(r_squared$rsq_values)))   # use for debugging
    
    #update vector of selected variables
    all_preds_selected <- append(all_preds_selected, max_rsq_pred) #vector of char
    
    #drop selected variable (var_selected) from candidate predictors (remaining_candidates)
    #remove(remaining_candidates[["TA_F"]])
    remaining_candidates <- remaining_candidates[!names(remaining_candidates) %in% max_rsq_pred]
    
    lm_formula <- paste(target_name, " ~", paste(all_preds_selected, collapse = "+"))
    # print(lm_formula)   #use for debugging
    
    #store the current model 
    linmod_current <- lm(formula = as.formula(lm_formula), 
                         data = data)
    
    #determine whether AIC improved
    AIC_current <- AIC(linmod_current)
    # print(paste("AIC current: ", AIC_current))  # use for debugging
    # print(paste("AIC last: ", AIC_last))        # use for debugging
    
    #if not, BREAK, you have find the best linear model
    if(AIC_current > AIC_last){
      break() #this stops the loop
    }
    
  }
  
  print(lm_formula)   #use for debugging
  print(linmod_best)
  return(linmod_best)
}