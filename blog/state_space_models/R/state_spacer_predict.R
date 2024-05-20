create_predict_data <- function(statespacer_object, 
                                forecast_period = 25, 
                                nsim = 500){
    
    
    statespacer_object$filtered$  
    
    
spacer_pred = predict(spacer_res, 
                      forecast_period = forecast_period, 
                      nsim = nsim)


pred_dt = apply(spacer_pred$sim$y, 1, 
                function(x) quantile(x, c(0.05, 0.5, 0.95))) %>% 
    t %>% 
    data.table %>% 
    setnames(new = c("q005", "q05", "q095")) %>% 
    .[, point_estimate_y := spacer_pred$y_fc] %>% 
    .[,time := 1:.N]
    



    
}

