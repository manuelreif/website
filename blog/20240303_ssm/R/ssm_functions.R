sim_state_space = function(time_points = 100, 
                           xi = 1, 
                           epsilon = 2,
                           drift = 0){
    
    true_values = vector(mode = "numeric", 
                         length = time_points)
    
    y = vector(mode = "numeric",
               length = time_points)
    
    true_values[1] = 1 # true value 1
    for(i in 2:time_points){
        true_values[i] <- true_values[i-1] + drift + rnorm(1, mean = 0, sd = xi)
    }
    
    y = true_values + rnorm(time_points, mean = 0, sd = epsilon)
    dt = data.table(true_values = true_values, y = y, time = 1:time_points)
    
    dt    
}
