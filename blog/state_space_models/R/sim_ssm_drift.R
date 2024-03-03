sim_state_space_d = function(time_points = 100, 
                           xi = 1, 
                           epsilon = 2,
                           theta = 1.5){
    
    # tau
    true_values = vector(mode = "numeric", 
                         length = time_points)
    # varying slopes
    nu = vector(mode = "numeric",
                length = time_points)
    
    # beobachteten Werte
    y = vector(mode = "numeric",
               length = time_points)
    
    # startwerte
    true_values[1] = 1 # true value 1
    nu[1] = 0
    
    # erzeugung der wahren werte
    for(i in 2:time_points){
        true_values[i] <- true_values[i-1] + nu[i-1] + rnorm(1, mean = 0, sd = xi)
        nu[i] <- nu[i-1] + rnorm(1, mean = 0, sd = theta)
    }
    
    # erzeugung der Beobachtungen
    y = true_values + rnorm(time_points, 
                            mean = 0, 
                            sd = epsilon)
    
    dt = data.table(true_values = true_values,
                    nu = nu,
                    y = y, 
                    time = 1:time_points)
    
    dt    
}
