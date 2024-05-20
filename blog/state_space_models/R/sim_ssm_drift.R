sim_state_space_d = function(time_points = 100, 
                             slope = 0,
                             sd_xi = 1, 
                             sd_zeta = 1,
                             sd_epsilon = 1,
                             what = 1
){
    ## With Slope
    # tau
    true_values = vector(mode = "numeric", 
                         length = time_points)
    # varying slopes
    nu = vector(mode = "numeric",
                length = time_points)
    
    # beobachteten Werte
    y = vector(mode = "numeric",
               length = time_points)
    
    # sampeln schonmal jetzt
    xi_vec      = rnorm((time_points - 1), mean = 0, sd = sd_xi)
    zeta_vec    = rnorm((time_points - 1), mean = 0, sd = sd_zeta)
    epsilon_vec = rnorm(time_points, mean = 0, sd = sd_epsilon)
    
    # Startwerte
    true_values[1] = 1 # true value 1
    nu[1] = 0
    
    # Erzeugung der wahren werte
    for(i in 2:time_points){
        true_values[i] <- true_values[i-1] + slope + nu[i-1] + xi_vec[i-1]
        nu[i] <- nu[i-1] + zeta_vec[i-1]
    }
    
    # Erzeugung der Beobachtungen
    y = true_values + epsilon_vec

    
    dt = data.table(true_values = true_values,
                    nu = nu,
                    slope = slope,
                    y = y, 
                    xi_vec = c(NA, xi_vec),
                    zeta_vec = c(NA, zeta_vec),
                    epsilon_vec = epsilon_vec,
                    time = 1:time_points)
    
    dt    
}

