
## multi_sim
multi_sim_ssm2 <- function(anz = 1000, ...){
    
    sim_ssm2 = lapply(1:anz, function(x){
        dat1_drift = sim_state_space_d(...)
        dat1_drift[, .(time, y, true_values, dg = x)]
    })
    sim_ssm2_dt = rbindlist(sim_ssm2)
    
    mittlere50prozent = sim_ssm2_dt[, .(q25 = quantile(y, 0.25),
                                        q75 = quantile(y, 0.75)), 
                                    keyby = time] %>% 
        .[, dg := 1]
    

    acf_res = sim_ssm2_dt[, .(acf_values = acf(y, lag.max = 15, plot = FALSE)$acf[, ,1],
                              lag = 0:15), by = "dg"]

    list(daten = sim_ssm2_dt, 
         band = mittlere50prozent,
         acf_res = acf_res,
         pars = list(...))
}


#### STEP PLOT ##################################
multi_sim_plot <- function(MUSI){
    
    duga = MUSI$daten$dg %>% max    
    
    subtxt = "$\\sigma_{\\xi} = [sd_xi];
    \\phantom{x} sigma_{\\epsilon} = [sd_epsilon];
    \\phantom{x}  sigma_{\\zeta} = [sd_zeta] ; 
    \\phantom{x} \\beta = [slope]$"     
    
    subtxt_glued = glue(subtxt, .open = "[", .close = "]", 
                        sd_zeta = MUSI$pars$sd_zeta, 
                        sd_epsilon = MUSI$pars$sd_epsilon, 
                        sd_xi = MUSI$pars$sd_xi,
                        slope = MUSI$pars$slope)
    
    p1 = ggplot(data = MUSI$daten, aes(x = time, y = true_values, group = dg)) + 
        geom_step(color = paletteer_d("trekcolors::starfleet")[2], alpha = 0.1) +
        geom_line(data = MUSI$band, aes(y = q25), linetype = 3) + 
        geom_line(data = MUSI$band, aes(y = q75), linetype = 3) + 
        geom_hline(yintercept = 0, 
                   linetype = 2, 
                   alpha = 0.7, 
                   color = paletteer_d("trekcolors::starfleet")[3]) + 
        scale_x_continuous(expand = c(0, 0.8)) +
        ylab("True Values") + 
        labs(title = glue("{duga} Simulationen"), subtitle = latex2exp::TeX(subtxt_glued)) +
        ssm_theme2() +
        theme(axis.text.x =  element_text(family = "barr", face = "plain"))
    
    p1
    
}


multi_plot_density <- function(MUSI){
    
    dat = MUSI$daten
    
    dat[, tv_shifted := shift(true_values, type = "lead"),by = "dg"]
    
    direc_changes = na.omit(dat) %>% 
        .[, .(next_bigger = sum(true_values > tv_shifted, na.rm = TRUE)/.N), keyby = "dg"]
    
    quant_go = direc_changes[, .(q25 = quantile(next_bigger, 0.25),
                                 median = median(next_bigger),
                                 q75 = quantile(next_bigger, 0.75))]
    

    p = ggplot(data = direc_changes, aes(next_bigger)) + 
        geom_density(fill = paletteer_d("trekcolors::starfleet")[2], 
                     color = paletteer_d("trekcolors::starfleet")[2]) + 
        geom_pointrange(aes(xmin = quant_go[, q25], 
                            xmax = quant_go[, q75], 
                            x = quant_go[, median],
                            y = 0.1), 
                        inherit.aes = FALSE, 
                        color = "white", shape = 1, size = 0.2) + 
        geom_textvline(xintercept = direc_changes[,median(next_bigger)], 
                       label = quant_go[,median] %>% round(3), 
                       linetype = 2, 
                       color = "white", hjust = 0.13, size = 3.6) + 
        scale_x_continuous(latex2exp::TeX("$(\\tau_{t+1} < \\tau_{t})/N$"), 
                           expand = c(0, 0), 
                           breaks = c(0, 0.25, 0.5, 0.75, 1), 
                           labels =  c(0, 0.25, 0.5, 0.75, 1)) +
        scale_y_continuous("", expand = c(0.01, 0)) + 
        coord_cartesian(xlim = c(0,1)) + 
        ssm_theme2()
    
    p
}




multi_plot_acf <- function(MUSI){
    
    dat = MUSI$acf_res
    
    quant_acf = dat[, .(q25 = quantile(acf_values, 0.25),
                        median = median(acf_values),
                        q75 = quantile(acf_values, 0.75)), by = c("lag")]
    
    
    p = ggplot(data = quant_acf, aes(x = lag, xend = lag, y = 0, yend = median)) + 
        geom_crossbar(aes(y = median, 
                          ymin = q25, 
                          ymax = q75), 
                      fill = paletteer_d("trekcolors::starfleet")[2],
                      alpha = 1) + 
        geom_hline(yintercept = 0, linetype = 2) +
        geom_segment(linetype = 3) + 
        scale_x_continuous("Lag") + 
        scale_y_continuous("Korrelation") + 
        ssm_theme2()
    
    p
}







