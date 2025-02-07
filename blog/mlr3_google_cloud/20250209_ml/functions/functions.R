twoD_parameter_gra = function(ERG_LIST, add_points = FALSE){
    
    at = ERG_LIST$at %>% copy
    archive = at$archive$data
    
    fit <- Tps(archive[, c("rf.mtry.ratio", "rf.min.node.size")], archive$classif.auc)
    
    x_range <- seq(min(archive$rf.mtry.ratio), max(archive$rf.mtry.ratio), length.out = 100)
    y_range <- seq(min(archive$rf.min.node.size), max(archive$rf.min.node.size), length.out = 100)
    grid1 <- expand.grid(x = x_range, y = y_range)
    grid1$auc <- predict(fit, grid1)
    
    best_point = archive[classif.auc == max(classif.auc),]
    
    p = ggplot(grid1, aes(x = x, y = y)) +
        geom_tile(aes(fill = auc)) +
        stat_contour(aes(z = auc), color = "white") +
        geom_point(data = best_point,
                   aes(x = rf.mtry.ratio,
                       y = rf.min.node.size),
                   size = 1,
                   color = "red", shape = 4) +
        scale_fill_viridis_c() +
        scale_x_continuous(expand = c(0,0)) +
        scale_y_continuous(expand = c(0,0)) +
        labs(title = "Hyperparameter Tuning Ergebnisse",
             x = "mtry.ratio",
             y = "min.node.size",
             fill = "AUC") +
        theme_bw()    
    
    
    if(add_points){
        p = p + geom_point(data = archive,
                           aes(x = rf.mtry.ratio,
                               y = rf.min.node.size), 
                           shape = 1)
        
        
    }
    
    return(p) 
}

create_stroke_task = function(file = "data/healthcare-dataset-stroke-data.csv"){
    
    d = fread(file) %>% 
        .[, bmi := as.numeric(ifelse(bmi == "N/A", NA_character_, bmi))] %>% 
        .[, id := as.character(id)]  %>% 
        .[gender != "Other",]
    
    # id muss ein character sein, sonst bekomme ich unten bei der rollenzuteilung einen error
    # denn ein "name" spalte muss character oder factor sein, aber nie integer - whyever
    
    
    #### TASK ######################################################################
    
    # es ist etwas kompliziert eine variable den feature status zu entziehen.
    task_stroke = as_task_classif(d, target = "stroke") #<4>
    task_stroke$col_roles$feature <- setdiff(task_stroke$col_roles$feature, "id")
    task_stroke$col_roles$name <- "id"
    task_stroke$set_col_roles("stroke", c("target","stratum")) #<5>
    
    task_stroke
}

create3profiles = function(explainer, variables, groups = NULL, width = 22, height = 10){
    r1p = model_profile(explainer, variables = variables, 
                              N = 200, center = FALSE, type = "partial", groups = groups)
    
    p = plot(r1p, variables = variables, geom = "profiles")
    filename = paste0("gra/r1p_",variables,"_",groups,".png")
    ggsave(plot = p, filename = filename, units = "cm", width=width, height=height)
    
    
    r2c = model_profile(explainer, variables = variables, 
                              N = 200, center = FALSE, type = "conditional", groups = groups)
    
    p = plot(r2c, variables = variables, geom = "profiles")
    filename = paste0("gra/r2c_",variables,"_",groups,".png")
    ggsave(plot = p, filename = filename, units = "cm", width=width, height=height)
    
    
    r3a = model_profile(explainer, variables = variables, 
                              N = 200, center = FALSE, type = "accumulated", groups = groups)
    
    
    p = plot(r3a, variables = variables, geom = "profiles")
    filename = paste0("gra/r3a_",variables,"_",groups,".png")
    ggsave(plot = p, filename = filename, units = "cm", width=width, height=height)
    
    
    list(r1p=r1p, r2c=r2c, r3a=r3a)
    
}


create_profile_rds <- function(save = TRUE, width=22, height=10){
    
    stroke_task = readRDS("data/stroke_task1.rds")
    training_data = stroke_task$data(rows = erg_list_mbo$split$train)
    
    explnr_rf = DALEXtra::explain_mlr3(model = erg_list_mbo$at$learner, 
                                       data = training_data,
                                       y = as.numeric(as.character(training_data$stroke)))
    res_list <- list()
    cat("agg1\n")
    res_list$age1 = create3profiles(explnr_rf, variables = "age", width=width, height=height)
    cat("agg2\n")
    res_list$age2 = create3profiles(explnr_rf, variables = "age", groups = "heart_disease", width=width, height=height)
    cat("agg3\n")
    res_list$gluc = create3profiles(explnr_rf, variables = "avg_glucose_level", groups = "heart_disease", width=width, height=height)
    
    if(save){
        saveRDS(res_list, file = "data/partial_plots_rf.rds")
    }
    
    return(res_list)
    
}

