


MARSS_model1 <- function(data_fullm, 
                         Qm = c("diagonal and unequal", "unconstrained", "selfmade1"),
                         Rm = c("diagonal and unequal", "diagonal and equal"),
                         method = c("BFGS", "kem"),
                         v0 = NULL,
                         parteien_uebrig){

  method <- method[1]
  Qm <- Qm[1]
  Rm <- Rm[1]
  
  ### Matritzen fuer MARSS vorbereiten
  
  ## x0 vektor --------------------------------------------------------------------
  lvl_start = data_fullm[,1]
  x_matrix = cbind(lvl_start, 0) %>% t %>% as.vector %>% matrix(ncol = 1)

  if(Qm == "selfmade1"){ # wenn die Q Matrix selbst gestrickt sein soll, dann selbst stricken!
  ## Q Matrix --------------------------------------------------------------------
  Qm = matrix(list(0), 
              nrow = nrow(data_fullm)*2, 
              ncol = nrow(data_fullm)*2)
  
  q_names = paste0(c("lvl", "slp"), rep(1:(nrow(data_fullm)), each = 2))
  
  rownames(Qm) <- colnames(Qm) <- q_names
  
  pos_lvls = grep("lvl", q_names)
  pos_slps = grep("slp", q_names)
  
  level_combis = combinat::combn(pos_lvls,2)
  slope_combis = combinat::combn(pos_slps,2)
  
  Qvariances = paste0("var_", q_names)
  diag(Qm) <- Qvariances
  
  cov_lvl_input = apply(level_combis, 2, function(x){
    akt_cov = paste0("cov_", q_names[x[1]], "_", q_names[x[2]])
    Qm[x[1],x[2]] <<- akt_cov
    Qm[x[2],x[1]] <<- akt_cov
  })
  
  cov_slp_input = apply(slope_combis, 2, function(x){
    akt_cov = paste0("cov_", q_names[x[1]], "_", q_names[x[2]])
    Qm[x[1],x[2]] <<- akt_cov
    Qm[x[2],x[1]] <<- akt_cov
  })
  } # Ende
  
  
  ## B Matrix --------------------------------------------------------------------
  b_block = matrix(c(1,1,0,1), ncol = 2, byrow = TRUE)
  Bm = lapply(1:nrow(data_fullm), function(x) b_block) %>% 
    .bdiag %>% 
    as.matrix
  
  ## Z Matrix --------------------------------------------------------------------
  z_block = matrix(c(1,1), ncol = 2, byrow = TRUE)
  Zm = lapply(1:nrow(data_fullm), function(x) z_block) %>% 
    .bdiag %>% 
    as.matrix
  
  ## R Matrix --------------------------------------------------------------------
  # r_dim = data_fullm %>% nrow
  # Rm = matrix(0, ncol = r_dim, nrow = r_dim)
  #Rm <- "diagonal and unequal"
  Rm <- "diagonal and equal" # vielleicht mal "einfacher" probieren?!
  
  ## V0 Matrix --------------------------------------------------------------------
  if(!is.null(v0)){
  V0m = matrix(v0, 
               nrow(data_fullm)*2, 
               nrow(data_fullm)*2) + 
    diag(1e-4, nrow(data_fullm)*2)
  is.positive.definite(V0m)
  }
  
  
  ### hier wird jetzt der initialzustand nicht geschätzt sondern hart gesetzt!
  ### Matrix Q funktioniert hier nicht richtig wenn ich es wie ich es will initialisiere! 
  
  mod_list <- list(
    x0 = x_matrix, 
    tinitx = 0,  # Anfangszustände beziehen sich auf t = 0
    Q = Qm,
    B = Bm,  # Übergangsmatrix
    Z = Zm,
    U = "zero",  # Mittelwert der Zustandsfehler
    A = "zero",
    R = Rm
  )
  
  if(!is.null(v0)){
    mod_list$V0 <- V0m # anfängliche Unsicherheit
  }
  
  fit1 <- MARSS(data_fullm, model = mod_list, 
                method = method,
                control = list(trace = 1, maxit = 2000))

  fit1pred = marsss_predict(fit1)
  
  return(list(fit1 = fit1, 
              fit1pred = fit1pred))
}






#### PREDICTIONS --------------------



marsss_predict <- function(marss_model, partei_uebrig) {
  
  fit2_predictions <- predict(marss_model, n.ahead = 100)
  fit2_predictions_dc = fit2_predictions$pred %>% 
    data.table %>% 
    dcast(t ~ .rownames, value.var = "estimate")
  tranf_cols = grep("transf", colnames(fit2_predictions_dc), value = TRUE)
  
  results_ratios = fit2_predictions_dc[, .SD, .SDcols = tranf_cols] %>% 
    as.matrix %>% 
    ilrInv() %>% 
    as.matrix %>% 
    apply(2,as.numeric) %>% 
    data.table
  
  setnames(results_ratios, old = colnames(results_ratios), new = partei_uebrig)
  
  results_ratios_m <- results_ratios  %>% 
    .[, time := 1:.N] %>% 
    melt(id.vars = "time")
  
  results_ratios_m
}


##########


q_corr <- function(Q_matrix){ # resultierende Q Matrix
  
  aktQ = data.table(vcov = Q_matrix[,1], 
                    names = row.names(Q_matrix))
  
  # aktQ = data.table(vcov = result_list[[1]]$fit1$par$Q[,1], 
  #                   names = row.names(result_list[[1]]$fit1$par$Q))
  
  Q_var = aktQ[grepl("^var", names), ]
  Q_cov = aktQ[grepl("^cov", names), ]
  
  Q_var[, leslo := ifelse(grepl("lvl",names), "lvl", "slp")]
  Q_var[, term := gsub("^var_(lvl|slp)(\\d)", "\\2", names) %>% as.integer]
  
  Q_cov1 = Q_cov %>% 
    .[, lauf := 1:.N] %>% 
    .[, c("leslo1", "term1", "leslo2", "term2") := {
      
      n1a = strsplit(names, "_")[[1]][-1]
      
      res <- list()
      
      res[[1]] <- regmatches(n1a[1],regexpr("[^0-9]+", n1a[1]))
      res[[2]] <- regmatches(n1a[1],regexpr("[0-9]+", n1a[1])) %>% as.integer
      
      res[[3]] <- regmatches(n1a[2],regexpr("[^0-9]+", n1a[2]))
      res[[4]] <- regmatches(n1a[2],regexpr("[0-9]+", n1a[2])) %>% as.integer
      res
    }, by = lauf]
  
  Q_cov1[Q_var, sd1 := sqrt(i.vcov), on = c("leslo1" = "leslo", "term1" = "term")]
  Q_cov1[Q_var, sd2 := sqrt(i.vcov), on = c("leslo2" = "leslo", "term2" = "term")]
  
  Q_cov1[, corr := vcov / (sd1 * sd2)]
  Q_cov1[, corr_round := round(corr, 4)]
  
  Q_cov1
}


