
# \begin{align}
# y_t &= Z \alpha_t + \varepsilon_t, &\quad \varepsilon_t &\sim N(0, H_t), \\
# \alpha_{t+1} &= T \alpha_t + R \eta_t, &\quad \eta_t &\sim N(0, Q_t), \\
# &&\alpha_1 &\sim N(\alpha_1, P_1),
# \end{align}




# download
dl_polldata <- function(url, file_name) {
  
  full_url <- paste0(url, "/", file_name)
  
  response <- GET(full_url)
  
  if (status_code(response) != 200) {
    stop("Fehler beim Herunterladen der Datei.")
  }
  
  data <- read_csv(content(response, "raw"))
  
  # Konvertiere zu data.table
  data_dt <- as.data.table(data)
  
  return(data_dt)
}

# prettyfy
prettyfy_data <- function(data_dt) {
  # Daten einlesen
  
  # "Not Available" zu NA umwandeln
  data_dt1 = data_dt[, lapply(.SD, function(x){
    if(class(x) == "character"){
      x[x == "Not Available"] <- NA_character_
    }
    x
  })] %>% 
    .[, lapply(.SD, function(x){ # Prozentwerte zu Anteile
      if(any(grepl("%",x))){
        x = as.numeric(gsub("%", "", x))/100
      }
      x
    })] %>% 
    .[, `Sample Size` := as.numeric(`Sample Size`)] %>% 
    .[, lapply(.SD, function(x){
      if(is.character(x)){
        x = factor(x)
      }
      x
    })]
  
  
  data_dt1 %>% 
    setnames(old = c("Polling Firm", "Commissioners", "Fieldwork Start", "Fieldwork End", 
                     "Scope", "Sample Size", "Sample Size Qualification", "Participation", 
                     "Precision", "ÖVP", "SPÖ", "FPÖ", "NEOS", "JETZT", "GRÜNE", 
                     "G!LT", "HC", "MFG", "KPÖ", "BIER", "Other"),
             new = c("Polling_Firm", "Commissioners", "Start", "End", 
                     "Scope", "Samplesize", "Samplesize_Qualification", "Participation", 
                     "Precision", "ÖVP", "SPÖ", "FPÖ", "NEOS", "JETZT", "GRÜNE", 
                     "GILT", "HC", "MFG", "KPÖ", "BIER", "Other"))
  
  
  return(data_dt1)
}

# parteien zusammenlegen
add2_other <- function(dat, parteien2other = c("MFG", "KPÖ", "BIER")){
  
  parteien2otherN = colnames(dat)[colnames(dat) %in% parteien2other]
  
  dat[, lauf := 1:.N]
  dat[, Other := ifelse(is.na(Other), 0, Other)]
  dat[, Other := Other + sum(.SD, na.rm = TRUE), .SDcols = parteien2otherN, by = lauf]
  dat[, lauf := NULL]
  dat1 = dat[,.SD, .SDcols = !parteien2otherN]
  dat1
}

# Funktion, um den nächsten Sonntag zu berechnen
next_sonntag <- function(datum) {
  wochentag <- wday(datum)  # 1 = Sonntag
  
  # Berechnung der Tage bis zum nächsten Sonntag
  days_until_sonntag <- (8 - wochentag) %% 7
  if (days_until_sonntag == 0) {
    return(datum)
  }
  
  # Berechnung der Tage seit dem letzten Sonntag
  tage_seit_letzter_sonntag <- (wochentag - 1) %% 7
  
  # Überprüfen, welcher Sonntag näher ist
  if (tage_seit_letzter_sonntag <= days_until_sonntag) {
    return(datum - tage_seit_letzter_sonntag)
  } else {
    return(datum + days_until_sonntag)
  }
}

# datum unique machen
## das Datum unique machen indem wir greedy einfach immer +1 machen wenn notwendig

add_date_unique <- function(dat){ # Start Datum wird unique gemacht
  
  dat %>% setorder(Start) # ordnen - das jüngste datum zuerst
  
  start_dats = dat$Start
  mod_start_dats = start_dats %>% copy
  durch = 2:length(start_dats)
  
  for(i in durch){
    aktuelles_datum = start_dats[i]
    alle_daten_davor = mod_start_dats[1:(i-1)]
    
    date_duplicate_check = any(aktuelles_datum %in% alle_daten_davor) # wenn irgendeines doppelt ist
    
    if(date_duplicate_check){ 
      
      while(date_duplicate_check){
        aktuelles_datum <- aktuelles_datum + days(1)
        date_duplicate_check = any(aktuelles_datum %in% alle_daten_davor)
      }
    }
    mod_start_dats[i] <- aktuelles_datum
    
    
  }
  
  dat[, Start_unique := mod_start_dats]
  
  dat
  
}

########

prepare_NATdata_4_marss = function(dat){

  # die zeitreihe muss nicht endlos lang sein!
  dat[, end_year := lubridate::year(End)]
  data_rdy23ff = dat[end_year >= 2021, ]
  
  # alle parteien!
  parteinamen = c("ÖVP", "SPÖ", "FPÖ", "NEOS", "JETZT", "GRÜNE", 
                  "GILT", "HC", "MFG", "KPÖ", "BIER", "Other")
  

  data_rdy23ff_red = data_rdy23ff %>% 
    .[Scope == "National"] %>% # nur die nationalen umfragen
    setorder(Start) %>% 
    .[, laufnummer := 1:.N] %>% 
    remove_empty(which = "cols") %>% 
    copy %>% 
    add2_other(parteien2other = c("MFG")) %>% # gruppen zusammenlegen
    .[, next_sundayEND := next_sonntag(End), by = laufnummer] %>% 
    add_date_unique() %>% # data mal professorisch unique machen
    .[is.na(`KPÖ`), KPÖ := 0.0001] %>%  # darf nicht 0 sein fuer transformation!
    .[is.na(`BIER`), BIER := 0.0001]
  
  data_rdy23ff_red2 <- data_rdy23ff_red %>% copy %>% 
    .[is.na(`KPÖ`), KPÖ := 0.0001] %>%  # darf nicht 0 sein fuer transformation!
    .[is.na(`BIER`), BIER := 0.0001]
  
  # die uebriggebliebenen parteien 
  partei_uebrig = intersect(parteinamen, colnames(data_rdy23ff_red2))
  
  # sicherstellen dass sich auch alles auf 100% summiert!
  data_rdy23ff_red2[, lauf := 1:.N]
  data_rdy23ff_red2[, (partei_uebrig) := .SD/sum(.SD), .SDcols = partei_uebrig, by = "lauf"]
  data_rdy23ff_red2[, lauf := NULL]
  
  # is unique?
  stopifnot(nrow(data_rdy23ff_red2) == nrow(unique(data_rdy23ff_red2, by = "Start_unique")))
  
  ## IRL TRANSFORMATION --------------------------------------------------------
  partei_uebrigt = paste0(partei_uebrig, "_transf")
  
  transf_anteile = data_rdy23ff_red2[, ilr(.SD), .SDcols = partei_uebrig]
  colnames(transf_anteile) <- paste0("transf", 1:ncol(transf_anteile))
  
  # transformierte werden drangehängt
  data_rdy23ff_red_transf = cbind(data_rdy23ff_red2, transf_anteile)
  
  # wir brauchen fuer das Modell nur die werte plus datum
  data_poll4join = cbind(data_rdy23ff_red2[, .(Start_unique)], 
                         transf_anteile) %>% 
    setkey(Start_unique)
  
  ### alles vervollständigen:
  
  min_date = data_rdy23ff_red_transf[, min(Start_unique)]
  max_date = data_rdy23ff_red_transf[, max(Start_unique)]
  
  min_to_max = seq(min_date, max_date, by = "day")
  data_full = data.table(time = min_to_max) %>% setkey(time)
  data_full = data_poll4join[data_full] ### das sind die daten mit denen man arbeiten will
  data_fullm = data_full[,-1] %>% as.matrix %>% t
  
  # nochmal zusammenhaengen mit allen daten
  data_fullX = data.table(time = min_to_max) %>% setkey(time)
  data_fullX = data_rdy23ff_red2[data_fullX, on = c("Start_unique" = "time")] 
  data_fullX[, time := 1:.N]
  
  
  list(data4marss = data_fullm,
       data_plot = data_rdy23ff_red,
       data_all = data_rdy23ff_red_transf,
       data_full = data_fullX,
       partei_uebrig = partei_uebrig)
}



