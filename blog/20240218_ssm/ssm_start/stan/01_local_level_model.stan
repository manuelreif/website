//--> STAN: local_level_model
data {
  int<lower=0> N; // Anzahl der Beobachtungen
  vector[N] y; // Beobachtungen
  vector[N] time; // Zeitpunkte
}

parameters {
  vector[N] tau; // latenter Status 
  real<lower=0> xi; // Sd - je nachdem wie sich tau 'bewegt'
  real<lower=0> epsilon; // Error Term für die Obs
}

model {
  tau[1] ~ normal(0, 10); // Prior für ersten latenten Status
  
  // state equation
  for (n in 2:N)
    tau[n] ~ normal(tau[n-1], xi);
  
  // measurement equations
  for (n in 1:N)
    y[n] ~ normal(tau[n], epsilon);
    
 // Priors fuer die Varianzen     
  xi ~ student_t(2, 0, 3); 
  epsilon ~ student_t(2, 0, 3);
}
