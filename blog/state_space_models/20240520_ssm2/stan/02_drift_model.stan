data {
  int<lower=0> N; // Anzahl der Beobachtungen
  int<lower=0> NPRED; // Anzahl Predictions
  vector[N] y; // beobachtete Werte
}

parameters {
  vector[N] tau; //
  vector[N] nu; // 
  real<lower=0> sd_xi; // Sd - je nachdem wie sich tau 'bewegt'
  real<lower=0> sd_epsilon; // Error Term für die Obs
  real<lower=0> sd_zeta;
  real beta;
}

model {
  // Zustandsgleichung
  for (n in 2:N){
    tau[n] ~ normal(tau[n-1] + nu[n-1], sd_xi);
    nu[n] ~ normal(nu[n-1], sd_zeta);
  }
  
  // Beobachtungsgleichung
  for (n in 1:N)
    y[n] ~ normal(tau[n], sd_epsilon);
    
  sd_xi ~ student_t(2, 0, 3);
  sd_epsilon ~ student_t(2, 0, 3);
  sd_zeta ~ student_t(2, 0, 3);
}
