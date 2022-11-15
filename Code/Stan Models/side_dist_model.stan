
data {
  int<lower=0> N; // Number of observations
  int<lower=0> n_cov; // Number of covariates
  int<lower=0> n_tracks; // Number of track surface/condition combos in data
  int<lower=0> n_jockeys;
  vector[N] y; // Outcome
  int<lower=1> track_id[N]; // Tracks
  int<lower=1> jockey_id[N]; // Jockeys
  vector[N] y_lag; // Lagged outcome (to be used as predictor variable)
  matrix[N,n_cov] x; // Covariates
  vector[N] track_width; // Track width
}


parameters {
  real beta_lag; // Parameter for previous side movement
  vector[n_cov] beta; // Parameters for covariates
  vector[n_tracks-1] theta_track_raw; // Random effect for track surface + condition
  vector[n_jockeys] theta_jockey;
  real<lower=0> sigma; // Parameter for sigma
}

transformed parameters {
  vector[n_tracks] theta_track = append_row(theta_track_raw, -sum(theta_track_raw));
}

model {
  
  vector[N] xbeta; // Linear predictor
  real xmean; // Not used?
  real phi_a; // Not used?
  real phi_b; // Not used?
  real PHI_a; // Not used?
  real PHI_b; // Not used?
  
  // LIKELIHOOD
  
  for(i in 1:N){
    xbeta[i] = y_lag[i]*beta_lag + x[i,]*beta + theta_track[track_id[i]] + theta_jockey[jockey_id[i]];
  }
  
  y ~ normal(xbeta, sigma);
  
  // PRIORS
  
  beta_lag ~ normal(0.99,0.2); // Strong prior
  beta ~ normal(0, 0.1);
  sigma ~ normal(0, 0.25);
  theta_track_raw ~ normal(0, 0.1);
  theta_jockey ~ normal(0, 0.1);

}

