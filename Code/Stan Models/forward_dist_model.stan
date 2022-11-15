
data {
  int<lower=0> N; // Number of observations
  int<lower=0> n_splines; // Number of basis functions in spline
  int<lower=0> n_horses; // Number of horses
  int<lower=0> n_jockeys; // Number of jockeys
  int<lower=0> n_tracks; // Number of track surface/condition combos in data
  int<lower=0> n_cov; // Number of covariates
  vector[N] dist; // Outcome (distance_next_smooth)
  matrix[N,n_splines] spline_matrix; // Spline matrix
  matrix[N,n_cov] x; // Covariates
  int horse_id[N]; // Horse IDs for each frame
  int<lower=1> jockey_id[N]; // Tracks
  int<lower=1> track_id[N]; // Tracks
  real beta_prior[n_splines];
  //vector[n_tracks] track_prior; // Prior mean for tracks
}


parameters {
  matrix[n_splines,n_horses] beta; // Spline parameters
  real<lower=0> sigma; // Variance parameter
  vector[n_cov] gamma; // Covariate parameters
  vector[n_tracks-1] theta_track_raw; // Random effect for track surface + condition
  vector[n_jockeys - 1] theta_jockey_raw;
  vector[n_splines] mu_beta; // Mean vector for betas
}


transformed parameters {
  vector[n_tracks] theta_track = append_row(theta_track_raw, -sum(theta_track_raw));
  vector[n_jockeys] theta_jockey = append_row(theta_jockey_raw, -sum(theta_jockey_raw));
}


model {
 
 // LIKELIHOOD
 
 vector[N] xbeta; // Linear predictor
 
 real xb_spline; // Linear predictor for just splines
 real xb_cov; // Linear predictor for just covariates
 
 for (i in 1:N) {
   xb_spline = dot_product(to_vector(beta[,horse_id[i]]),to_row_vector(spline_matrix[i,]));
   xb_cov = dot_product(gamma,x[i,]);
   
   xbeta[i] = xb_spline + xb_cov + theta_track[track_id[i]] + theta_jockey[jockey_id[i]];
 }
 
 
 dist ~ normal(xbeta, sigma);
 
 
 // PRIORS
 
 for(j in 1:n_splines){
   to_vector(beta[j,:]) ~ normal(mu_beta[j],1);
 }

 sigma ~ normal(0,0.5);
 mu_beta ~ normal(beta_prior,0.5);
 gamma ~ normal(0,0.5);
 theta_track_raw ~ normal(0, 0.1);
 theta_jockey_raw ~ normal(0, 0.1);
 
}


