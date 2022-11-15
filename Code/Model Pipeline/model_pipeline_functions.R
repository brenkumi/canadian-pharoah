
source("Code/Data Preparation/track_connector_functions.R")

#####################################
## STEP 1: FIT SIDE MOVEMENT MODEL ##
#####################################

# Fit side movement model
fit_side_model = function(df,
                          n_fit, # Number of observations in model
                          exclude_tol = 1, # Removes row if side movement exceeds this value in a single frame
                          fit_type = "optim", # What type of engine for model fitting?
                          draws = 2000, # Number of draws in Stan model
                          chains = 1, # Number of chains in Stan model
                          fit_lm = FALSE, # Option to fit the model with `lm` as well as Stan
                          covariates = c(
                            "inside_movement",
                            "n_horses_inside",
                            "n_horses_outside",
                            "nearest_inside",
                            "nearest_outside",
                            "nearest_inside_euclid",
                            "nearest_outside_euclid",
                            "drafting_int",
                            "bend",
                            "home_stretch",
                            "turn2hs",
                            "first40"
                          )) {
  
  # Convert track types to numeric
  tracks = data.frame(condition_type = c("dirt_fast", "dirt_good", "turf_firm", "turf_good", "dirt_muddy", "dirt_sloppy", "turf_yielding")) %>%
    mutate(track_index = as.numeric(row_number()))
  
  # Convert horse names to numeric from 1:n_horses (note: we didn't end up using a horse effect in the model)
  horses = df %>%
    select(horse_name, horse_id) %>%
    unique() %>%
    arrange(horse_id) %>%
    mutate(horse_index = as.numeric(row_number()))
  
  # Convert jockey names to numeric from 1:n_jockeys
  jockeys = df %>%
    select(jockey, jockey_id) %>%
    unique() %>%
    arrange(jockey_id) %>%
    mutate(jockey_index = as.numeric(row_number()))
  
  
  # Remove first frames and unsensible data
  df = df %>%
    filter(abs(inside_movement) < exclude_tol & frame_id != 1) %>%
    ungroup() %>%
    # Join in new track type, horse and jockey IDs
    left_join(tracks, by = "condition_type") %>%
    left_join(horses, by = c("horse_name", "horse_id")) %>%
    left_join(jockeys, by = c("jockey", "jockey_id")) 
  
  # Remove NAs for some covariates if applicable
  if ("percentage_side_ways_movement_lag" %in% covariates) {
    df = df %>% filter(!is.na(percentage_side_ways_movement_lag))
  } 
  
  if ("nearest_fwd" %in% covariates) {
    df = df %>% filter(!(nearest_fwd == Inf))
  }
  
  # Randomly sample n_fit observations to fit model
  r_samp = sample.int(n = nrow(df), size = n_fit)
  df_samp = df[r_samp,]
  
  # Select just covariates (x's)
  xs <- df_samp %>%
    select(all_of(covariates))
  
  # Create list of data to use in Stan model
  stan_list = list(
    N = nrow(df_samp), # Number of observations
    n_cov = ncol(xs), # Number of covariates
    n_tracks = nrow(tracks), # Number of track types
    n_jockeys = nrow(jockeys), # Number of jockeys
    y = df_samp$dist_to_track_inner_clean, # Outcome
    jockey_id = df_samp$jockey_index, # Horse IDs
    y_lag = df_samp$lag_inside, # Lagged outcome
    track_id = df_samp$track_index, # Track types
    x = as.matrix(xs), # Covariates
    track_width = df_samp$track_width # Track width
  )
  
  # Load model
  side_mod = stan_model("Code/Stan Models/side_dist_model.stan")
  
  # If engine is `optim`...
  if (fit_type == "optim") {
    
    # Fit model
    mod = tryCatch(
      optimizing(
        side_mod,
        data = stan_list,
        importance_resampling = TRUE,
        draws = draws,
        hessian = TRUE
      ),
      warning = function(e) {
        message("Rerun the function!!!")
        print(e)
      }
    )
    
    side_stan_model = mod
    
    # NOTE!
    #   If the model is optim then the following code gets the mean parameters + samples
    #   Pay attention to the warnings, the above functions sometimes need to be rerun
    
    # Extract model parameter estimates
    mean_params = side_stan_model$par
    side_parameter_matrix = side_stan_model$theta_tilde
    beta_lag_samples = side_parameter_matrix[,1]
    side_beta_samples = side_parameter_matrix[,grepl("beta\\[",colnames(side_parameter_matrix))]
    side_sigma_samples = side_parameter_matrix[,grepl("sigma",colnames(side_parameter_matrix))]
    side_theta_track_samples = side_parameter_matrix[,grepl("theta_track\\[",colnames(side_parameter_matrix))]
    side_theta_jockey_samples = side_parameter_matrix[,grepl("theta_jockey\\[",colnames(side_parameter_matrix))]
    
  } else {
    
    # If not optim, then run the model with MCMC
    mod = sampling(
      side_mod,
      data = stan_list,
      chains = chains,
      iter = draws
    )
  }
  
  if (fit_lm) {
    
    # Extract data for model
    equiv_df = data.frame(
      y = df_samp$dist_to_track_inner_clean,
      y_lag = df_samp$lag_inside,
      xs
    )
    
    # Fit lm
    lm_mod = lm(y ~ ., data = equiv_df)
    
    out = list(
      stan_model = mod,
      lm_model = lm_mod
    )
    
  } else {
    out = list(
      stan_model = mod,
      mean_params = side_stan_model$par,
      param_matrix = side_parameter_matrix,
      beta_lag_samples = beta_lag_samples,
      beta_samples = side_beta_samples,
      theta_track_samples = side_theta_track_samples,
      theta_jockey_samples = side_theta_jockey_samples,
      sigma_samples = side_sigma_samples,
      track_index = tracks,
      jockey_index = jockeys,
      covariates = covariates
    )
  }
  
  return(out)
  
}



########################################
## STEP 2: FIT FORWARD MOVEMENT MODEL ##
########################################

# Fit splines to data for all horses in a single race
make_splines_single_race = function(df,
                                    race,
                                    knots,
                                    boundary_knots,
                                    degree) {
  
  # Filter for selected race
  df_s = df %>% filter(race_id == race)
  
  # Find number of horses in race
  race_horse_ids = unique(df_s$horse_id)
  n_horses = length(race_horse_ids)
  
  # Fit a spline for each horse in the race
  spline_df <- foreach::foreach(i = 1:n_horses,.combine = rbind) %do% {
    
    # Filter for specified horse
    spline_df = df_s %>% filter(horse_id == race_horse_ids[i])
    
    # Compute b-spline for distance_travelled
    y = bSpline(
      spline_df$distance_travelled,
      knots = knots,
      degree = degree,
      intercept = TRUE,
      Boundary.knots = boundary_knots
    )
    
    # Convert spline to data frame
    s_df <- as.data.frame(y)
    names(s_df) <- paste0("spline_",c(1:ncol(s_df)))
    
    # Add spline data to the main df
    out_df <- cbind(spline_df,s_df) %>%
      mutate(
        cumulative_draft = cumsum(is_drafting),
        percentage_cumulative_draft = cumulative_draft / c(1:nrow(.)),
        drafting_pct_last5_frames = frollmean(x = is_drafting, n = 5, fill = 0),
        drafting_pct_last10_frames = frollmean(x = is_drafting, n = 10, fill = 0)
      )
    
    out_df
    
  }
  
  return(spline_df)
}



# Fit splines to data for all horses in all races
make_splines_multiple_races = function(df,
                                       knots,
                                       boundary_knots,
                                       degree,
                                       races) {
  
  # Extract number of races
  n_races = length(races)
  
  # For each race...
  out_df = foreach::foreach(j = 1:n_races, .combine = 'rbind') %do% {
    
    curr_race = races[j]
    
    # Fit splines for all horses in race
    make_splines_single_race(
      df = df,
      race = curr_race,
      knots = knots,
      boundary_knots = boundary_knots,
      degree = degree
    )
    
  }  
  
  # Extract unique horses and number of horses
  obs_horse_ids <- unique(out_df$horse_id)
  n_horses_obs <- length(obs_horse_ids)
  
  # Re-map horse IDs
  out_df$stan_horse_id <- plyr::mapvalues(
    x = out_df$horse_id,
    from = obs_horse_ids,
    to = 1:n_horses_obs
  )
  
  return(out_df) 
  
}



# Create the stan data frame and related stan list
long_df_standata = function(df,
                            knots,
                            boundary_knots,
                            degree,
                            races,
                            covariates) {
  
  # Clean up data
  df = df %>%
    filter(race_id %in% races) %>%
    group_by(race_id, horse_id) %>%
    filter(frame_id != max(frame_id)) %>%
    ungroup()
  
  # Convert track types to numeric
  tracks = data.frame(condition_type = c("dirt_fast", "dirt_good", "turf_firm", "turf_good", "dirt_muddy", "dirt_sloppy", "turf_yielding")) %>%
    mutate(track_index = as.numeric(row_number())) %>%
    mutate(prior = c(0.3, 0.2, 0.1, 0, -0.1, -0.2, -0.3))
  
  df = df %>%
    left_join(tracks, by = "condition_type")
  
  # Add in jockey effect
  jockeys = df %>%
    select(jockey, jockey_id) %>%
    unique() %>%
    arrange(jockey_id) %>%
    mutate(jockey_index = as.numeric(row_number()))
  
  df = df %>%
    left_join(jockeys, by = "jockey")
  
  # Fit splines for each horse in each race
  stan_df = make_splines_multiple_races(
    df = df,
    races = races,
    knots = knots,
    boundary_knots = boundary_knots,
    degree = degree
  )
  
  # Get data frame of horse identifiers
  horses = stan_df %>%
    select(horse_name, horse_id, stan_horse_id) %>%
    unique()
  
  # Obtain number of basis functions
  spline_cols <- names(stan_df)[grepl("spline",names(stan_df))]
  n_splines <- length(spline_cols)
  
  # Obtain number of horses
  horses_obs <- stan_df$stan_horse_id
  n_horses <- max(horses_obs)
  
  # Extract just spline data
  spline_matrix <- stan_df[,spline_cols]
  
  # Extract covariates
  xs <- stan_df %>%
    dplyr::ungroup() %>%
    dplyr::select(all_of(covariates))
  
  
  # Only use observations with no NAs
  indices <- apply(spline_matrix,1,function(x){
    sum(is.na(x)) == 0
  })
  
  indices2 <- apply(xs,1,function(x){
    sum(is.na(x)) == 0
  })
  
  inds <- (indices & indices2)
  
  stan_df <- stan_df[inds,]
  
  # Create data list to be used in model
  stan_list <- list(
    N = nrow(stan_df), # Number of observations
    n_splines = n_splines, # Number of splines
    n_horses = n_horses, # Number of horses
    n_jockeys = nrow(jockeys), # Number of jockeys
    n_tracks = nrow(tracks), # Number of track type/condition combos
    x = as.matrix(xs[inds,]), # Covariate data
    n_cov = ncol(xs), # Number of covariates
    dist = stan_df$distance_next_smooth, # Outcome
    spline_matrix = spline_matrix[inds,], # Spline data
    horse_id = stan_df$stan_horse_id, # Horse IDs
    jockey_id = stan_df$jockey_index, # Jockey IDs
    track_id = stan_df$track_index, # Track combos
    track_prior = tracks$prior # Prior for tracks
  )
  
  # Spit out list of data frame with splines and Stan data list
  out <- list(
    stan_df = stan_df,
    stan_list = stan_list,
    jockey_index = jockeys,
    track_index = tracks,
    horse_index = horses
  ) 
  
  return(out)
  
}



# Fit forward movement model
fit_forward_model = function(df,
                             knots,
                             boundary_knots,
                             degree,
                             beta_prior,
                             races,
                             covariates,
                             fit_type = "optim",
                             n_draws = 2000,
                             chains = 1,
                             cores = 1,
                             max_iter = 15000,
                             lite = FALSE,
                             hess = FALSE) {
  
  # Make distance_next_smooth sensible
  df = df %>%
    mutate(distance_next_smooth = case_when(
      distance_next_smooth > 5.2 ~ 5.2,
      distance_next_smooth < 0 ~ runif(1),
      TRUE ~ distance_next_smooth
    )) %>%
    mutate(distance_travelled = case_when(
      distance_travelled < 0 ~ runif(1),
      TRUE ~ distance_travelled
    ))
  
  
  # Remove NAs for some covariates if applicable
  if ("percentage_side_ways_movement_lag" %in% covariates) {
    df = df %>% filter(!is.na(percentage_side_ways_movement_lag))
  } 
  
  if ("nearest_fwd" %in% covariates) {
    df = df %>% filter(!(nearest_fwd == Inf))
  }
  
  # Create Stan data list
  stan_data = long_df_standata(
    df = df,
    knots = knots,
    boundary_knots = boundary_knots,
    degree = degree,
    races = races,
    covariates = covariates
  )
  
  # Extract data from stan_data
  stan_list <- stan_data$stan_list 
  tracks = stan_data$track_index
  horses = stan_data$horse_index
  jockeys = stan_data$jockey_index
  
  # Load model code
  if (lite) {
    fwd_stan_mod <- stan_model("Code/Stan Models/forward_dist_model_lite.stan")
  } else {
    fwd_stan_mod <- stan_model("Code/Stan Models/forward_dist_model.stan")
  }
  
  # If engine is variational inference...
  if(fit_type == "vb") {
    
    fitted_mod = vb(
      object = fwd_stan_mod,
      data = stan_list,
      importance_resampling = TRUE,
      output_samples = n_draws,
      iter = max_iter
    )
    
  }
  
  if(fit_type == "sample") {
    
    fitted_mod = sampling(
      object = fwd_stan_mod,
      data = stan_list,
      iter = n_draws*2,
      chains = chains,
      cores = cores,
      refresh = 20
    )
    
  }
  
  if(fit_type == "optim"){
    
    fitted_mod = tryCatch(
      optimizing(
        object = fwd_stan_mod,
        data = stan_list,
        importance_resampling = TRUE,
        draws = n_draws,
        hessian = hess
      ),
      warning = function(e){
        message("Rerun the function!!!")
        print(e)
      }
    )
    
    # Extract the full model data frame and horse IDs
    stan_df <- stan_data$stan_df
    stan_horse_ids <- unique(stan_df$stan_horse_id)
    horse_ids <- unique(stan_df$horse_id)
    
    # Extract parameter matrix
    param_matrix <- fitted_mod$theta_tilde
    
    # Extract spline parameters (beta)
    beta_matrix <- param_matrix[,grepl("^beta",colnames(param_matrix))]
    
    # Extract covariate parameters (gamma)
    gamma_samples <- param_matrix[,grepl("gamma",colnames(param_matrix))]
    
    # Extract track effect (theta_track)
    theta_track_samples = param_matrix[,grepl("theta_track\\[", colnames(param_matrix))]
    
    # Extract jockey effect (theta_jockey)
    theta_jockey_samples = param_matrix[,grepl("theta_jockey\\[", colnames(param_matrix))]
    
    # Extract variance parameter (sigma)
    sigma_samples <- param_matrix[,grepl("sigma",colnames(param_matrix))]
    
    # Spit out list of model, data and parameter estimates
    out <- list(
      fitted_mod = fitted_mod,
      stan_df = stan_df,
      stan_list = stan_data$stan_list,
      stan_horse_ids = stan_horse_ids,
      horse_ids = horse_ids,
      param_matrix = param_matrix,
      beta_samples = beta_matrix,
      gamma_samples = gamma_samples,
      theta_track_samples = theta_track_samples,
      theta_jockey_samples = theta_jockey_samples,
      sigma_samples = sigma_samples,
      track_index = tracks,
      jockey_index = jockeys,
      horse_index = horses,
      knots = knots,
      boundary_knots = boundary_knots,
      degree = degree,
      covariates = covariates,
      beta_prior = beta_prior
    )
  
   
    
  } else {
    
    # Extract stan data and horse IDs
    stan_df <- stan_data$stan_df
    stan_horse_ids <- unique(stan_df$stan_horse_id)
    horse_ids <- unique(stan_df$horse_id)
    
    # Spit out basic data list
    out <- list(
      fitted_mod = fitted_mod,
      stan_df = stan_df,
      stan_list = stan_data$stan_list,
      stan_horse_ids = stan_horse_ids,
      horse_ids = horse_ids,
      track_index = tracks,
      jockey_index = jockeys,
      horse_index = horses
    )
    
  }
  
  return(out)
}



# Extract posterior samples for a set of horses
get_beta_list = function(beta_matrix, horse_ids){
  
  # Obtain number of horses
  n_horses <- length(horse_ids)
  
  # For each horse...
  beta_list <- foreach::foreach(i = 1:n_horses)%do%{
    
    # Extract posterior estimates of beta for the horse's basis functions
    horse_id <- horse_ids[i]
    betas_cols <- grepl(paste0(",",paste0(horse_id,"]")),colnames(beta_matrix))
    betas <- beta_matrix[,betas_cols]
    
    betas
  }
  
  return(beta_list)
}



# Take 'draws' from pointwise model with normal(0,0.1) jitter
jitter_fwd_model = function(fwd_model, n_draws = 2000, jitter = 0.05) {
  
  param_matrix = fwd_model$param_matrix
  
  sigma = param_matrix[,which(colnames(param_matrix) == "sigma")]

  jittered_matrix = apply(param_matrix, 2, function(x) {
    rnorm(n_draws, mean = x, sd = jitter)
  })
  
  jittered_matrix[,which(colnames(jittered_matrix) == "sigma")] = truncnorm::rtruncnorm(n_draws, a = 0, mean = sigma, sd = jitter / 4)

  fwd_model$param_matrix = jittered_matrix
  
  return(fwd_model)
  
}



############################
## STEP 3: SIMULATE RACES ##
############################

## CREATE B-SPLINE MATRIX

create_spline_matrix = function(track,
                                course,
                                run_up_distance,
                                fwd_model,
                                granularity) {
  
  knots = fwd_model$knots
  boundary_knots = fwd_model$boundary_knots
  degree = fwd_model$degree
  
  # Extract track
  locs = track_connector %>%
    filter(track_id == track & course_type == course & race_loc >= -run_up_distance) %>%
    mutate(distance = race_loc + run_up_distance) %>%
    slice(which(round(race_loc, 1) %% granularity == 0))
  
  # Create spline for each marker on track
  spline = bSpline(
    locs$distance,
    knots = knots,
    degree = degree,
    intercept = TRUE,
    Boundary.knots = boundary_knots
  )
  
  return(spline)
  
}


## EXTRACT SIMULATION INPUT DATA ##

# ... FROM A PARTICULAR RACE AND FRAME

extract_frame = function(tracking_data, race, frame, fwd_model, lat_model) {
  
  # Filter for specified frame in race and arrange by horse_id
  frame_data = tracking_data %>%
    group_by(race_id, horse_id) %>%
    arrange(race_id, horse_id, frame_id) %>%
    mutate(
      race_loc_lag = lag(race_loc, default = NA),
      race_loc_lag2 = lag(race_loc, n = 2, default = NA),
      dist_to_track_inner_clean_lag = lag(dist_to_track_inner_clean, default = NA),
      dist_to_track_inner_clean_lag2 = lag(dist_to_track_inner_clean, n = 2, default = NA)
    ) %>%
    mutate(
      race_loc_lag = ifelse(is.na(race_loc_lag), race_loc, race_loc_lag),
      race_loc_lag2 = ifelse(is.na(race_loc_lag2), race_loc, race_loc_lag2),
      dist_to_track_inner_clean_lag = ifelse(is.na(dist_to_track_inner_clean_lag), dist_to_track_inner_clean, dist_to_track_inner_clean_lag),
      dist_to_track_inner_clean_lag2 = ifelse(is.na(dist_to_track_inner_clean_lag2), dist_to_track_inner_clean, dist_to_track_inner_clean_lag2)
    ) %>%
    ungroup() %>%
    filter(race_id == race & frame_id == frame & !is_finished) %>%
    arrange(horse_id)
  
  # Obtain run up distance
  run_up = frame_data$run_up_distance[[1]]
  
  # Extract stan horse IDs
  horse_ids = frame_data %>%
    select(horse_name) %>%
    left_join(fwd_model$horse_index, by = "horse_name") %>%
    select(stan_horse_id) %>%
    unlist() %>%
    unname()
  
  # Extract stan jockey IDs
  jockey_ids = frame_data %>%
    select(jockey) %>%
    left_join(fwd_model$jockey_index %>% rename(fwd_jockey = jockey_index), by = "jockey") %>%
    left_join(lat_model$jockey_index %>% rename(lat_jockey = jockey_index), by = "jockey") %>%
    select(fwd_jockey, lat_jockey)
  
  # Extract stan track type/condition ID
  track_id = frame_data %>%
    select(condition_type) %>%
    head(1) %>%
    left_join(fwd_model$track_index, by = "condition_type") %>%
    select(track_index) %>%
    unlist() %>%
    unname()
  
  # Extract forward and lateral locations
  fwd_locs = frame_data$race_loc
  lat_locs = frame_data$dist_to_track_inner_clean
  fwd_locs_lag = frame_data$race_loc_lag
  lat_locs_lag = frame_data$dist_to_track_inner_clean_lag
  fwd_locs_lag2 = frame_data$race_loc_lag2
  lat_locs_lag2 = frame_data$dist_to_track_inner_clean_lag2
  
  # Extract covariates
  fwd_covariates = frame_data %>%
    select(any_of(fwd_model$covariates)) %>%
    mutate_all(as.numeric) %>%
    as.matrix()
  
  lat_covariates = frame_data %>%
    select(any_of(lat_model$covariates)) %>%
    mutate_all(as.numeric) %>%
    as.matrix()
  
  # Extract track and course
  track = frame_data$track_id[[1]]
  course = frame_data$course_type[[1]]
  
  # Extract energy measures
  c_energy = frame_data$c_energy
  c_max_energy = frame_data$c_max_energy
  
  return(list(
    track_name = track,
    course_type = course,
    frame_id = frame,
    run_up_distance = run_up,
    fwd_locs = fwd_locs,
    lat_locs = lat_locs,
    fwd_locs_lag = fwd_locs_lag,
    lat_locs_lag = lat_locs_lag,
    fwd_locs_lag2 = fwd_locs_lag2,
    lat_locs_lag2 = lat_locs_lag2,
    horse_ids = horse_ids,
    jockey_ids = jockey_ids,
    track_id = track_id,
    fwd_covariates = fwd_covariates,
    lat_covariates = lat_covariates,
    c_energy = c_energy,
    c_max_energy = c_max_energy
  ))
  
}


# ... FOR A HYPOTHETICAL RACE

initialize_frame = function(track, course, condition, run_up, horses, jockeys, fwd_model, lat_model) {
  
  ## SET TRACK_ID ##
  
  if (course == "D") {
    ctype = paste("dirt", tolower(condition), sep = "_")
  } else {
    ctype = paste("turf", tolower(condition), sep = "_")
  }
  
  track_id = fwd_model$track_index %>%
    filter(condition_type == ctype) %>%
    select(track_index) %>%
    unlist() %>%
    unname()
  
  
  ## SET HORSE_IDS ##
  
  if (is.character(horses)) {
    horse_ids = data.frame(horse_name = horses) %>%
      left_join(fwd_model$horse_index, by = "horse_name") %>%
      select(stan_horse_id) %>%
      unlist() %>%
      unname()
  } else {
    horse_ids = data.frame(horse_id = horses) %>%
      left_join(fwd_model$horse_index, by = "horse_id") %>%
      select(stan_horse_id) %>%
      unlist() %>%
      unname()
  }
  
  
  ## SET JOCKEY_IDS ##
  
  if (is.character(jockeys)) {
    jockey_ids = data.frame(jockey = jockeys) %>%
      left_join(fwd_model$jockey_index %>% rename(fwd_jockey = jockey_index), by = "jockey") %>%
      left_join(lat_model$jockey_index %>% rename(lat_jockey = jockey_index), by = "jockey") %>%
      select(fwd_jockey, lat_jockey)
  } else {
    jockey_ids = data.frame(jockey_id = jockeys) %>%
      left_join(fwd_model$jockey_index %>% rename(fwd_jockey = jockey_index), by = "jockey_id") %>%
      left_join(lat_model$jockey_index %>% rename(lat_jockey = jockey_index), by = "jockey_id") %>%
      select(fwd_jockey, lat_jockey)
  }
  
  
  ## DETERMINE STARTING LOCATIONS ##
  
  start_point = track_connector %>%
    filter(track_id == track & course_type == course & -round(run_up,1) == race_loc)
  
  x_inner = start_point$x_inner[[1]]
  y_inner = start_point$y_inner[[1]]
  x_outer = start_point$x_outer[[1]]
  y_outer = start_point$y_outer[[1]]
  
  track_width = sqrt((x_inner - x_outer)^2 + (y_inner - y_outer)^2)
  
  starting_positions = data.frame(horse_index = 1:length(horses)) %>%
    mutate(race_loc = -run_up) %>%
    mutate(track_loc = race2track(track,course,race_loc)) %>%
    mutate(lat_dist = 4 + horse_index) %>%
    mutate(x = x_inner * (track_width - (4 + horse_index)) / track_width + x_outer * (4 + horse_index) / track_width) %>%
    mutate(y = y_inner * (track_width - (4 + horse_index)) / track_width + y_outer * (4 + horse_index) / track_width) %>%
    mutate(horse_id = horses)
  
  fwd_locs = starting_positions$race_loc
  lat_locs = starting_positions$lat_dist
  
  
  ## INITIALIZE COVARIATES ##
  
  covariates = data.frame(
    inside_movement = 0,
    distance_prev = 0,
    n_horses_inside = 1:length(horses) - 1,
    n_horses_outside = rev(1:length(horses)) - 1,
    n_horses_fwd = 0,
    n_horses_bwd = 0,
    nearest_inside = 1,
    nearest_outside = 1,
    nearest_inside_euclid = 1,
    nearest_outside_euclid = 1,
    nearest_fwd = 0,
    drafting_int = 0,
    bend = 0,
    home_stretch = 0,
    turn2hs = 0,
    first40 = 1,
    prop_energy_saved = 0,
    prop_energy_saved_x_hs = 0
  )
  
  fwd_covariates = covariates %>%
    select(any_of(fwd_model$covariates))
  
  lat_model$covariates = lat_model$covariates[lat_model$covariates != "y_lag"]
  
  lat_covariates = covariates %>%
    select(any_of(lat_model$covariates))
  
  y_lag = 0;
  
  return(list(
    track_name = track,
    course_type = course,
    frame_id = 1,
    run_up_distance = run_up,
    fwd_locs = fwd_locs,
    lat_locs = lat_locs,
    fwd_locs_lag = fwd_locs,
    lat_locs_lag = lat_locs,
    fwd_locs_lag2 = fwd_locs,
    lat_locs_lag2 = lat_locs,
    horse_ids = horse_ids,
    jockey_ids = jockey_ids,
    track_id = track_id,
    fwd_covariates = fwd_covariates,
    lat_covariates = lat_covariates,
    #y_lag = 0,
    c_energy = rep(0, length(horse_ids)),
    c_max_energy = rep(0, length(horse_ids))
  ))
  
}


## MERGE POSTERIOR DRAWS FROM BOTH MODELS ##

merge_parameters = function(fwd_model, lat_model) {
  
  # Extract posterior draws from both models
  fwd_param_matrix = fwd_model$param_matrix
  lat_param_matrix = lat_model$param_matrix
  
  # Slice and prep spline parameters
  
  
  # Remove "_raw" parameters
  fwd_param_matrix = fwd_param_matrix[, which(!grepl("raw", colnames(fwd_param_matrix)))]
  lat_param_matrix = lat_param_matrix[, which(!grepl("raw", colnames(lat_param_matrix)))]
  
  # Add a "fwd_" or "lat_" prefix to parameters
  colnames(fwd_param_matrix) = paste("fwd", colnames(fwd_param_matrix), sep = "_")
  colnames(lat_param_matrix) = paste("lat", colnames(lat_param_matrix), sep = "_")
  
  # Combine parameters into one matrix
  param_matrix = cbind(fwd_param_matrix, lat_param_matrix)
  
  return(param_matrix)
  
}


# Simulate a race from a certain moment using Stan
simulate_race = function(input_data,
                         fwd_model,
                         lat_model,
                         n_draws = 2000) {
  
  # Set granularity
  # We use this to create a grid in which we round each horse's position to throughout the race
  granularity = 1
  
  # Extract some important data
  track = input_data$track_name
  course = input_data$course_type
  run_up = input_data$run_up_distance
  
  # Obtain parameter matrix for both models
  param_matrix = merge_parameters(fwd_model, lat_model)
  
  # Create spline matrix for any location along the track
  spline_matrix = create_spline_matrix(track, course, run_up, fwd_model, granularity)
  
  # Create vector of binaries for track stage
  track_binaries = track_connector %>%
    filter(track_id == track & course_type == course & race_loc >= -run_up) %>%
    group_by(stage) %>%
    mutate(
      last5 = ifelse(race_loc >= max(race_loc) - 5, 1, 0),
      first10 = ifelse(race_loc <= min(race_loc) + 10, 1, 0),
      home_stretch = ifelse(stage == "upper straightaway" & !first10, 1, 0),
      bend = ifelse((stage == "right bend" & !last5) | stage == "left bend", 1, 0),
      turn2hs = ifelse((stage == "upper straightaway" & first10) | (stage == "left bend" & last5), 1, 0)
    ) %>%
    ungroup() %>%
    slice(which(round(race_loc, 1) %% granularity == 0))
  
  # Create list of data to be input into Stan
  stan_list = list(
    n_race_horses = length(input_data$horse_ids),
    n_horses = nrow(fwd_model$horse_index),
    n_jockeys_fwd = nrow(fwd_model$jockey_index),
    n_jockeys_lat = nrow(lat_model$jockey_index),
    n_tracks = nrow(fwd_model$track_index),
    n_splines = ncol(spline_matrix),
    n_markers = nrow(spline_matrix),
    n_fwd_cov = ncol(input_data$fwd_covariates),
    n_lat_cov = ncol(input_data$lat_covariates),
    start_frame = input_data$frame_id,
    race_length = 1609.344,
    run_up_distance = run_up,
    horse_id_fwd = input_data$horse_ids,
    jockey_id_fwd = input_data$jockey_ids$fwd_jockey,
    jockey_id_lat = input_data$jockey_ids$lat_jockey,
    track_id = input_data$track_id,
    initial_fwd_loc = input_data$fwd_locs,
    initial_lat_loc = input_data$lat_locs,
    initial_fwd_loc_lag = input_data$fwd_locs_lag,
    initial_lat_loc_lag = input_data$lat_locs_lag,
    initial_fwd_loc_lag2 = input_data$fwd_locs_lag2,
    initial_lat_loc_lag2 = input_data$lat_locs_lag2,
    initial_y_lag = input_data$y_lag,
    spline_matrix = spline_matrix,
    initial_fwd_cov = as.matrix(input_data$fwd_covariates),
    initial_lat_cov = as.matrix(input_data$lat_covariates),
    cum_energy = input_data$c_energy,
    cum_max_energy = input_data$c_max_energy,
    bend = track_binaries$bend,
    turn2hs = track_binaries$turn2hs,
    home_stretch = track_binaries$home_stretch,
    granularity = granularity
  )
  
  n_race_horses = stan_list$n_race_horses
  
  # Load race_simulations code
  generate_races = stan_model("Code/Stan Models/race_simulations.stan")
  
  # Run simulations
  simulated_races = gqs(generate_races, data = stan_list, draws = param_matrix)
  
  # Convert to an array
  simulation_array = as.array(simulated_races)
  
  # Unpack array into a list of data frames
  simulation_list = lapply(
    seq(dim(simulation_array)[1]),
    function(x) {
      simulation_array[x,1,] %>%
        matrix(nrow = 450 - stan_list$start_frame, byrow = FALSE) %>%
        as.data.frame() %>%
        magrittr::set_colnames(c(
          paste("fwd_loc", 1:n_race_horses, sep = ""),
          paste("lat_loc", 1:n_race_horses, sep = ""),
          paste("c_energy", 1:n_race_horses, sep = ""),
          paste("c_max_energy", 1:n_race_horses, sep = "")
        )) %>%
        mutate(frame_id = stan_list$start_frame + as.numeric(row_number())) %>%
        pivot_longer(
          cols = -c("frame_id"),
          names_to = c(".value", "horse"),
          names_pattern = "(.*)(1$|2$|3$|4$|5$|6$|7$|8$|9$|10$|11$|12$)"
        )
    }
  )
  
  # Prep horse and jockey data to add to the simulation data
  horses = data.frame(horse = 1:n_race_horses, stan_horse_id = stan_list$horse_id_fwd) %>%
    left_join(fwd_model$horse_index, by = "stan_horse_id") %>%
    select(horse, horse_name)
  
  jockeys = data.frame(jockey = 1:n_race_horses, jockey_index = stan_list$jockey_id_fwd) %>%
    left_join(fwd_model$jockey_index %>% select(jockey_index, jockey_name = jockey), by = "jockey_index") %>%
    select(jockey_name, horse = jockey)
    
  
  # Extract and unnest simulation data
  simulation_df = data.frame(sim_id = 1:n_draws, race_sim = I(simulation_list)) %>%
    unnest(race_sim) %>%
    ungroup() %>%
    mutate(horse = as.numeric(horse)) %>%
    left_join(horses, by = "horse") %>%
    left_join(jockeys, by = "horse") %>%
    mutate(horse_jockey = paste(horse_name, jockey_name, sep = " / ")) %>%
    select(-horse, -horse_name, -jockey_name) %>%
    select(sim_id, horse_jockey, frame_id, everything())
    
  
  # Determine the approximate finish time for each horse in each simulation
  finishes_df = simulation_df %>%
    group_by(sim_id, horse_jockey) %>%
    arrange(sim_id, horse_jockey, frame_id) %>%
    mutate(
      fwd_lag = lag(fwd_loc, default = 0),
      fwd_lag2 = lag(fwd_loc, n = 2, default = 0)
    ) %>%
    filter(fwd_loc >= 1609.4) %>%
    filter(row_number() == 1) %>%
    mutate(frame_speed = fwd_lag - fwd_lag2) %>%
    mutate(interpolated_frame = frame_id + (fwd_loc - fwd_lag) / (fwd_lag - fwd_lag2)) %>%
    mutate(finish_time = (interpolated_frame - 1)/3.6245) %>%
    ungroup() %>%
    group_by(sim_id) %>%
    mutate(finish_place = rank(finish_time, ties.method = "random")) %>%
    ungroup() %>%
    select(sim_id, horse_jockey, finish_time, finish_place)
  
  # Aggregate simulations to obtain average finish times
  time_summary = finishes_df %>%
    group_by(horse_jockey) %>%
    summarize(mean_time = mean(finish_time), sd = sd(finish_time)) %>%
    ungroup()
  
  # Aggregate simulations to obtain placement probabilities
  placement_summary = finishes_df %>%
    group_by(horse_jockey, finish_place) %>%
    summarize(prob = n() / n_draws) %>%
    ungroup()
  
  return(list(
    simulation_object = simulated_races,
    all_simulations = simulation_df,
    all_finishes = finishes_df,
    aggregated_times = time_summary,
    aggregated_placements = placement_summary,
    n_race_horses = n_race_horses
  ))
  
}


# Not working :(
simulate_full_race = function(race,
                              frame_start = 1,
                              frame_end = "max",
                              fwd_model,
                              lat_model,
                              tracking_data,
                              n_draws = 2000,
                              n_cores = 14) {
  
  if (frame_end == "max") {
    frame_end = tracking_data %>% filter(race_id == race) %>% filter(frame_id == max(frame_id)) %>% select(frame_id) %>% unlist() %>% unname()
  }
  
  # Set granularity
  # We use this to create a grid in which we round each horse's position to throughout the race
  granularity = 1
  
  # Extract some important data
  track = tracking_data %>% filter(race_id == race) %>% select(track_id) %>% head(1) %>% unlist() %>% unname()
  course = tracking_data %>% filter(race_id == race) %>% select(course_type) %>% head(1) %>% unlist() %>% unname()
  run_up = tracking_data %>% filter(race_id == race) %>% select(run_up_distance) %>% head(1) %>% unlist() %>% unname()
  
  # Create spline matrix for any location along the track
  spline_matrix = create_spline_matrix(track, course, run_up, fwd_model, granularity)
  
  # Obtain parameter matrix for both models
  param_matrix = merge_parameters(fwd_model, lat_model)
  
  # Create vector of binaries for track stage
  track_binaries = track_connector %>%
    filter(track_id == track & course_type == course & race_loc >= -run_up) %>%
    group_by(stage) %>%
    mutate(
      last5 = ifelse(race_loc >= max(race_loc) - 5, 1, 0),
      first10 = ifelse(race_loc <= min(race_loc) + 10, 1, 0),
      home_stretch = ifelse(stage == "upper straightaway" & !first10, 1, 0),
      bend = ifelse((stage == "right bend" & !last5) | stage == "left bend", 1, 0),
      turn2hs = ifelse((stage == "upper straightaway" & first10) | (stage == "left bend" & last5), 1, 0)
    ) %>%
    ungroup() %>%
    slice(which(round(race_loc, 1) %% granularity == 0))
  
  # Get max frame_id
  max_frame = tracking_data %>%
    filter(race_id == race) %>%
    select(frame_id) %>%
    unlist() %>%
    unname() %>%
    max()
  
  # Load race_simulations code
  generate_races = stan_model("Code/Stan Models/race_simulations.stan")
  
  # Parallelize code
  my_cluster = makeCluster(n_cores, setup_strategy = "sequential")
  #my_cluster = makeCluster(n_cores, type = "PSOCK")
  registerDoParallel(my_cluster)
  
  # For each frame...
  parLapply(cl = my_cluster, X = 1:(max_frame-1), fun = function(frame) {
    
    # Prepare the input data for simulations
    input_data = extract_frame(tracking_data, race, frame, fwd_model, lat_model)
    
    # Create list of data to be input into Stan
    stan_list = list(
      n_race_horses = length(input_data$horse_ids),
      n_horses = nrow(fwd_model$horse_index),
      n_jockeys_fwd = nrow(fwd_model$jockey_index),
      n_jockeys_lat = nrow(lat_model$jockey_index),
      n_tracks = nrow(fwd_model$track_index),
      n_splines = ncol(spline_matrix),
      n_markers = nrow(spline_matrix),
      n_fwd_cov = ncol(input_data$fwd_covariates),
      n_lat_cov = ncol(input_data$lat_covariates),
      start_frame = input_data$frame_id,
      race_length = 1609.344,
      run_up_distance = run_up,
      horse_id_fwd = input_data$horse_ids,
      jockey_id_fwd = input_data$jockey_ids$fwd_jockey,
      jockey_id_lat = input_data$jockey_ids$lat_jockey,
      track_id = input_data$track_id,
      initial_fwd_loc = input_data$fwd_locs,
      initial_lat_loc = input_data$lat_locs,
      initial_fwd_loc_lag = input_data$fwd_locs_lag,
      initial_lat_loc_lag = input_data$lat_locs_lag,
      initial_fwd_loc_lag2 = input_data$fwd_locs_lag2,
      initial_lat_loc_lag2 = input_data$lat_locs_lag2,
      initial_y_lag = input_data$y_lag,
      spline_matrix = spline_matrix,
      initial_fwd_cov = as.matrix(input_data$fwd_covariates),
      initial_lat_cov = as.matrix(input_data$lat_covariates),
      cum_energy = input_data$c_energy,
      cum_max_energy = input_data$c_max_energy,
      bend = track_binaries$bend,
      turn2hs = track_binaries$turn2hs,
      home_stretch = track_binaries$home_stretch,
      granularity = granularity
    )
    
    # Run simulations
    simulated_races = gqs(generate_races, data = stan_list, draws = param_matrix)
    
    # Convert to an array
    simulation_array = as.array(simulated_races)
    
    # Unpack array into a list of data frames
    simulation_list = lapply(
      seq(dim(simulation_array)[1]),
      function(x) {
        simulation_array[x,1,] %>%
          matrix(nrow = 450 - stan_list$start_frame, byrow = FALSE) %>%
          as.data.frame() %>%
          magrittr::set_colnames(c(
            paste("fwd_loc", 1:n_race_horses, sep = ""),
            paste("lat_loc", 1:n_race_horses, sep = ""),
            paste("c_energy", 1:n_race_horses, sep = ""),
            paste("c_max_energy", 1:n_race_horses, sep = "")
          )) %>%
          mutate(frame_id = stan_list$start_frame + as.numeric(row_number())) %>%
          pivot_longer(
            cols = -c("frame_id"),
            names_to = c(".value", "horse"),
            names_pattern = "(.*)(1$|2$|3$|4$|5$|6$|7$|8$|9$|10$|11$|12$)"
          )
      }
    )
    
    # Prep horse and jockey data to add to the simulation data
    horses = fwd_model$horse_index %>% select(horse_name, horse = stan_horse_id)
    jockeys = input_data$jockey_ids %>% 
      select(jockey_index = fwd_jockey) %>%
      left_join(fwd_model$jockey_index %>% select(jockey_index, jockey), by = "jockey_index") %>%
      mutate(horse = row_number() %>% as.numeric())
    
    # Determine the approximate finish time for each horse in each simulation
    finishes_df = data.frame(sim_id = 1:n_draws, race_sim = I(simulation_list)) %>%
      unnest(race_sim) %>%
      ungroup() %>%
      mutate(horse = as.numeric(horse)) %>%
      left_join(horses, by = "horse") %>%
      left_join(jockeys, by = "horse") %>%
      mutate(horse_jockey = paste(horse_name, jockey, sep = " / ")) %>%
      select(-horse, -horse_name, -jockey) %>%
      select(sim_id, horse_jockey, frame_id, everything()) %>%
      group_by(sim_id, horse) %>%
      arrange(sim_id, horse, frame_id) %>%
      mutate(
        fwd_lag = lag(fwd_loc, default = 0),
        fwd_lag2 = lag(fwd_loc, n = 2, default = 0)
      ) %>%
      filter(fwd_loc >= 1609.4) %>%
      filter(row_number() == 1) %>%
      mutate(frame_speed = fwd_lag - fwd_lag2) %>%
      mutate(interpolated_frame = frame_id + (fwd_loc - fwd_lag) / (fwd_lag - fwd_lag2)) %>%
      mutate(finish_time = (interpolated_frame - 1)/4) %>%
      ungroup() %>%
      group_by(sim_id) %>%
      mutate(finish_place = rank(finish_time)) %>%
      ungroup() %>%
      select(sim_id, horse, finish_time, finish_place)
    
    # Aggregate simulations to obtain average finish times
    time_summary = finishes_df %>%
      group_by(horse) %>%
      summarize(mean_time = mean(finish_time), sd = sd(finish_time)) %>%
      ungroup()
    
    # Aggregate simulations to obtain placement probabilities
    placement_summary = finishes_df %>%
      group_by(horse, finish_place) %>%
      summarize(prob = n() / n_draws) %>%
      ungroup() %>%
      mutate(frame_id = frame) %>%
      select(frame_id, horse, finish_place, prob) %>%
      left_join(time_summary, by = "horse")
    
    return(placement_summary)
  })
  
  # 
  # placement_probs = foreach(frame = 1:(max_frame-1), .combine = "rbind", .packages = c("tidyverse", "rstan")) %dopar% {
  #   
  #   
  #   extract_frame = function(tracking_data, race, frame, fwd_model, lat_model) {
  #     
  #     # Filter for specified frame in race and arrange by horse_id
  #     frame_data = tracking_data %>%
  #       group_by(race_id, horse_id) %>%
  #       arrange(race_id, horse_id, frame_id) %>%
  #       mutate(
  #         race_loc_lag = lag(race_loc, default = NA),
  #         race_loc_lag2 = lag(race_loc, n = 2, default = NA),
  #         dist_to_track_inner_clean_lag = lag(dist_to_track_inner_clean, default = NA),
  #         dist_to_track_inner_clean_lag2 = lag(dist_to_track_inner_clean, n = 2, default = NA)
  #       ) %>%
  #       mutate(
  #         race_loc_lag = ifelse(is.na(race_loc_lag), race_loc, race_loc_lag),
  #         race_loc_lag2 = ifelse(is.na(race_loc_lag2), race_loc, race_loc_lag2),
  #         dist_to_track_inner_clean_lag = ifelse(is.na(dist_to_track_inner_clean_lag), dist_to_track_inner_clean, dist_to_track_inner_clean_lag),
  #         dist_to_track_inner_clean_lag2 = ifelse(is.na(dist_to_track_inner_clean_lag2), dist_to_track_inner_clean, dist_to_track_inner_clean_lag2)
  #       ) %>%
  #       ungroup() %>%
  #       filter(race_id == race & frame_id == frame) %>%
  #       arrange(horse_id)
  #     
  #     # Obtain run up distance
  #     run_up = frame_data$run_up_distance[[1]]
  #     
  #     # Extract stan horse IDs
  #     horse_ids = frame_data %>%
  #       select(horse_name) %>%
  #       left_join(fwd_model$horse_index, by = "horse_name") %>%
  #       select(stan_horse_id) %>%
  #       unlist() %>%
  #       unname()
  #     
  #     # Extract stan jockey IDs
  #     jockey_ids = frame_data %>%
  #       select(jockey) %>%
  #       left_join(fwd_model$jockey_index %>% rename(fwd_jockey = jockey_index), by = "jockey") %>%
  #       left_join(lat_model$jockey_index %>% rename(lat_jockey = jockey_index), by = "jockey") %>%
  #       select(fwd_jockey, lat_jockey)
  #     
  #     # Extract stan track type/condition ID
  #     track_id = frame_data %>%
  #       select(condition_type) %>%
  #       head(1) %>%
  #       left_join(fwd_model$track_index, by = "condition_type") %>%
  #       select(track_index) %>%
  #       unlist() %>%
  #       unname()
  #     
  #     # Extract forward and lateral locations
  #     fwd_locs = frame_data$race_loc
  #     lat_locs = frame_data$dist_to_track_inner_clean
  #     fwd_locs_lag = frame_data$race_loc_lag
  #     lat_locs_lag = frame_data$dist_to_track_inner_clean_lag
  #     fwd_locs_lag2 = frame_data$race_loc_lag2
  #     lat_locs_lag2 = frame_data$dist_to_track_inner_clean_lag2
  #     
  #     # Extract covariates
  #     fwd_covariates = frame_data %>%
  #       select(any_of(fwd_model$covariates)) %>%
  #       mutate_all(as.numeric) %>%
  #       as.matrix()
  #     
  #     lat_covariates = frame_data %>%
  #       select(any_of(lat_model$covariates)) %>%
  #       mutate_all(as.numeric) %>%
  #       as.matrix()
  #     
  #     # Extract track and course
  #     track = frame_data$track_id[[1]]
  #     course = frame_data$course_type[[1]]
  #     
  #     # Extract energy measures
  #     c_energy = frame_data$c_energy
  #     c_max_energy = frame_data$c_max_energy
  #     
  #     return(list(
  #       track_name = track,
  #       course_type = course,
  #       frame_id = frame,
  #       run_up_distance = run_up,
  #       fwd_locs = fwd_locs,
  #       lat_locs = lat_locs,
  #       fwd_locs_lag = fwd_locs_lag,
  #       lat_locs_lag = lat_locs_lag,
  #       fwd_locs_lag2 = fwd_locs_lag2,
  #       lat_locs_lag2 = lat_locs_lag2,
  #       horse_ids = horse_ids,
  #       jockey_ids = jockey_ids,
  #       track_id = track_id,
  #       fwd_covariates = fwd_covariates,
  #       lat_covariates = lat_covariates,
  #       c_energy = c_energy,
  #       c_max_energy = c_max_energy
  #     ))
  #     
  #   }
  #   
  #   
  #   # Prepare the input data for simulations
  #   input_data = extract_frame(tracking_data, race, frame, fwd_model, lat_model)
  #   
  #   # Create list of data to be input into Stan
  #   stan_list = list(
  #     n_race_horses = length(input_data$horse_ids),
  #     n_horses = nrow(fwd_model$horse_index),
  #     n_jockeys_fwd = nrow(fwd_model$jockey_index),
  #     n_jockeys_lat = nrow(lat_model$jockey_index),
  #     n_tracks = nrow(fwd_model$track_index),
  #     n_splines = ncol(spline_matrix),
  #     n_markers = nrow(spline_matrix),
  #     n_fwd_cov = ncol(input_data$fwd_covariates),
  #     n_lat_cov = ncol(input_data$lat_covariates),
  #     start_frame = input_data$frame_id,
  #     race_length = 1609.344,
  #     run_up_distance = run_up,
  #     horse_id_fwd = input_data$horse_ids,
  #     jockey_id_fwd = input_data$jockey_ids$fwd_jockey,
  #     jockey_id_lat = input_data$jockey_ids$lat_jockey,
  #     track_id = input_data$track_id,
  #     initial_fwd_loc = input_data$fwd_locs,
  #     initial_lat_loc = input_data$lat_locs,
  #     initial_fwd_loc_lag = input_data$fwd_locs_lag,
  #     initial_lat_loc_lag = input_data$lat_locs_lag,
  #     initial_fwd_loc_lag2 = input_data$fwd_locs_lag2,
  #     initial_lat_loc_lag2 = input_data$lat_locs_lag2,
  #     initial_y_lag = input_data$y_lag,
  #     spline_matrix = spline_matrix,
  #     initial_fwd_cov = as.matrix(input_data$fwd_covariates),
  #     initial_lat_cov = as.matrix(input_data$lat_covariates),
  #     cum_energy = input_data$c_energy,
  #     cum_max_energy = input_data$c_max_energy,
  #     bend = track_binaries$bend,
  #     turn2hs = track_binaries$turn2hs,
  #     home_stretch = track_binaries$home_stretch,
  #     granularity = granularity
  #   )
  #   
  #   # Run simulations
  #   simulated_races = gqs(generate_races, data = stan_list, draws = param_matrix)
  #   
  #   # Convert to an array
  #   simulation_array = as.array(simulated_races)
  #   
  #   # Unpack array into a list of data frames
  #   simulation_list = lapply(
  #     seq(dim(simulation_array)[1]),
  #     function(x) {
  #       simulation_array[x,1,] %>%
  #         matrix(nrow = 450 - stan_list$start_frame, byrow = FALSE) %>%
  #         as.data.frame() %>%
  #         magrittr::set_colnames(c(
  #           paste("fwd_loc", 1:n_race_horses, sep = ""),
  #           paste("lat_loc", 1:n_race_horses, sep = ""),
  #           paste("c_energy", 1:n_race_horses, sep = ""),
  #           paste("c_max_energy", 1:n_race_horses, sep = "")
  #         )) %>%
  #         mutate(frame_id = stan_list$start_frame + as.numeric(row_number())) %>%
  #         pivot_longer(
  #           cols = -c("frame_id"),
  #           names_to = c(".value", "horse"),
  #           names_pattern = "(.*)(1$|2$|3$|4$|5$|6$|7$|8$|9$|10$|11$|12$)"
  #         )
  #     }
  #   )
  #   
  #   # Prep horse and jockey data to add to the simulation data
  #   horses = fwd_model$horse_index %>% select(horse_name, horse = stan_horse_id)
  #   jockeys = input_data$jockey_ids %>% 
  #     select(jockey_index = fwd_jockey) %>%
  #     left_join(fwd_model$jockey_index %>% select(jockey_index, jockey), by = "jockey_index") %>%
  #     mutate(horse = row_number() %>% as.numeric())
  #   
  #   # Determine the approximate finish time for each horse in each simulation
  #   finishes_df = data.frame(sim_id = 1:n_draws, race_sim = I(simulation_list)) %>%
  #     unnest(race_sim) %>%
  #     ungroup() %>%
  #     mutate(horse = as.numeric(horse)) %>%
  #     left_join(horses, by = "horse") %>%
  #     left_join(jockeys, by = "horse") %>%
  #     mutate(horse_jockey = paste(horse_name, jockey, sep = " / ")) %>%
  #     select(-horse, -horse_name, -jockey) %>%
  #     select(sim_id, horse_jockey, frame_id, everything()) %>%
  #     group_by(sim_id, horse) %>%
  #     arrange(sim_id, horse, frame_id) %>%
  #     mutate(
  #       fwd_lag = lag(fwd_loc, default = 0),
  #       fwd_lag2 = lag(fwd_loc, n = 2, default = 0)
  #     ) %>%
  #     filter(fwd_loc >= 1609.4) %>%
  #     filter(row_number() == 1) %>%
  #     mutate(frame_speed = fwd_lag - fwd_lag2) %>%
  #     mutate(interpolated_frame = frame_id + (fwd_loc - fwd_lag) / (fwd_lag - fwd_lag2)) %>%
  #     mutate(finish_time = (interpolated_frame - 1)/4) %>%
  #     ungroup() %>%
  #     group_by(sim_id) %>%
  #     mutate(finish_place = rank(finish_time)) %>%
  #     ungroup() %>%
  #     select(sim_id, horse, finish_time, finish_place)
  #   
  #   # Aggregate simulations to obtain average finish times
  #   time_summary = finishes_df %>%
  #     group_by(horse) %>%
  #     summarize(mean_time = mean(finish_time), sd = sd(finish_time)) %>%
  #     ungroup()
  #   
  #   # Aggregate simulations to obtain placement probabilities
  #   placement_summary = finishes_df %>%
  #     group_by(horse, finish_place) %>%
  #     summarize(prob = n() / n_draws) %>%
  #     ungroup() %>%
  #     mutate(frame_id = frame) %>%
  #     select(frame_id, horse, finish_place, prob) %>%
  #     left_join(time_summary, by = "horse")
  #   
  #   return(placement_summary)
  #   
  # }
  # 
  # Stop parallelization
  stopCluster(my_cluster)
  
  return(placement_probs)
}




