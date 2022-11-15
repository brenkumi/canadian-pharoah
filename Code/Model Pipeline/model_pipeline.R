
####################################
## STEP 0: LOAD DATA AND PACKAGES ##
####################################

# Load packages
library(tidyverse)
library(rstan)
library(parallel)
library(foreach)
library(splines2)
library(data.table)
library(tictoc)
library(beepr)
library(truncnorm)
library(doParallel)

# Load data
tracking_data = read_csv("Data/Prepped Input/tracking_data_cleaned.csv") %>%
  group_by(race_id) %>%
  filter(!any(dnf)) %>%
  ungroup() %>%
  filter(!(race_id %in% c("AQU_20191117_6", "AQU_20190302_9", "AQU_20190315_6")))

# Load data visualization functions
source("Code/Model Pipeline/model_pipeline_functions.R")



#####################################
## STEP 1: FIT SIDE MOVEMENT MODEL ##
#####################################

# Number of observations used to fit model
n_fit = 900000


# Set the covariates for the forward model (subset of covariates for side model)
covariates_fwd = c(
  "inside_movement",
  #"distance_prev",
  "n_horses_inside",
  "n_horses_outside",
  "n_horses_fwd",
  "n_horses_bwd",
  "nearest_inside",
  "nearest_outside",
  "nearest_inside_euclid",
  "nearest_outside_euclid",
  "nearest_fwd",
  "prop_energy_saved_x_hs",
  "drafting_int",
  "bend"
)

# Add in a couple extra for the side model
covariates_side = append(covariates_fwd, c("home_stretch", "turn2hs"))

# Fit the side movement model
tic()
side_model = fit_side_model(
  df = tracking_data,
  n_fit = n_fit,
  exclude_tol = 1, # How much of a jump should be considered outlier?
  fit_type = "optim", # This is basically like sampling from a ridge regression model
  draws = 2000,
  chains = 1,
  fit_lm = FALSE,
  covariates = covariates_side
)
toc()

beep()

# Extract samples from the fitted model
side_stan_model <- side_model$stan_model
side_posterior_means = side_model$mean_params
side_parameter_matrix = side_model$parameter_matrix
side_beta_lag_samples <- side_model$beta_lag_samples
side_beta_samples <- side_model$beta_samples
side_sigma_samples <- side_model$sigma_samples
side_theta_track_samples = side_model$theta_track_samples
side_theta_jockey_samples = side_model$theta_jockey_samples
side_track_index = side_model$track_index
side_jockey_index = side_model$jockey_index

# Create data frame of covariates and random effect posterior means
side_posterior_cov_df = data.frame(
  covariates = covariates_side,
  mean = colMeans(side_beta_samples)
)

side_posterior_track_df = side_track_index %>%
  mutate(mean = colMeans(side_theta_track_samples))

side_posterior_jockey_df = side_jockey_index %>%
  mutate(mean = colMeans(side_theta_jockey_samples))


#write_rds(side_model, "Data/Output/final_lat_model.Rda")



########################################
## STEP 2: FIT FORWARD MOVEMENT MODEL ##
########################################

# Set knots
knots <- c(90, 250, 800, 1207, 1375)
degree <- 3
boundary_knots <- c(0, 1650)
beta_prior = c(-1.22, 0.14, 0.50, 0.48, 0.32, 0.17, 0.08, -0.07, -0.21)

# Get vector of unique races
all_races = unique(tracking_data$race_id)

# Fit forward movement model
tic()
fwd_model = fit_forward_model(
  df = tracking_data,
  knots = knots,
  boundary_knots = boundary_knots,
  degree = degree,
  beta_prior = beta_prior,
  races = all_races[1:20],
  covariates = covariates_fwd,
  fit_type = "optim",
  n_draws = 0,
  chains = 1,
  cores = 1,
  max_iter = 15000,
  lite = TRUE,
  hess = F
)
toc()
beep()

# Extarct posterior samples
fwd_param_matrix = fwd_model$param_matrix
fwd_beta_samples = fwd_model$beta_samples
fwd_gamma_samples = fwd_model$gamma_samples
fwd_sigma_samples = fwd_model$sigma_samples
fwd_theta_track_samples = fwd_model$theta_track_samples
fwd_theta_jockey_samples = fwd_model$theta_jockey_samples
fwd_stan_df = fwd_model$stan_df
fwd_tracks = fwd_model$track_index
fwd_jockeys = fwd_model$jockey_index
fwd_horses = fwd_model$horse_index

# Create data frame of covariates and random effect posterior means
fwd_posterior_cov_df = data.frame(
  covariates = covariates_fwd,
  mean = colMeans(fwd_gamma_samples)
)

fwd_posterior_track_df = fwd_tracks %>%
  mutate(mean = colMeans(fwd_theta_track_samples))

fwd_posterior_jockey_df = fwd_jockeys %>%
  mutate(mean = colMeans(fwd_theta_jockey_samples))

# Get horse IDs from a selected race
prace_id = all_races[1]
sample_horse_ids = fwd_model$stan_horse_ids

horse_ids = fwd_stan_df %>%
  ungroup() %>%
  filter(race_id == prace_id) %>%
  select(stan_horse_id) %>%
  unique() %>%
  unlist() %>%
  unname()

# Collect the right betas for the chosen horses
beta_list = get_beta_list(
  beta_matrix = fwd_beta_samples,
  horse_ids = horse_ids
)

beta_prior = colMeans(matrix(colMeans(fwd_beta_samples), ncol = 9, byrow = TRUE))


#write_rds(fwd_model, "Data/Output/final_fwd_model_with_samples.Rda")
#write_rds(fwd_model, "Data/Output/final_fwd_model_no_samples.Rda")


#############################################
## STEP 3: SIMULATE RACE AT A SINGLE FRAME ##
#############################################

# Jitter and expand fwd_model if necessary
if (nrow(fwd_model$param_matrix) == 1) {
  fwd_model = jitter_fwd_model(fwd_model)
}

# Set up data for simulations...

# ... from certain point of a real race
input_data = extract_frame(
  tracking_data = tracking_data,
  race = tracking_data$race_id[[1]],
  frame = 250,
  fwd_model = fwd_model,
  lat_model = side_model
)

# ... for hypothetical race from start
input_data = initialize_frame(
  track = "BEL",
  course = "D",
  condition = "muddy",
  run_up = 20,
  horses = tracking_data %>% arrange(race_id, frame_id) %>% head(5) %>% select(horse_name) %>% unlist() %>% unname(),
  jockeys = tracking_data %>% arrange(race_id, frame_id) %>% head(5) %>% select(jockey) %>% unlist() %>% unname(),
  fwd_model = fwd_model,
  lat_model = side_model
)

# Simulate a race
tic()
race_simulation = simulate_race(
  input_data = input_data,
  fwd_model = fwd_model,
  lat_model = side_model,
  n_draws = 2000
)
toc()

write_rds(race_simulation, "Data/Output/race_simulation.Rda")

# Extract data from race simulations
finishes_df = race_simulation$all_finishes
placement_summary = race_simulation$aggregated_placements


# Create ridge plot of finishing times
ggplot(finishes_df) +
  ggridges::geom_density_ridges_gradient(aes(x = finish_time, y = horse_jockey, fill = stat(x)), colour = "grey50") +
  scale_x_reverse() +
  scale_fill_viridis_c(
    option = "magma",
    guide = guide_colorbar(barwidth = 0.8, barheight = 14)
  ) +
  theme_bw() +
  labs(x = "Finish Time (Seconds)", y = "") +
  theme(legend.position = "none")

# Complete placement summary to fill in NAs
placement_complete = expand.grid(horse_jockey = unique(placement_summary$horse_jockey), finish_place = unique(placement_summary$finish_place)) %>%
  left_join(placement_summary, by = c("horse_jockey", "finish_place"))

# Create square heatmap of placement probabilities
ggplot(placement_complete) +
  geom_tile(aes(x = factor(finish_place), y = factor(horse_jockey), fill = prob_mask), colour = "grey70") +
  scale_fill_viridis_c(
    breaks = c(0.00001, 0.5, 1, 1.5, 1.9), limits= c(0,1.9),
    labels = c("0", "0.05", "0.1", "0.5", "1"), option = "magma",
    guide = guide_colorbar(barwidth = 0.8, barheight = 14),
    na.value = "black"
  ) +
  scale_y_discrete(limits=rev, expand = c(0,0)) +
  scale_x_discrete(expand = c(0,0), breaks = 1:n_race_horses) +
  theme_bw() +
  labs(x = "Finishing Place", y = "", fill = "Probability") +
  theme(axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 10)) +
  coord_equal()



##########################################
## STEP 4: SIMULATE RACE AT EVERY FRAME ##
##########################################

race = "BEL_20190516_3"

full_race_simulations = data.frame()

for (i in 211:355) {
  
  print(i)
  
  # Set up race at frame
  input_data = extract_frame(
    tracking_data = tracking_data,
    race = race,
    frame = i,
    fwd_model = fwd_model,
    lat_model = side_model
  )
  
  # Simulate from that frame
  tic()
  temp_sim = simulate_race(
    input_data = input_data,
    fwd_model = fwd_model,
    lat_model = side_model,
    n_draws = 2000
  )$aggregated_placements %>%
    mutate(frame_id = i)
  toc()
  
  full_race_simulations = bind_rows(full_race_simulations, temp_sim)
  
}

beep()

final_order = tracking_data %>%
  filter(race_id == race) %>%
  mutate(horse_jockey = paste(horse_name, jockey, sep = " / ")) %>%
  select(horse_jockey, finishing_place) %>%
  unique() %>%
  arrange(finishing_place)

possible_placements = final_order %>%
  rename(actual_placement = finishing_place) %>%
  mutate(sim = map(horse_jockey, ~data.frame(simulated_placement = 1:7))) %>%
  unnest() %>%
  ungroup() %>%
  mutate(frame_id = map(horse_jockey, ~data.frame(frame_id = min(full_race_simulations$frame_id):max(full_race_simulations$frame_id)))) %>%
  unnest() %>%
  ungroup()


all_horse_frames = full_race_simulations %>%
  select(frame_id) %>%
  unique() %>%
  mutate(data = map(frame_id, ~final_order)) %>%
  unnest() %>%
  ungroup() %>%
  rename(actual_placement = finishing_place) %>%
  left_join(full_race_simulations %>% rename(simulated_placement = finish_place), by = c("frame_id", "horse_jockey")) %>%
  mutate(is_finished = is.na(simulated_placement)) %>%
  mutate(simulated_placement = ifelse(is_finished, actual_placement, simulated_placement)) %>%
  group_by(frame_id) %>%
  mutate(count_fin = sum(is_finished)) %>%
  ungroup() %>%
  mutate(simulated_placement = ifelse(is_finished, actual_placement, count_fin + simulated_placement)) %>%
  mutate(prob = ifelse(is_finished, 1, prob))

final = possible_placements %>%
  left_join(all_horse_frames, by = c("frame_id", "horse_jockey", "actual_placement", "simulated_placement")) %>%
  mutate(prob = ifelse(is.na(prob), 0, prob)) %>%
  mutate(prob_mask = ifelse(prob <= 0.1, prob*10, prob + 0.9)) %>%
  mutate(horse_jockey = factor(horse_jockey, levels = final_order$horse_jockey)) %>%
  mutate(simulated_placement = factor(simulated_placement))

ggplot(final) +
  geom_tile(aes(x = factor(simulated_placement), y = horse_jockey, fill = prob_mask), colour = "grey70") +
  scale_fill_viridis_c(
    breaks = c(0.00001, 0.5, 1, 1.5, 1.9), limits= c(0,1.9),
    labels = c("0", "0.05", "0.1", "0.5", "1"), option = "magma",
    guide = guide_colorbar(barwidth = 0.8, barheight = 14),
    na.value = "black"
  ) +
  scale_y_discrete(limits=rev, expand = c(0,0)) +
  scale_x_discrete(expand = c(0,0), breaks = 1:n_race_horses) +
  theme_bw() +
  labs(x = "Finishing Place", y = "", fill = "Probability") +
  theme(axis.text.x = element_text(size = 9), axis.text.y = element_text(size = 10)) +
  coord_equal() +
  transition_time(frame_id)



write_csv(final, "Data/Output/full_race_simulation_updated.csv")
