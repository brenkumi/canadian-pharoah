
##############################################
### STEP 0: LOAD MODELS, DATA AND PACKAGES ###
##############################################

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

# Load models
fwd_model = read_rds("Data/Output/final_fwd_model_no_samples.Rda")
lat_model = read_rds("Data/Output/final_lat_model.Rda")


###########################################
### STEP 1: PREPARE DATA FOR CLUSTERING ###
###########################################

# Obtain jockey ratings for Figure 8 (not relevant for clustering)
race_counts = tracking_data %>%
  select(race_id, jockey) %>%
  unique() %>%
  group_by(jockey) %>%
  tally()

fwd_jockeys = fwd_model$jockey_index %>%
  mutate(fwd_rating = fwd_model$theta_jockey_samples) %>%
  select(-jockey_index)

lat_jockeys =  lat_model$jockey_index %>%
  mutate(lat_rating = colMeans(lat_model$theta_jockey_samples)) %>%
  select(-jockey_index)

all_jockeys = fwd_jockeys %>%
  left_join(lat_jockeys, by = c("jockey", "jockey_id")) %>%
  left_join(race_counts, by = "jockey")


# Count number of races each horse appears in
race_counts_horse = tracking_data %>%
  select(race_id, horse_name) %>%
  unique() %>%
  group_by(horse_name) %>%
  tally()

# Average covariates at each time step
covariates_avg = tracking_data %>%
  mutate(distance_travelled = round(distance_travelled)) %>%
  select(
    distance_travelled, n_horses_fwd, n_horses_bwd, n_horses_inside, n_horses_outside,
    nearest_fwd, nearest_inside, nearest_outside, nearest_inside_euclid, nearest_outside_euclid,
    bend, drafting_int, distance_prev, inside_movement, prop_energy_saved_x_hs
  ) %>%
  group_by(distance_travelled) %>%
  summarize_all(mean) %>%
  ungroup() %>%
  mutate_at(vars(-("distance_travelled")), ~ifelse(is.na(frollmean(.x, n = 3, fill = )), .x, frollmean(.x, n = 3, fill = )))

# Find mean jockey rating for each horse
horse_jockey_combo = tracking_data %>%
  select(race_id, horse_name, jockey) %>%
  unique() %>%
  group_by(horse_name, jockey) %>%
  tally() %>%
  left_join(all_jockeys %>% select(-n), by = "jockey") %>%
  ungroup() %>%
  group_by(horse_name) %>%
  summarize(mean_jockey = mean(n * fwd_rating / sum(n)))

# Find mean track effect for each horse
track_effects = fwd_model$track_index %>%
  mutate(track_effect = fwd_model$theta_track_samples)

track_types = tracking_data %>%
  select(race_id, horse_name, condition_type) %>%
  unique() %>%
  group_by(horse_name, condition_type) %>%
  tally() %>%
  left_join(track_effects, by = "condition_type") %>%
  ungroup() %>%
  group_by(horse_name) %>%
  summarize(mean_track = mean(n * track_effect / sum(n)))

# Obtain spline for each horse
beta_mat = matrix(fwd_model$beta_samples, nrow = nrow(fwd_model$horse_index), byrow = TRUE) 

# Obtain average linear predictor for each horse across knots
horse_effects = beta_mat %>%
  as.data.frame() %>%
  bind_cols(fwd_model$horse_index) %>%
  mutate(avg_first3 = rowMeans(select(., V1, V2, V3), na.rm = TRUE)) %>%
  mutate(avg_mid3 = rowMeans(select(., V4, V5, V6), na.rm = TRUE)) %>%
  mutate(avg_end3 = rowMeans(select(., V7, V8, V9), na.rm = TRUE)) %>%
  left_join(race_counts_horse, by = "horse_name") %>%
  filter(n >= 5) %>%
  left_join(horse_jockey_combo, by = "horse_name") %>%
  mutate(across(V1:V9, ~.x + mean_jockey)) %>%
  left_join(track_types, by = "horse_name") %>%
  mutate(across(V1:V9, ~.x + mean_track)) %>%
  mutate(avg = rowMeans(select(., starts_with("V")), na.rm = TRUE)) %>%
  rowwise() %>%
  mutate(sd = sd(c(V1, V2, V3, V4, V5, V6, V7, V8, V9))) %>%
  ungroup()

# Convert knot coefficients to distance matrix
dist_matrix = horse_effects %>%
  select(V1, V2, V3, V4, V5, V6, V7, V8, V9) %>%
  dist()

# Perform hierarchical clustering
hier_clustering = hclust(dist_matrix, method = "ward.D2")
hier_clustering$labels = horse_effects$horse_name

# Plot dendrogram
plot(hier_clustering, hang = -1, cex = 0.5)
rect.hclust(hier_clustering, k = 4, border = "red")

# Obtain cluster assignments
cluster_assignments = cutree(hier_clustering, k = 3)

# Save cluster results
clusters_df = data.frame(horse_name = names(cluster_assignments), cluster = cluster_assignments)

# Add in clusters to full data frame
all_data = horse_effects %>%
  left_join(clusters_df, by = "horse_name")

# Set knots
knots <- c(90, 250, 800, 1207, 1375)
degree <- 3
boundary_knots <- c(0, 1650)

x_increment = 1
race_meters = 1609.344
x <- seq(from = 0, to = race_meters, by = x_increment)

x_spline <- splines2::bSpline(
  x = x,
  knots = knots,
  Boundary.knots = boundary_knots,
  degree = degree,
  intercept = TRUE
)



plot_df <- foreach::foreach(i = 1:nrow(all_data),.combine = rbind)%do%{
  xbeta <- x_spline%*%beta_mat[i,]
  out <- data.frame(speed_original_scale = xbeta[,1]/0.25, distance_travelled = x,
                    horse_name = all_data$horse_name[i])
  out$speed_kph <- out$speed_original_scale*3.6
  out$horse_name <- as.factor(out$horse_name)
  
  out
}


dirt_vs_turf = tracking_data %>%
  mutate(is_dirt = ifelse(course_type == "D", "dirt", "turf")) %>%
  select(race_id, horse_name, is_dirt) %>%
  unique() %>%
  group_by(horse_name, is_dirt) %>%
  tally() %>%
  pivot_wider(id_cols = horse_name, names_from = is_dirt, values_from = n) %>%
  mutate(
    dirt = replace_na(dirt, 0),
    turf = replace_na(turf, 0)
  ) %>%
  mutate(is_dirt = dirt > turf) %>%
  mutate(prop_dirt = (dirt + 1) / (dirt + turf + 2))

dirt_horses = dirt_vs_turf %>%
  select(horse_name, is_dirt)

plot_df2 = plot_df %>%
  group_by(horse_name) %>%
  nest() %>%
  left_join(all_data, by = "horse_name") %>%
  left_join(dirt_horses, by = "horse_name") %>%
  unnest() %>%
  ungroup() %>%
  left_join(covariates_avg, by = "distance_travelled") %>%
  mutate(covariates = 0.323*n_horses_fwd + 0.310*n_horses_bwd + 0.173*nearest_outside + 0.091*bend +
           drafting_int*0.067 + n_horses_outside*(-0.024) + n_horses_inside*(-0.037) + inside_movement * (-0.039) + nearest_inside*(-0.185) +
           prop_energy_saved_x_hs*(-1.596)) %>%
  group_by(horse_name) %>%
  mutate(covariates = loess(covariates ~ distance_travelled)$fitted) %>%
  ungroup() %>%
  mutate(speed_original_scale = speed_original_scale + covariates) %>%
  group_by(horse_name) %>%
  filter(!any(speed_original_scale > 8)) %>%
  ungroup()

mean_curves = plot_df2 %>%
  group_by(cluster, distance_travelled) %>%
  summarize(mean = mean(speed_original_scale)) %>%
  ungroup()

horse_order = horse_effects$horse_name[hier_clustering$order]


ggplot(plot_df2) +
  geom_tile(aes(x = distance_travelled, y = factor(horse_name, levels = horse_order), fill = speed_original_scale)) +
  scale_fill_viridis_c() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  theme_bw()


ggplot(plot_df2) +
  geom_path(aes(x = distance_travelled, y = speed_original_scale * (3.6245/4), colour = is_dirt, group = horse_name), size = 0.6) +
  geom_path(data = mean_curves, aes(x = distance_travelled, y = mean), size = 1.2) +
  theme_bw() +
  labs(x = "Distance Travelled in Race (m)", y = "Impact on Speed Relative to Average Horse (m/s)", colour = "Horse") +
  facet_wrap(~cluster) +
  theme(legend.position = "none")

cluster_centres = mean_curves %>%
  filter(cluster != 3) %>%
  mutate(cluster_name = case_when(
    cluster == 1 ~ "Strong Start, Slow Finish",
    cluster == 2 ~ "Slow Start, Strong Finish",
    cluster == 4 ~ "Medium Start, Medium Finish"
  )) %>%
  group_by(cluster_name) %>%
  mutate(mean2 = loess(mean~distance_travelled)$fitted, span = 0.01) %>%
  ggplot() +
  #geom_path(aes(x = distance_travelled, y = speed_original_scale, colour = is_dirt, group = horse_name), size = 0.6) +
  geom_path(aes(x = distance_travelled, y = mean * 4 * (3.6245/4), colour = factor(cluster_name)), size = 1.2) +
  theme_bw() +
  labs(x = "Distance Travelled in Race (m)", y = "Impact on Speed (m/s)", colour = "Horse")

ggsave("Figures/Writeup/horse_profile_cluster_centres.png", cluster_centres, height = 7, width = 12)

cluster_centres_ind = cluster_centres +
  facet_wrap(~cluster_name, nrow = 2) +
  theme(strip.text = element_text(size = 18))

ggsave("Figures/Writeup/horse_profile_cluster_centres_ind.png", cluster_centres_ind, height = 7, width = 12)


