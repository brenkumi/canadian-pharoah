
######################################
### STEP 0: LOAD DATA AND PACKAGES ###
######################################

# Load packages
library(tidyverse)
library(gganimate)
library(ggforce)
library(geosphere)
library(foreach)
library(parallel)
library(doParallel)
library(tictoc)

# Source custom functions for data prep and plotting
source("Code/Data Preparation/data_cleaning_functions.R")

# Load race tracking data
tracking_data = read_csv("Data/Input/nyra_2019_complete.csv")

# Load track outlines for our three race tracks
track_outlines = read_csv("Data/Input/track_outlines.csv")

# Load in horse names and IDs
horse_names = read_csv("Data/Input/horse_names.csv")
write.csv(horse_names, "Data/Input/horse_names.csv", row.names = FALSE)

horse_ids = read_csv("Data/Input/horse_ids.csv")
write.csv(horse_ids, "Data/Input/horse_ids.csv", row.names = FALSE)

# Create jockey ids
jockey_ids = tracking_data %>%
  select(jockey) %>%
  unique() %>%
  arrange(jockey) %>%
  rownames_to_column("jockey_id") %>%
  mutate(jockey_id = as.numeric(jockey_id))

write.csv(jockey_ids, "Data/Input/jockey_ids.csv", row.names = FALSE)



######################################
### STEP 1: BASIC DATA PREPARATION ###
######################################

# In this section, I perform some preliminary data prep to make things easier to work with
# from our perspective

tracking_data_s1 = tracking_data %>%
  # Join in horse names and placements
  left_join(horse_ids, by = c("track_id", "race_date", "race_number" = "race", "program_number")) %>%
  left_join(horse_names, by = "horse_id") %>%
  left_join(jockey_ids, by = "jockey") %>%
  # Clean up and arrange the data
  select(-program_number) %>%
  rename(frame_id = trakus_index) %>%
  arrange(track_id, race_date, race_number, horse_id, frame_id) %>%
  # Create a unique race ID
  mutate(race_id = paste(track_id, str_replace_all(race_date, "-", ""), race_number, sep = "_")) %>%
  # Prepare our tracking data and convert distances to metres
  mutate(
    race_distance_furlongs = distance_id / 100, # Convert distance to furlongs
    race_distance_metres = 201.168 * race_distance_furlongs, # Convert distance to metres
    run_up_distance = run_up_distance * 0.3048 # Convert run-up distance to metres
  ) %>%
  # Determine number of horses in the race
  group_by(race_id) %>%
  mutate(n_horses = horse_id %>% unique() %>% length()) %>%
  ungroup() %>%
  # Add in elevation (found online)
  mutate(elevation = case_when(
    track_id == "AQU" ~ 3,
    track_id == "BEL" ~ 25,
    track_id == "SAR" ~ 93
  )) %>%
  # Adjust course_type to be consistent across tracks
  mutate(course_type = case_when(
    track_id == "AQU" & course_type == "T" ~ "I",
    track_id == "BEL" & course_type == "T" ~ "O",
    track_id == "SAR" & course_type == "T" ~ "O",
    TRUE ~ course_type
  )) %>%
  # Remove hurdle races for now
  filter(course_type != "M") %>%
  # Remove problem race(s)
  #filter(!(race_id %in% c("SAR_20190724_2", "SAR_20190717_8"))) %>%
  #filter(race_type == "STK") %>%
  select(race_id, track_id, race_date, race_number, horse_name, horse_id, finishing_place, everything()) %>%
  select(-distance_id)



########################################################
### STEP 2: CONVERT LONGITUDE AND LATITUDE TO METRES ###
########################################################

# To make the data more digestible, we'll convert coordinates from longitude/latitude
# to metres using Haversine distance, a method for finding the distance between two points
# on a sphere.

# Find Haversine translation factors for each track
aqueduct_translations = tracking_data_s1 %>%
  filter(track_id == "AQU") %>%
  find_haversine_translations()

belmont_translations = tracking_data_s1 %>%
  filter(track_id == "BEL") %>%
  find_haversine_translations()

saratoga_translations = tracking_data_s1 %>%
  filter(track_id == "SAR") %>%
  find_haversine_translations()


# Define origin for each track (the point we want to be x=0, y=0)
aqueduct_origin = c("longitude" = -73.830172, "latitude" = 40.666620)

belmont_origin = c("longitude" = -73.714667, "latitude" = 40.715200)

saratoga_origin = c("longitude" = -73.77205, "latitude" = 43.06852)


# Convert coordinates from longitude/latitude to x/y in metres for tracking data
tracking_data_s2 = tracking_data_s1 %>%
  mutate(
    x = case_when(
      track_id == "AQU" ~ (longitude - aqueduct_origin["longitude"]) * aqueduct_translations["longitude"],
      track_id == "BEL" ~ (longitude - belmont_origin["longitude"]) * belmont_translations["longitude"],
      track_id == "SAR" ~ (longitude - saratoga_origin["longitude"]) * saratoga_translations["longitude"]
    ),
    y = case_when(
      track_id == "AQU" ~ (latitude - aqueduct_origin["latitude"]) * aqueduct_translations["latitude"],
      track_id == "BEL" ~ (latitude - belmont_origin["latitude"]) * belmont_translations["latitude"],
      track_id == "SAR" ~ (latitude - saratoga_origin["latitude"]) * saratoga_translations["latitude"]
    )
  ) %>%
  select(-longitude, -latitude)

# Convert coordinates from longitude/latitude to x/y in metres for track outlines
track_outlines_s2 = track_outlines %>%
  mutate(
    x = case_when(
      track_id == "AQU" ~ (longitude - aqueduct_origin["longitude"]) * aqueduct_translations["longitude"],
      track_id == "BEL" ~ (longitude - belmont_origin["longitude"]) * belmont_translations["longitude"],
      track_id == "SAR" ~ (longitude - saratoga_origin["longitude"]) * saratoga_translations["longitude"]
    ),
    y = case_when(
      track_id == "AQU" ~ (latitude - aqueduct_origin["latitude"]) * aqueduct_translations["latitude"],
      track_id == "BEL" ~ (latitude - belmont_origin["latitude"]) * belmont_translations["latitude"],
      track_id == "SAR" ~ (latitude - saratoga_origin["latitude"]) * saratoga_translations["latitude"]
    )
  ) %>%
  select(-longitude, -latitude)



######################################################################################
### STEP 3: CALCULATE SPEED, ACCELERATION, ANGLES, DISTANCES AND EXPECTED LOCATION ###
######################################################################################

# Here we calculate some basic metrics like speed, acceleration, angles and expected
# location for the horses

tracking_data_s3 = tracking_data_s2 %>%
  arrange(race_id, horse_id, frame_id) %>%
  # Fix some oddities in the data (fixed here aren't great but are more compatible with my smoothing)
  mutate(
    x = ifelse(race_id == "AQU_20191108_9" & horse_id == 1842 & frame_id >= 344 & frame_id <= 387, -54.4136725, x),
    y = ifelse(race_id == "AQU_20191108_9" & horse_id == 1842 & frame_id >= 344 & frame_id <= 387, 622.7861, y)
  ) %>%
  mutate(
    x = ifelse(race_id == "BEL_20190619_9" & horse_id == 194 & frame_id == 374, -625.6451, x),
    y = ifelse(race_id == "BEL_20190619_9" & horse_id == 194 & frame_id == 374, -241.44444, y),
    x = ifelse(race_id == "BEL_20190619_9" & horse_id == 194 & frame_id >= 358 & frame_id < 374, -662.8984, x),
    y = ifelse(race_id == "BEL_20190619_9" & horse_id == 194 & frame_id >= 358 & frame_id < 374, -227.54306, y)
  ) %>%
  filter(
    !(race_id == "AQU_20191117_7" & frame_id >= 425) &
      !(race_id == "AQU_20191228_6" & frame_id >= 350) &
      !(race_id == "BEL_20190619_9" & frame_id >= 375) &
      !(race_id == "SAR_20190822_4" & frame_id >= 475) &
      !(race_id == "SAR_20190811_10" & frame_id >= 410) &
      !(race_id == "AQU_20191101_2" & frame_id >= 330) &
      !(race_id == "AQU_20191207_3" & frame_id >= 330)
  ) %>%
  group_by(race_id, horse_id) %>%
  # Get xy-location at previous and next 4 frames
  mutate(
    frames_remaining = max(frame_id) + 1 - frame_id,
    x_fwd = case_when(
      frames_remaining > 4 ~ lead(x,4),
      frames_remaining == 4 ~ lead(x,3),
      frames_remaining == 3 ~ lead(x,2),
      frames_remaining == 2 ~ lead(x,1),
      frames_remaining == 1 ~ x
    ),
    y_fwd = case_when(
      frames_remaining > 4 ~ lead(y,4),
      frames_remaining == 4 ~ lead(y,3),
      frames_remaining == 3 ~ lead(y,2),
      frames_remaining == 2 ~ lead(y,1),
      frames_remaining == 1 ~ y
    ),
    x_back = case_when(
      frame_id > 4 ~ lag(x,4),
      frame_id == 4 ~ lag(x,3),
      frame_id == 3 ~ lag(x,2),
      frame_id == 2 ~ lag(x,1),
      frame_id == 1 ~ x
    ),
    y_back = case_when(
      frame_id > 4 ~ lag(y,4),
      frame_id == 4 ~ lag(y,3),
      frame_id == 3 ~ lag(y,2),
      frame_id == 2 ~ lag(y,1),
      frame_id == 1 ~ y
    ),
    # Calculate speed over +/- 4 frames
    speed = case_when(
      frames_remaining > 4 & frame_id > 4 ~ sqrt((lag(x,4) - lead(x,4))^2 + (lag(y,4) - lead(y,4))^2) / 2.25,
      frames_remaining == 4 ~ sqrt((lag(x,4) - lead(x,3))^2 + (lag(y,4) - lead(y,3))^2) / 2,
      frames_remaining == 3 ~ sqrt((lag(x,4) - lead(x,2))^2 + (lag(y,4) - lead(y,2))^2) / 1.75,
      frames_remaining == 2 ~ sqrt((lag(x,4) - lead(x,1))^2 + (lag(y,4) - lead(y,1))^2) / 1.5,
      frames_remaining == 1 ~ sqrt((lag(x,4) - x)^2 + (lag(y,4) - y)^2) / 1.25,
      frame_id == 4 ~ sqrt((lag(x,3) - lead(x,4))^2 + (lag(y,3) - lead(y,4))^2) / 2,
      frame_id == 3 ~ sqrt((lag(x,2) - lead(x,4))^2 + (lag(y,2) - lead(y,4))^2) / 1.75,
      frame_id == 2 ~ sqrt((lag(x,1) - lead(x,4))^2 + (lag(y,1) - lead(y,4))^2) / 1.5,
      frame_id == 1 ~ sqrt((x - lead(x,4))^2 + (y - lead(y,4))^2) / 1.25
    ),
    # Calculate acceleration over +/- 4 frames
    acceleration = case_when(
      frames_remaining > 4 & frame_id > 4 ~ (lead(speed,4) - lag(speed,4)) / 2.25,
      frames_remaining == 4 ~ (lead(speed,3) - lag(speed,4)) / 2,
      frames_remaining == 3 ~ (lead(speed,2) - lag(speed,4)) / 1.75,
      frames_remaining == 2 ~ (lead(speed,1) - lag(speed,4)) / 1.5,
      frames_remaining == 1 ~ (speed - lag(speed,4)) / 1.25,
      frame_id == 4 ~ (lead(speed,4) - lag(speed,3)) / 2,
      frame_id == 3 ~ (lead(speed,4) - lag(speed,2)) / 1.75,
      frame_id == 2 ~ (lead(speed,4) - lag(speed,1)) / 1.5,
      frame_id == 1 ~ (lead(speed,4) - speed) / 1.25
    ),
    # Calculate distance travelled
    distance_prev = sqrt((lag(x) - x)^2 + (lag(y) - y)^2) %>% replace_na(0),
    distance_next = sqrt((lead(x) - x)^2 + (lead(y) - y)^2) %>% replace_na(0),
    distance_travelled = cumsum(distance_prev)
  ) %>%
  rowwise() %>%
  mutate(angle = atan2(y_fwd - y_back, x_fwd - x_back) * 180 / pi) %>%
  mutate(angle = ifelse(angle < 0, 360 + angle, angle)) %>%
  ungroup() %>%
  select(-x_fwd, -x_back, -y_fwd, -y_back, -frames_remaining)



###################################################################
### STEP 4: REORIENT TRACK SO THAT STRAIGHTAWAYS ARE HORIZONTAL ###
###################################################################

# Determine the straightaway angle for each of the 3 tracks
aqueduct_straightaway = find_straightaway_angle(tracking_data_s3, "AQU")

belmont_straightaway = find_straightaway_angle(tracking_data_s3, "BEL")

saratoga_straightaway = find_straightaway_angle(tracking_data_s3, "SAR")


# Convert xy-coordinates so that straightaways are horizontal
aqueduct_reoriented_data = tracking_data_s3 %>%
  filter(track_id == "AQU") %>%
  bind_cols(reorient_coordinates(.$x, .$y, .$angle, 180 + aqueduct_straightaway)) %>%
  select(-x, -y, -angle) %>%
  rename(x = x_reor, y = y_reor, angle = angle_reor)

belmont_reoriented_data = tracking_data_s3 %>%
  filter(track_id == "BEL") %>%
  bind_cols(reorient_coordinates(.$x, .$y, .$angle, 180 + belmont_straightaway)) %>%
  select(-x, -y, -angle) %>%
  rename(x = x_reor, y = y_reor, angle = angle_reor)

saratoga_reoriented_data = tracking_data_s3 %>%
  filter(track_id == "SAR") %>%
  bind_cols(reorient_coordinates(.$x, .$y, .$angle, 180 + saratoga_straightaway)) %>%
  select(-x, -y, -angle) %>%
  rename(x = x_reor, y = y_reor, angle = angle_reor)

tracking_data_s4 = aqueduct_reoriented_data %>%
  bind_rows(belmont_reoriented_data) %>%
  bind_rows(saratoga_reoriented_data) %>%
  # Obtain projected location over next second
  mutate(
    x_proj = x - cos(angle * pi / 180) * speed / 4,
    y_proj = y - sin(angle * pi / 180) * speed / 4
  )

# Convert xy-coordinates so that straightaways are horizontal in track outline
aqueduct_reoriented_track = track_outlines_s2 %>%
  filter(track_id == "AQU") %>%
  bind_cols(reorient_coordinates(.$x, .$y, 0, 180 + aqueduct_straightaway)) %>%
  select(-x, -y, -angle_reor) %>%
  rename(x = x_reor, y = y_reor)

belmont_reoriented_track = track_outlines_s2 %>%
  filter(track_id == "BEL") %>%
  bind_cols(reorient_coordinates(.$x, .$y, 0, 180 + belmont_straightaway)) %>%
  select(-x, -y, -angle_reor) %>%
  rename(x = x_reor, y = y_reor)

saratoga_reoriented_track = track_outlines_s2 %>%
  filter(track_id == "SAR") %>%
  bind_cols(reorient_coordinates(.$x, .$y, 0, 180 + saratoga_straightaway)) %>%
  select(-x, -y, -angle_reor) %>%
  rename(x = x_reor, y = y_reor)

track_outlines_s4 = aqueduct_reoriented_track %>%
  bind_rows(belmont_reoriented_track) %>%
  bind_rows(saratoga_reoriented_track)







######################################################
## STEP 5: CALCULATE RANK ON A FRAME-BY-FRAME BASIS ##
######################################################

# Extract and reformat track finish lines
finish_lines = track_outlines_s4 %>%
  arrange(track_id) %>%
  filter(outline_type == "finish_line") %>%
  mutate(dummy = "finish", dummy2 = rep(1:2,9)) %>%
  pivot_wider(id_cols = c(track_id, course_type), names_from = c(dummy, dummy2), values_from = c(x,y))

# Create vector of races where the horses crosses the finish line twice
double_crosses = c(
  "AQU_20191103_8", "AQU_20191107_6", "AQU_20191117_4",
  "AQU_20191123_9", "AQU_20191130_9",
  "BEL_20190607_10", "SAR_20190718_4", "SAR_20190719_7",
  "SAR_20190725_4", "SAR_20190727_10", "SAR_20190728_10",
  "SAR_20190801_9", "SAR_20190804_5", "SAR_20190817_5",
  "SAR_20190824_10", "SAR_20190825_3", "SAR_20190831_10"
)

# Create vector of races where there is a glitchy finish
glitch_finishes = c("BEL_20190619_9", "SAR_20190822_4")

# Get ranks both after finish and throughout race
tracking_data_s5 = tracking_data_s4 %>%
  # Determine when each horse crossed the finish line
  left_join(finish_lines, by = c("track_id", "course_type")) %>%
  arrange(race_id, horse_id, frame_id) %>%
  group_by(race_id, horse_id) %>%
  mutate(prev_x = lag(x), prev_y = lag(y)) %>%
  mutate(cross_finish = check_intersection(x, y, prev_x, prev_y, x_finish_1, y_finish_1, x_finish_2, y_finish_2) %>% replace_na(FALSE)) %>%
  # Fix races where the finish line is crossed twice (i.e. full lap)
  mutate(cross_finish = ifelse(frame_id < 100, FALSE, cross_finish)) %>%
  mutate(cross_finish = ifelse(race_id %in% double_crosses & frame_id < 300, FALSE, cross_finish)) %>%
  mutate(is_finished = cumsum(cross_finish)) %>%
  # Fix races with glitches
  mutate(is_finished = ifelse(race_id %in% glitch_finishes & is_finished > 1, 1, is_finished)) %>%
  mutate(is_finished = as.logical(is_finished)) %>%
  ungroup() %>%
  # Estimate rank at each frame
  mutate(m = -(x_proj - x) / (y_proj - y)) %>%
  group_by(race_id, frame_id) %>%
  mutate(m = mean(m)) %>%
  mutate(b_proj = mean(y_proj - m*x_proj)) %>%
  mutate(b_init = mean(y - m*x)) %>%
  mutate(want_positive = b_proj - b_init >= 0) %>%
  mutate(b = y - m*x) %>%
  mutate(rank = ifelse(want_positive, rank(-b), rank(b))) %>%
  ungroup() %>%
  select(-m, -b_proj, -b_init, -want_positive, -b) %>%
  # If the horse has crossed the finish, set rank = finishing_place
  mutate(rank = ifelse(is_finished, finishing_place, rank))

# Check that there are no more double finishes
double_finishes = tracking_data_s5 %>%
  group_by(race_id) %>%
  filter(max(is_finished) > 1) %>%
  ungroup() %>%
  filter(cross_finish) %>%
  arrange(race_id, horse_id) %>%
  select(race_id, horse_id, frame_id, is_finished)

# # Visualize a specified race
# ggplot(tracking_data_s5 %>% filter(race_id == "AQU_20190405_6")) +
#   geom_point(aes(x = x, y = y, colour = is_finished), alpha = 0.25) +
#   geom_segment(aes(x = x_finish_1[1], y = y_finish_1[1], xend = x_finish_2[1], yend = y_finish_2[1])) +
#   coord_fixed() +
#   theme_bw()


##############################################
## STEP 6: ANOMALY DETECTION AND IMPUTATION ##
##############################################

## CHECK A: INTERPOLATE AT BEGINNING OF RACE TO SMOOTH ISSUES WITH TRACKING DATA ##

# Calculate average speeds for each track length
mean_motion = tracking_data_s5 %>%
  filter(frame_id <= 40) %>%
  group_by(race_id, horse_id) %>%
  filter(!any((distance_prev == 0 & frame_id != 1) | distance_prev >= 7)) %>%
  ungroup() %>%
  group_by(race_type, course_type, race_distance_furlongs, frame_id) %>%
  summarize(avg_dist = mean(distance_prev)) %>%
  ungroup() %>%
  group_by(race_type, race_distance_furlongs, course_type) %>%
  mutate(c_dist = cumsum(avg_dist)) %>%
  mutate(prop_travelled = c_dist / max(c_dist)) %>%
  ungroup() %>%
  select(-avg_dist, -c_dist)

# Interpolate horses that have choppy data using other horse's performances to describe acceleration
imputed_starts = tracking_data_s5 %>%
  filter(frame_id <= 40) %>%
  group_by(race_id, horse_id) %>%
  filter(any((distance_prev == 0 & frame_id != 1) | distance_prev >= 7)) %>%
  ungroup() %>%
  filter(frame_id == 1 | frame_id == 40) %>%
  mutate(frame_type = ifelse(frame_id == 1, "start", "end")) %>%
  select(race_id, horse_id, race_distance_furlongs, race_type, course_type, frame_type, x, y) %>%
  pivot_wider(id_cols = c("race_id", "horse_id", "race_distance_furlongs", "race_type", "course_type"), names_from = "frame_type", values_from = c("x", "y")) %>%
  nest_join(mean_motion, by = c("race_distance_furlongs", "race_type", "course_type")) %>%
  mutate(mean_motion = pmap(list(mean_motion, x_start, y_start, x_end, y_end), interpolate_coords)) %>%
  unnest() %>%
  select(race_id, horse_id, frame_id, imputed_x = x, imputed_y = y)

# Add interpolated data into full data frame
tracking_data_s6a = tracking_data_s5 %>%
  left_join(imputed_starts, by = c("horse_id", "race_id", "frame_id")) %>%
  mutate(is_imputed = !is.na(imputed_x)) %>%
  rename(raw_x = x, raw_y = y) %>%
  mutate(
    x = ifelse(is_imputed, imputed_x, raw_x),
    y = ifelse(is_imputed, imputed_y, raw_y)
  ) %>%
  select(-imputed_x, -imputed_y)


## CHECK B: ENSURE ALL RACE PLACEMENTS LINE UP WITH OUR OBSERVED RESULTS ##

# Check to ensure that all horses finish the race (this should have 0 rows)
no_finishes = tracking_data_s6a %>%
  group_by(race_id, horse_id, course_type, x_finish_1, x_finish_2, y_finish_1, y_finish_2) %>%
  summarize(finish_frames = sum(cross_finish)) %>%
  filter(finish_frames == 0)

# Add DNF indicator column to data
tracking_data_s6b = tracking_data_s6a %>%
  left_join(no_finishes %>% ungroup() %>% select(race_id, horse_id) %>% mutate(dnf = TRUE), by = c("race_id", "horse_id")) %>%
  mutate(dnf = ifelse(is.na(dnf), FALSE, dnf))

# Create template to plot issue races
i = 10
tracking_data_s6b %>%
  filter(race_id == no_finishes$race_id[i] & horse_id == no_finishes$horse_id[i]) %>%
  ggplot() +
  geom_point(aes(x = x, y = y)) +
  geom_segment(aes(x = x_finish_1[1], y = y_finish_1[1], xend = x_finish_2[1], yend = y_finish_2[1]), colour = "red") +
  coord_fixed()

# Obtain tracking data rankings by finish frame
finish_frames = tracking_data_s6b %>%
  filter(cross_finish & is_finished == 1) %>%
  group_by(race_id) %>%
  arrange(race_id, finishing_place) %>%
  mutate(tracking_finish = rank(frame_id, ties.method = "first")) %>%
  select(race_id, finishing_place, tracking_finish, frame_id, horse_name, horse_id, jockey)

# Plot results
finish_frames %>%
  filter(!(race_id %in% glitch_finishes)) %>%
  group_by(finishing_place, tracking_finish) %>%
  tally() %>%
  ggplot() +
  #geom_point(aes(x = finishing_place, y = tracking_finish, fill = n, size = n), shape = 21) +
  geom_label(aes(x = finishing_place, y = tracking_finish, label = n, fill = n), size = 2) +
  scale_fill_gradientn(colours = c("yellow", "gold", "orange", "red")) +
  scale_size(name = "Count") +
  scale_x_continuous(breaks = c(1:14)) +
  scale_y_continuous(breaks = c(1:14)) +
  theme_bw() +
  labs(x = "Official Finish Position", y = "Finish Position in Tracking Data", fill = "Count")

# Find incorrect ranks
incorrect_ranks = finish_frames %>%
  filter(tracking_finish != finishing_place)


## CHECK C: FIND ANOMALIES IN MINIMUM AND MAXIMUM FRAME FOR EACH HORSE/RACE COMBO ##

# Find difference in max frame for horse vs max frame for race
max_frames = tracking_data_s6b %>%
  group_by(race_id) %>%
  mutate(max_frame = max(frame_id)) %>%
  ungroup() %>%
  group_by(race_id, horse_id) %>%
  slice(which.max(frame_id)) %>%
  ungroup() %>%
  mutate(frame_diff = max_frame - frame_id) %>%
  filter(frame_diff > 0) %>%
  arrange(desc(frame_diff)) %>%
  left_join(no_finishes %>% ungroup() %>% select(race_id, horse_id) %>% mutate(no_finish = 1), by = c("race_id", "horse_id")) %>%
  mutate(no_finish = ifelse(is.na(no_finish), 0, no_finish)) %>%
  select(frame_diff, frame_id, max_frame, race_id, horse_id, no_finish, everything())
# All remaining rows should likely have a value of no_finish == 1 meaning the horse did not finish the race

# Find difference in max frame for horse vs max frame for race
min_frames = tracking_data_s6b %>%
  group_by(race_id) %>%
  mutate(min_frame = min(frame_id)) %>%
  ungroup() %>%
  group_by(race_id, horse_id) %>%
  slice(which.min(frame_id)) %>%
  ungroup() %>%
  mutate(frame_diff = frame_id - min_frame) %>%
  filter(frame_diff > 0) %>%
  arrange(desc(frame_diff)) %>%
  select(frame_diff, frame_id, min_frame, race_id, horse_id, everything())
# Should have 0 rows



############################################################
## STEP 7: RECALCULATE SPEED, ACCELERATION AND DISTANCES ###
############################################################

tracking_data_s7 = tracking_data_s6b %>%
  arrange(race_id, horse_id, frame_id) %>%
  group_by(race_id, horse_id) %>%
  # Get xy-location at previous and next 4 frames
  mutate(
    frames_remaining = max(frame_id) + 1 - frame_id,
    x_fwd = case_when(
      frames_remaining > 4 ~ lead(x,4),
      frames_remaining == 4 ~ lead(x,3),
      frames_remaining == 3 ~ lead(x,2),
      frames_remaining == 2 ~ lead(x,1),
      frames_remaining == 1 ~ x
    ),
    y_fwd = case_when(
      frames_remaining > 4 ~ lead(y,4),
      frames_remaining == 4 ~ lead(y,3),
      frames_remaining == 3 ~ lead(y,2),
      frames_remaining == 2 ~ lead(y,1),
      frames_remaining == 1 ~ y
    ),
    x_back = case_when(
      frame_id > 4 ~ lag(x,4),
      frame_id == 4 ~ lag(x,3),
      frame_id == 3 ~ lag(x,2),
      frame_id == 2 ~ lag(x,1),
      frame_id == 1 ~ x
    ),
    y_back = case_when(
      frame_id > 4 ~ lag(y,4),
      frame_id == 4 ~ lag(y,3),
      frame_id == 3 ~ lag(y,2),
      frame_id == 2 ~ lag(y,1),
      frame_id == 1 ~ y
    ),
    # Calculate speed over +/- 4 frames
    speed = case_when(
      frames_remaining > 4 & frame_id > 4 ~ sqrt((lag(x,4) - lead(x,4))^2 + (lag(y,4) - lead(y,4))^2) / 2.25,
      frames_remaining == 4 ~ sqrt((lag(x,4) - lead(x,3))^2 + (lag(y,4) - lead(y,3))^2) / 2,
      frames_remaining == 3 ~ sqrt((lag(x,4) - lead(x,2))^2 + (lag(y,4) - lead(y,2))^2) / 1.75,
      frames_remaining == 2 ~ sqrt((lag(x,4) - lead(x,1))^2 + (lag(y,4) - lead(y,1))^2) / 1.5,
      frames_remaining == 1 ~ sqrt((lag(x,4) - x)^2 + (lag(y,4) - y)^2) / 1.25,
      frame_id == 4 ~ sqrt((lag(x,3) - lead(x,4))^2 + (lag(y,3) - lead(y,4))^2) / 2,
      frame_id == 3 ~ sqrt((lag(x,2) - lead(x,4))^2 + (lag(y,2) - lead(y,4))^2) / 1.75,
      frame_id == 2 ~ sqrt((lag(x,1) - lead(x,4))^2 + (lag(y,1) - lead(y,4))^2) / 1.5,
      frame_id == 1 ~ sqrt((x - lead(x,4))^2 + (y - lead(y,4))^2) / 1.25
    ),
    # Calculate acceleration over +/- 4 frames
    acceleration = case_when(
      frames_remaining > 4 & frame_id > 4 ~ (lead(speed,4) - lag(speed,4)) / 2.25,
      frames_remaining == 4 ~ (lead(speed,3) - lag(speed,4)) / 2,
      frames_remaining == 3 ~ (lead(speed,2) - lag(speed,4)) / 1.75,
      frames_remaining == 2 ~ (lead(speed,1) - lag(speed,4)) / 1.5,
      frames_remaining == 1 ~ (speed - lag(speed,4)) / 1.25,
      frame_id == 4 ~ (lead(speed,4) - lag(speed,3)) / 2,
      frame_id == 3 ~ (lead(speed,4) - lag(speed,2)) / 1.75,
      frame_id == 2 ~ (lead(speed,4) - lag(speed,1)) / 1.5,
      frame_id == 1 ~ (lead(speed,4) - speed) / 1.25
    ),
    # Calculate distance travelled
    euclid_prev = sqrt((lag(x) - x)^2 + (lag(y) - y)^2) %>% replace_na(0),
    euclid_next = sqrt((lead(x) - x)^2 + (lead(y) - y)^2) %>% replace_na(0),
    euclid_travelled = cumsum(euclid_prev)
  ) %>%
  rowwise() %>%
  mutate(angle = atan2(y_fwd - y_back, x_fwd - x_back) * 180 / pi) %>%
  mutate(angle = ifelse(angle < 0, 360 + angle, angle)) %>%
  ungroup() %>%
  select(-x_fwd, -x_back, -y_fwd, -y_back, -frames_remaining) %>%
  # Determine when each horse crossed the finish line
  arrange(race_id, horse_id, frame_id) %>%
  group_by(race_id, horse_id) %>%
  mutate(prev_x = lag(x), prev_y = lag(y)) %>%
  mutate(cross_finish = check_intersection(x, y, prev_x, prev_y, x_finish_1, y_finish_1, x_finish_2, y_finish_2) %>% replace_na(FALSE)) %>%
  # Fix races where the finish line is crossed twice (i.e. full lap)
  mutate(cross_finish = ifelse(frame_id < 100, FALSE, cross_finish)) %>%
  mutate(cross_finish = ifelse(race_id %in% double_crosses & frame_id < 300, FALSE, cross_finish)) %>%
  mutate(is_finished = cumsum(cross_finish)) %>%
  # Fix races with glitches
  mutate(is_finished = ifelse(race_id %in% glitch_finishes & is_finished > 1, 1, is_finished)) %>%
  mutate(is_finished = as.logical(is_finished)) %>%
  ungroup() %>%
  # Estimate rank at each frame
  mutate(m = -(x_proj - x) / (y_proj - y)) %>%
  group_by(race_id, frame_id) %>%
  mutate(m = mean(m)) %>%
  mutate(b_proj = mean(y_proj - m*x_proj)) %>%
  mutate(b_init = mean(y - m*x)) %>%
  mutate(want_positive = b_proj - b_init >= 0) %>%
  mutate(b = y - m*x) %>%
  mutate(rank = ifelse(want_positive, rank(-b), rank(b))) %>%
  ungroup() %>%
  select(-m, -b_proj, -b_init, -want_positive, -b) %>%
  # If the horse has crossed the finish, set rank = finishing_place
  mutate(rank = ifelse(is_finished, finishing_place, rank)) %>%
  # Only look at active frames during race for each horse
  arrange(race_id, horse_id, frame_id) %>%
  group_by(race_id, horse_id) %>%
  mutate(prev_finished = lag(is_finished, default = FALSE)) %>%
  filter(!prev_finished) %>%
  ungroup() %>%
  # Compute the rolling average distance over the next 4 frames
  mutate(euclid_rolling = (euclid_next + lead(euclid_next) + lead(euclid_next,2) + lead(euclid_next,3)) / 4) %>%
  # Clean up columns
  select(
    -x_finish_1, -y_finish_1, -x_finish_2, -y_finish_2,
    -euclid_prev, -prev_x, -prev_y, -distance_travelled, -distance_next, -distance_prev
  ) %>%
  select(
    # Race-specific info
    race_id, track_id, race_date, race_number, course_type, track_condition, race_distance_furlongs, race_distance_metres,
    run_up_distance, race_type, post_time, n_horses, elevation,
    # Horse-specific info
    purse, odds, weight_carried, jockey_id, jockey, horse_id, horse_name,
    # Frame-specific info
    frame_id, finishing_place, rank, euclid_next, euclid_rolling, x, y, x_proj, y_proj, speed, acceleration, angle,
    euclid_travelled, is_finished, cross_finish, dnf, raw_x, raw_y, is_imputed,
    everything()
  )

#write_csv(tracking_data_s7, "Data/Output/partially_cleaned_data.csv")


################################################################
## STEP 8: BREAK DOWN TRACK INTO STAGES ALONG EACH 0.1 METRES ##
################################################################

# Parallelize code
n_cores = 14
my_cluster = makeCluster(n_cores, setup_strategy = "sequential")
#my_cluster = makeCluster(n_cores, type = "PSOCK")
registerDoParallel(my_cluster)

# Match outer and inner track at every 0.1 metres and split off run-up area
tic()
# Nest the data for parallelization
track_outlines_s8_w1 = track_outlines_s4 %>%
  group_by(track_id, course_type) %>%
  nest() %>%
  filter(course_type != "extra")

# Determine location on track in parallel
track_outlines_s8 = foreach(i = seq_len(nrow(track_outlines_s8_w1)), .combine = "rbind", .packages = "tidyverse") %dopar% {
  
  track_row = track_outlines_s8_w1[i,] %>%
    mutate(final_track = pmap(list(track_id, course_type, data, 0.1), clean_tracks)) %>%
    select(-data) %>%
    unnest() %>%
    ungroup() %>%
    group_by(track_id, course_type, stage) %>%
    mutate(distance = ifelse(grepl("run up", stage), max(distance) - distance, distance)) %>%
    ungroup()
  
  return(track_row)
  
}
toc()

# Stop parallelization
stopCluster(my_cluster)


# # Original code
# track_outlines_s8 = track_outlines_s4 %>%
#   group_by(track_id, course_type) %>%
#   nest() %>%
#   filter(course_type != "extra") %>%
#   mutate(final_track = pmap(list(track_id, course_type, data, 0.1), clean_tracks)) %>%
#   select(-data) %>%
#   unnest() %>%
#   ungroup() %>%
#   group_by(track_id, course_type, stage) %>%
#   mutate(distance = ifelse(grepl("run up", stage), max(distance) - distance, distance)) %>%
#   ungroup()


# Plot outline of tracks
ggplot(track_outlines_s8) +
  geom_point(aes(x = x_circle, y = y_circle, colour = stage)) +
  geom_point(aes(x = x_inner, y = y_inner, colour = stage)) +
  geom_point(aes(x = x_outer, y = y_outer, colour = stage)) +
  geom_segment(aes(x = x_inner, y = y_inner, xend = x_outer, yend = y_outer, colour = stage)) +
  coord_fixed() +
  theme_bw() +
  facet_grid(track_id ~ course_type)

# This will be joined on to the cleaned up horse trajectories in Step 8



##########################################################################
## STEP 9: MATCH TRACKING DATA WITH TRACK AND DETERMINE EXACT DISTANCES ##
##########################################################################

# Determine where the max distance marker occurs (i.e. distance marker flips back to zero)
track_max = track_outlines_s8 %>%
  group_by(track_id, course_type) %>%
  filter(distance == max(distance)) %>%
  ungroup() %>%
  select(track_id, course_type, max_marker = distance)

# Determine where the run-up ends and the full track begins
run_up_ends = track_outlines_s8 %>%
  filter(grepl("run up", stage) & distance == 0) %>%
  select(track_id, course_type, run_up_type = stage, ru_x_inner = x_inner, ru_y_inner = y_inner, ru_x_outer = x_outer, ru_y_outer = y_outer)

full_track_starts = run_up_ends %>%
  left_join(track_outlines_s8 %>% filter(!grepl("run up", stage)), by = c("track_id", "course_type")) %>%
  mutate(euclid = sqrt((x_inner - ru_x_inner)^2 + (y_inner - ru_y_inner)^2 + (x_outer - ru_x_outer)^2 + (y_outer - ru_y_outer)^2)) %>%
  group_by(track_id, course_type, run_up_type) %>%
  filter(euclid == min(euclid)) %>%
  ungroup() %>%
  select(track_id, course_type, run_up_type, run_up_marker = distance)

# Parallelize code
n_cores = 14
my_cluster = makeCluster(n_cores, setup_strategy = "sequential")
#my_cluster = makeCluster(n_cores, type = "PSOCK")
registerDoParallel(my_cluster)

tic()
# Nest the data for parallelization
tracking_data_nested = tracking_data_s7 %>%
  group_by(race_id, horse_id) %>%
  arrange(race_id, horse_id, frame_id) %>%
  nest()

# Determine location on track in parallel
tracking_data_s9a = foreach(i = seq_len(nrow(tracking_data_nested)), .combine = "rbind", .packages = "tidyverse") %dopar% {
  
  track_row = tracking_data_nested[i,] %>%
    mutate(data = map(data, ~assign_track_position(.x, track_outlines_s8))) %>%
    unnest() %>%
    ungroup()
  
  return(track_row)
  
}
toc()

# Stop parallelization
stopCluster(my_cluster)


tracking_data_s9b = tracking_data_s9a %>%
  left_join(track_max, by = c("track_id", "course_type")) %>%
  group_by(race_id, horse_id) %>%
  mutate(run_up_type = ifelse(grepl("run up", first(unique(stage))), first(unique(stage)), NA)) %>%
  left_join(full_track_starts, by = c("track_id", "course_type", "run_up_type")) %>%
  mutate(distance_next = case_when(
    grepl("run up", stage) & grepl("run up", lead(stage)) ~ distance_marker - lead(distance_marker),
    grepl("run up", stage) & !grepl("run up", lead(stage)) ~ distance_marker + (lead(distance_marker) - run_up_marker),
    distance_marker >= max_marker - 10 & lead(distance_marker) <= 10 ~ lead(distance_marker) + (max_marker - distance_marker),
    TRUE ~ lead(distance_marker) - distance_marker
  )) %>%
  mutate(distance_prev = case_when(
    grepl("run up", stage) & grepl("run up", lag(stage)) ~ lag(distance_marker) - distance_marker,
    !grepl("run up", stage) & grepl("run up", lag(stage)) ~ lag(distance_marker) + (distance_marker - lag(run_up_marker)),
    lag(distance_marker) >= max_marker - 10 & distance_marker <= 10 ~ distance_marker + (lag(max_marker) - lag(distance_marker)),
    TRUE ~ distance_marker - lag(distance_marker)
  )) %>%
  mutate(distance_prev = replace_na(distance_prev, 0)) %>%
  mutate(distance_rolling = case_when(
    last(frame_id) - frame_id == 1 ~ distance_next,
    last(frame_id) - frame_id == 2 ~ (distance_next + lead(distance_next)) / 2,
    last(frame_id) - frame_id == 3 ~ (distance_next + lead(distance_next) + lead(distance_next, 2)) / 3,
    TRUE ~ (distance_next + lead(distance_next) + lead(distance_next,2) + lead(distance_next,3)) / 4
  )) %>%
  mutate(distance_travelled = cumsum(distance_prev)) %>%
  ungroup() %>%
  mutate(distance_to_inside = ifelse(inside_track, distance_to_inside, -distance_to_inside))

# Find all data points outside of the track
outside_track = tracking_data_s9b %>% filter(!inside_track)

# Diagnostic plot: Check where points outside of track lie
ggplot() +
  geom_segment(data = track_outlines_s8, aes(x = x_inner, y = y_inner, xend = x_outer, yend = y_outer), colour = "black") +
  geom_segment(data = outside_track, aes(x = x, y = y, xend = x_inner, yend = y_inner), colour = "blue") +
  geom_point(data = outside_track, aes(x = x, y = y, colour = stage), size = 0.5) +
  facet_grid(track_id ~ course_type) +
  coord_fixed() +
  theme_bw()

# Diagnostic plot: How does the rolling distance compare for track vs euclidean
ggplot(tracking_data_s9b %>% filter(race_id == first(race_id), horse_id == first(horse_id))) +
  geom_path(aes(x = frame_id, y = euclid_rolling), colour = "black") +
  geom_path(aes(x = frame_id, y = distance_rolling, colour = stage)) +
  theme_bw()

# Diagnostic plot: How does the raw frame-by-frame distance compare for track vs euclidean
ggplot(tracking_data_s9b %>% filter(race_id == first(race_id), horse_id == first(horse_id))) +
  geom_path(aes(x = frame_id, y = euclid_next), colour = "black") +
  geom_path(aes(x = frame_id, y = distance_next, colour = stage)) +
  theme_bw()

# Determine which races are used in data
all_races = unique(tracking_data_s9b$race_id)


# Parallelize code
n_cores = 14
my_cluster = makeCluster(n_cores, setup_strategy = "sequential")
#my_cluster = makeCluster(n_cores, type = "PSOCK")
registerDoParallel(my_cluster)

tic()
# Nest the data for parallelization
tracking_data_nested2 = tracking_data_s9b %>%
  arrange(race_id, frame_id, horse_id) %>%
  ungroup() %>%
  group_by(race_id, frame_id) %>%
  nest()

# Determine pairwise distances in parallel
tracking_data_s9_temp = foreach(i = seq_len(nrow(tracking_data_nested2)), .combine = "bind_rows", .packages = "tidyverse", .export = "draft") %dopar% {
  
  track_row = tracking_data_nested2[i,] %>%
    mutate(data = map(data, calculate_pairwise_distances)) %>%
    unnest() %>%
    ungroup()
  
  return(track_row)
  
}
toc()

# Stop parallelization
stopCluster(my_cluster)

# Calculate pairwise distances and drafting indicators
tracking_data_s9c = tracking_data_s9_temp %>%
  arrange(race_id, horse_id, frame_id) %>%
  rowwise() %>%
  mutate(is_drafting = as.logical(sum(c_across(starts_with("is_drafting_")), na.rm = TRUE))) %>%
  ungroup() %>%
  group_by(race_id, horse_id) %>%
  mutate(dummy = 1) %>%
  mutate(prop_drafting = cumsum(is_drafting) / cumsum(dummy)) %>%
  select(-dummy) %>%
  ungroup() %>%
  relocate(starts_with("opp_horse"), .after = last_col()) %>%
  relocate(starts_with("euclidean"), .after = last_col()) %>%
  relocate(starts_with("lat_dist"), .after = last_col()) %>%
  relocate(starts_with("fwd_dist"), .after = last_col()) %>%
  relocate(starts_with("is_drafting_"), .after = last_col()) %>%
  select(-run_up_type, -distance_prev) %>%
  group_by(race_id, horse_id) %>%
  mutate(distance_next = ifelse(is.na(distance_next), lag(distance_next), distance_next)) %>%
  mutate(distance_next_smooth = loess(distance_next ~ frame_id, span = 0.1)$fitted) %>%
  mutate(distance_next_smooth = ifelse(is.na(distance_next_smooth), distance_next, distance_next_smooth)) %>%
  ungroup() %>%
  select(
    # Race-specific info
    race_id, track_id, race_date, race_number, course_type, track_condition, race_distance_furlongs, race_distance_metres,
    run_up_distance, race_type, post_time, n_horses, elevation,
    # Horse-specific info
    purse, odds, weight_carried, jockey_id, jockey, horse_id, horse_name,
    # Frame-specific info
    frame_id, finishing_place, rank, distance_next, distance_next_smooth, distance_rolling, distance_travelled, euclid_next, euclid_rolling, euclid_travelled,
    x, y, x_proj, y_proj, speed, acceleration, angle, is_drafting, prop_drafting, is_finished, cross_finish, dnf, raw_x, raw_y,
    is_imputed,
    # Track-specific information
    track_stage = stage, is_inside_track = inside_track, track_marker = distance_marker, track_x_inner = x_inner, track_y_inner = y_inner,
    track_x_outer = x_outer, track_y_outer = y_outer, dist_to_track_inner = distance_to_inside, dist_to_track_outer = distance_to_outside, track_width,
    everything()
  )


#####################################################
## STEP 10: TRIM FRAMES FOR COMPUTATIONAL PURPOSES ##
#####################################################

# Didn't need to do this

# Aggregate speed for each frame at each race length
speed_aggregated = tracking_data_s9b %>%
  group_by(frame_id, course_type, race_distance_furlongs, race_distance_metres) %>%
  summarize(speed = mean(speed, na.rm = TRUE))

# Plot average speed vs frame
ggplot(speed_aggregated %>% filter(frame_id <= 300)) +
  geom_point(aes(x = frame_id, y = speed, colour = race_distance_metres, group = factor(race_distance_furlongs))) +
  facet_wrap(~course_type) +
  scale_colour_viridis_c() +
  labs(x = "Frame", y = "Speed (m/s)", colour = "Distance (m)") +
  theme_bw()


############################################
## STEP 11: ADD IN EXTRA MODEL COVARIATES ##
############################################

# Calculate covariates
extra_covariates = race_covariates(df = tracking_data_s9c, horse_close = 3)

# Add to tracking data
tracking_data_s11 = tracking_data_s9c %>%
  mutate(
    n_horses_inside = extra_covariates$n_horses_inside,
    n_horses_outside = extra_covariates$n_horses_outside,
    n_horses_close_inside = extra_covariates$n_horses_close_inside,
    n_horses_close_outside = extra_covariates$n_horses_close_outside,
    nearest_inside = extra_covariates$nearest_inside,
    nearest_outside = extra_covariates$nearest_outside,
    nearest_inside_euclid = extra_covariates$nearest_inside_euclid,
    nearest_outside_euclid = extra_covariates$nearest_outside_euclid
  ) %>%
  mutate(
    bend = ifelse(grepl("bend", track_stage), 1, 0),
    home_stretch = ifelse(grepl("upper straightaway", track_stage) & frame_id > 100, 1, 0)
  ) %>%
  arrange(race_id, horse_id, frame_id) %>%
  group_by(race_id, horse_id) %>%
  mutate(
    dist_to_track_inner_clean = case_when(
      dist_to_track_inner < 0 ~ 0.05,
      dist_to_track_inner > track_width ~ track_width - 0.1,
      TRUE ~ dist_to_track_inner
    ),
    lag_inside = lag(dist_to_track_inner_clean, default = 0),
    lag_inside_2 = lag(dist_to_track_inner_clean, n = 2, default = 0),
    lag_inside_3 = lag(dist_to_track_inner_clean, n = 3, default = 0),
    lag_inside_4 = lag(dist_to_track_inner_clean, n = 4, default = 0),
    side_movement = ifelse(lag_inside != 0, dist_to_track_inner_clean - lag_inside, 0),
    side_movement_lag = lag(side_movement, default = 0),
    inside_movement = ifelse(lag_inside != 0, (lag_inside - lag_inside_2) / 0.25, 0),
    inside_movement_2 = ifelse(lag_inside != 0, (lag_inside - lag_inside_3) / 0.5, 0),
    inside_movement_3 = ifelse(lag_inside != 0, (lag_inside - lag_inside_4) / 0.5, 0),
    drafting_int = ifelse(is_drafting, 1, 0),
    constructed_euclid_next = sqrt(distance_next^2 + side_movement^2),
    percentage_side_ways_movement = abs(side_movement) / constructed_euclid_next,
    percentage_side_ways_movement_lag = lag(percentage_side_ways_movement, default = 0),
    percentage_perp_movement = 1 - percentage_side_ways_movement,
    percentage_perp_movement_lag = lag(percentage_perp_movement, default = 0)
  ) %>%
  ungroup()


#############################################################################
## STEP 12: STATE TRACK COORDINATES IN TERMS OF DISTANCE TRAVELLED IN RACE ##
#############################################################################

## BUILD TRACK CONNECTOR ##

# Determine where the max distance marker occurs (i.e. distance marker flips back to zero)
track_max = track_outlines_s8 %>%
  group_by(track_id, course_type) %>%
  filter(distance == max(distance)) %>%
  ungroup() %>%
  select(track_id, course_type, max_marker = distance)

# Determine where the run-up ends and the full track begins
run_up_ends = track_outlines_s8 %>%
  filter(grepl("run up", stage) & distance == 0) %>%
  select(track_id, course_type, run_up_type = stage, ru_x_inner = x_inner, ru_y_inner = y_inner, ru_x_outer = x_outer, ru_y_outer = y_outer)

full_track_starts = run_up_ends %>%
  left_join(track_outlines_s8 %>% filter(!grepl("run up", stage)), by = c("track_id", "course_type")) %>%
  mutate(euclid = sqrt((x_inner - ru_x_inner)^2 + (y_inner - ru_y_inner)^2 + (x_outer - ru_x_outer)^2 + (y_outer - ru_y_outer)^2)) %>%
  group_by(track_id, course_type, run_up_type) %>%
  filter(euclid == min(euclid)) %>%
  ungroup() %>%
  select(track_id, course_type, run_up_type, run_up_marker = distance)

run_up_lengths = tracking_data_s11 %>%
  select(track_id, course_type, run_up_distance) %>%
  unique()

# Match track marker to distance travelled in race
track_connector = finish_lines %>%
  left_join(track_outlines_s8, by = c("track_id", "course_type")) %>%
  mutate(dist_to_finish = sqrt((x_inner - x_finish_2)^2 + (y_inner - y_finish_2)^2)) %>%
  group_by(track_id, course_type) %>%
  slice(which.min(dist_to_finish)) %>%
  left_join(track_max, by = c("track_id", "course_type")) %>%
  mutate(run_up_type = ifelse(track_id == "BEL" & course_type == "O", "run up 2", "run up")) %>%
  mutate(include_chute = case_when(
    track_id == "SAR" ~ FALSE,
    course_type == "D" ~ TRUE,
    track_id == "BEL" ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  left_join(full_track_starts, by = c("track_id", "course_type", "run_up_type")) %>%
  mutate(start_chute = case_when(
    !include_chute ~ FALSE,
    is.na(run_up_marker) ~ FALSE,
    distance > run_up_marker ~ distance - run_up_marker < 1609.344,
    distance <= run_up_marker ~ distance + (max_marker - run_up_marker) < 1609.344
  )) %>%
  mutate(dist_to_chute = case_when(
    start_chute & distance > run_up_marker ~ distance - run_up_marker,
    start_chute & distance <= run_up_marker ~ distance + (max_marker - run_up_marker)
  )) %>%
  mutate(initial_marker = case_when(
    start_chute ~ 1609.344 - dist_to_chute,
    distance > 1609.344 ~ distance - 1609.344,
    distance < 1609.344 ~ distance + (max_marker - 1609.344)
  )) %>%
  mutate(initial_marker = round(initial_marker,1)) %>%
  mutate(tracks = pmap(list(track_id, course_type, distance, run_up_marker, max_marker, start_chute), function(track,course,end,chute,max,use_chute) {
    
    # Filter for track and course
    track_data = track_outlines_mod %>%
      filter(track_id == track & course_type == course & stage != "run up 1")
    
    if (track %in% c("BEL", "AQU") & course == "D") {
      track_data = track_data %>%
        group_by(track_id, course_type, stage) %>%
        filter(stage != "left bend" | distance >= max(distance) - 10) %>%
        ungroup()
    }
    
    # If no chute...
    if (!use_chute) {
      
      # Remove chute
      track_data = track_data %>%
        filter(!grepl("run up", stage))
      
      # Filter for relevant track area
      if (end < 1669.344) {
        track_data = track_data %>%
          filter(distance <= end | distance >= max - (1669.344 - end)) %>%
          mutate(race_loc = case_when(
            distance >= max - (1669.344 - end) ~ distance - (max - (1609.344 - end)),
            distance <= end ~ distance + (1609.344 - end)
          ))
      } else {
        track_data = track_data %>%
          filter(distance <= end & distance >= end - 1669.344) %>%
          mutate(race_loc = distance - min(distance) - 60)
      }
      
    } else {
      
      # Filter for relevant track area
      if (chute > end) {
        
        track_data = track_data %>%
          filter(stage %in% c("run up", "run up 2") | distance <= end | distance >= chute) %>%
          mutate(race_loc = case_when(
            stage %in% c("run up", "run up 2") ~ 1609.344 - ((max - chute) + end) - distance,
            distance <= end ~ 1609.344 - (end - distance),
            distance >= chute ~ 1609.344 - end - (max - distance)
          ))
        
      } else {
        
        track_data = track_data %>%
          filter(stage %in% c("run up", "run up 2") | distance <= end | distance >= chute) %>%
          mutate(race_loc = case_when(
            stage %in% c("run up", "run up 2") ~ 1609.344 - (end - chute) - distance,
            distance <= end & distance >= chute ~ 1609.344 - (end - distance)
          ))
        
      }
      
    }
    
    if (max(track_data$race_loc) < 1640 & course != "D" & !(track == "BEL" & course == "I")) {
      max_loc = max(track_data$race_loc)
      overlap = 1609.344 - max(track_data$race_loc)
      
      track_data2 = track_data %>%
        mutate(race_loc = race_loc + 60) %>%
        filter(race_loc <= overlap) %>%
        mutate(race_loc = race_loc + max_loc)
      
      track_data = bind_rows(track_data, track_data2)
    }
    
    return(track_data)
    
  })) %>%
  ungroup() %>%
  select(tracks) %>%
  unnest() %>%
  ungroup() %>%
  mutate(race_loc = round(race_loc, 1)) %>%
  arrange(track_id, course_type, race_loc) %>%
  select(track_id, course_type, stage, race_loc, everything()) %>%
  rename(track_loc = distance) %>%
  select(-x_circle, -y_circle)



## DATA CHECKS ##

euclid_jumps = track_connector %>%
  group_by(track_id, course_type) %>%
  arrange(race_loc) %>%
  mutate(diff = sqrt((x_inner - lead(x_inner))^2 + (y_inner - lead(y_inner))^2)) %>%
  select(diff, everything())

ggplot(track_connector) +
  geom_point(aes(x = paste(track_id, course_type), y = race_loc, colour = stage1)) +
  geom_hline(aes(yintercept = 1609.344)) +
  geom_hline(aes(yintercept = -60)) +
  theme_bw()

ggplot(track_connector) +
  geom_path(aes(x = x_inner, y = y_inner, colour = race_loc)) +
  geom_path(aes(x = x_outer, y = y_outer, colour = race_loc)) +
  facet_grid(track_id ~ course_type) +
  theme_bw()

ggplot(track_connector) +
  geom_point(aes(x = race_loc, y = distance, colour = stage)) +
  facet_grid(track_id ~ course_type) +
  theme_bw()



## UPDATE TRACKING DATA WITH RACE LOCATION ##

tracking_data_s12 = tracking_data_s11 %>%
  mutate(track_stage = case_when(
    track_marker %in% c(1989.2, 1989.3, 1989.4, 1989.5, 1989.6, 1989.7, 1989.8) & track_id == "BEL" & course_type == "O" ~ "run up 2",
    track_marker < 1776.2 & track_marker > 1750 & track_id == "AQU" & course_type == "D" ~ "run up",
    TRUE ~ track_stage
  )) %>%
  mutate(track_marker = case_when(
    track_marker %in% c(1989.2, 1989.3, 1989.4, 1989.5, 1989.6, 1989.7, 1989.8) & track_id == "BEL" & course_type == "O" ~ -(track_marker - 1989.9),
    track_marker < 1776.2 & track_marker > 1750 & track_id == "AQU" & course_type == "D" ~ -(track_marker - 1776.2),
    TRUE ~ track_marker
  )) %>%
  mutate(track_marker = track_marker %>% as.numeric() %>% round(1)) %>%
  nest_join(
    y = track_connector %>%
      select(track_id, course_type, stage, track_marker = track_loc, race_loc) %>%
      mutate(track_marker = track_marker %>% as.numeric() %>% round(1)),
    by = c("track_id", "course_type", "track_stage" = "stage", "track_marker"),
    name = "race_loc"
  ) %>%
  mutate(rows = map(race_loc, ~nrow(.x))) %>%
  unnest(rows) %>%
  arrange(desc(rows)) %>%
  mutate(race_loc = case_when(
    is_finished ~ map(race_loc, ~data.frame(race_loc = 1610)),
    TRUE ~ race_loc
  )) %>%
  unnest() %>%
  ungroup() %>%
  mutate(dummy = ifelse(frame_id < 100, -race_loc, race_loc)) %>%
  group_by(race_id, horse_id, frame_id) %>%
  slice(which.max(dummy)) %>%
  select(-dummy, -rows) %>%
  arrange(race_id, horse_id, frame_id)


###################################
## STEP 13: ADD FINAL COVARIATES ##
###################################

turn2hs = track_connector %>%
  filter(stage == "upper straightaway") %>%
  group_by(track_id, course_type) %>%
  summarize(home_stretch_start = min(track_loc))


tracking_data_s13 = tracking_data_s12 %>%
  mutate(condition_type = case_when(
    course_type %in% c("I", "O") & track_condition == "FM" ~ "turf_firm",
    course_type %in% c("I", "O") & track_condition == "GD" ~ "turf_good",
    course_type %in% c("I", "O") & track_condition == "YL" ~ "turf_yielding",
    course_type %in% c("D") & track_condition == "FT" ~ "dirt_fast",
    course_type %in% c("D") & track_condition == "GD" ~ "dirt_good",
    course_type %in% c("D") & track_condition == "MY" ~ "dirt_muddy",
    course_type %in% c("D") & track_condition == "SY" ~ "dirt_sloppy",
  )) %>%
  left_join(turn2hs, by = c("track_id", "course_type")) %>%
  mutate(
    track_marker = as.numeric(track_marker),
    first40 = ifelse(frame_id <= 40, 1, 0),
    turn2hs = ifelse(track_marker <= home_stretch_start + 10 & track_marker >= home_stretch_start - 5 & !grepl("run up", track_stage), 1, 0),
    bend = ifelse(track_marker < home_stretch_start - 5 & track_stage == "right bend", 1, 0),
    home_stretch = ifelse(track_marker > home_stretch_start + 10 & track_stage == "upper straightaway", 1, 0)
  ) %>%
  rowwise() %>%
  mutate(
    n_horses_fwd = sum(
      c(fwd_dist_1 > 0, fwd_dist_2 > 0, fwd_dist_3 > 0,
      fwd_dist_4 > 0, fwd_dist_5 > 0, fwd_dist_6 > 0,
      fwd_dist_7 > 0, fwd_dist_8 > 0, fwd_dist_9 > 0,
      fwd_dist_10 > 0, fwd_dist_11 > 0, fwd_dist_12 > 0),
      na.rm = TRUE
    ),
    n_horses_bwd = sum(
      c(fwd_dist_1 < 0, fwd_dist_2 < 0, fwd_dist_3 < 0,
      fwd_dist_4 < 0, fwd_dist_5 < 0, fwd_dist_6 < 0,
      fwd_dist_7 < 0, fwd_dist_8 < 0, fwd_dist_9 < 0,
      fwd_dist_10 < 0, fwd_dist_11 < 0, fwd_dist_12 < 0),
      na.rm = TRUE
    ),
    nearest_fwd = abs(min(c(lat_dist_1, lat_dist_2, lat_dist_3, lat_dist_4, lat_dist_5, lat_dist_6,
                            lat_dist_7, lat_dist_8, lat_dist_9, lat_dist_10, lat_dist_11, lat_dist_12), na.rm = TRUE))
  ) %>%
  ungroup() %>%
  group_by(race_id, horse_id) %>%
  arrange(race_id, horse_id, frame_id) %>%
  mutate(distance_prev = race_loc - lag(race_loc)) %>%
  mutate(distance_prev = ifelse(is.na(distance_prev), 0, distance_prev)) %>%
  ungroup()



###################################
## STEP 14: ADD IN DRAFTING DATA ##
###################################

# Load in drafting data
drafting_data = read_csv("Data/Input/drafting_data.csv") %>%
  pivot_wider(id_cols = "fwd_dist", names_from = "lateral", names_prefix = "lat_", values_from = "drag")

# Calculate drafting results
drafting_results = tracking_data_s13 %>%
  head(1000) %>%
  select(race_id, horse_id, frame_id, starts_with("is_drafting_"), starts_with("fwd_dist_"), starts_with("lat_dist_")) %>%
  pivot_longer(
    cols = -c("race_id", "horse_id", "frame_id"),
    names_to = c(".value", "horse"),
    names_pattern = "(.*)(1$|2$|3$|4$|5$|6$|7$|8$|9$|10$|11$|12$)"
  ) %>%
  select(-is_drafting_1, -lat_dist_1, -fwd_dist_1) %>%
  filter(!is.na(is_drafting_)) %>%
  filter(is_drafting_) %>%
  group_by(race_id, horse_id, frame_id) %>%
  slice(which.min(abs(lat_dist_))) %>%
  select(race_id, horse_id, frame_id, draft_fwd = fwd_dist_, draft_lat = lat_dist_) %>%
  mutate(abs_fwd = abs(draft_fwd), abs_lat = abs(draft_lat)) %>%
  mutate(round_fwd = case_when(
    abs_fwd <= 2 & abs_fwd < abs_lat ~ 0,
    abs_fwd <= 2 & abs_fwd >= abs_lat ~ 2,
    abs_fwd <= 2.75 ~ 2,
    abs_fwd <= 4.25 ~ 3.5,
    TRUE ~ 5
  )) %>%
  left_join(drafting_data, by = c("round_fwd" = "fwd_dist")) %>%
  mutate(interpolate_drag = case_when(
    abs_lat > 0.5 ~ `lat_0.5`,
    TRUE ~ 2*abs_lat*`lat_0.5` + (1-2*abs_lat)*lat_0
  )) %>%
  select(race_id, horse_id, frame_id, draft_fwd, draft_lat, drag = interpolate_drag)

# Add in drafting data to model
tracking_data_s14 = tracking_data_s13 %>%
  left_join(drafting_results, by = c("race_id", "horse_id", "frame_id")) %>%
  mutate(drag = replace_na(drag, 0.429)) %>%
  mutate(force = 0.5 * drag * speed^2 * 0.81956) %>%
  mutate(max_force = 0.5 * 0.429 * speed^2 * 0.81956) %>%
  group_by(race_id, horse_id) %>%
  arrange(race_id, horse_id, frame_id) %>%
  mutate(energy = force * lag(euclid_next, default = 0)) %>%
  mutate(max_energy = max_force * lag(euclid_next, default = 0)) %>%
  mutate(
    c_energy = cumsum(energy),
    c_max_energy = cumsum(max_energy),
    prop_energy_spent = c_energy / c_max_energy
  ) %>%
  ungroup() %>%
  group_by(race_id, frame_id) %>%
  mutate(prop_energy_vs_most = prop_energy_spent / max(prop_energy_spent)) %>%
  ungroup() %>%
  mutate(
    prop_energy_vs_most = ifelse(is.na(prop_energy_vs_most), 1, prop_energy_vs_most),
    prop_energy_spent = ifelse(is.na(prop_energy_spent), 1, prop_energy_spent),
    prop_energy_vs_most = ifelse(prop_energy_vs_most == Inf, 1, prop_energy_vs_most),
    prop_energy_spent = ifelse(prop_energy_spent == Inf, 1, prop_energy_spent),
    prop_energy_spent_x_hs = prop_energy_spent * home_stretch,
    prop_energy_vs_most_x_hs = prop_energy_vs_most * home_stretch,
    prop_energy_saved = 1 - prop_energy_spent,
    prop_energy_saved_x_hs = prop_energy_saved_x_hs * home_stretch
  )
  


##########################################
## STEP 14: REARRANGE AND SAVE THE DATA ##
##########################################

# Save data as csvs
write_csv(track_outlines_s4, "Data/Prepped Input/track_outlines_for_viz.csv")
write_csv(track_outlines_s8, "Data/Prepped Input/track_outlines_for_model.csv")
write_csv(finish_lines, "Data/Prepped Input/finish_lines.csv")
write_csv(tracking_data_s14, "Data/Prepped Input/tracking_data_cleaned.csv")
write_csv(track_connector, "Data/Prepped Input/track_connector.csv")





