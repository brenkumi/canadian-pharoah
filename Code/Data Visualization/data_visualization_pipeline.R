
######################################
### STEP 0: LOAD DATA AND PACKAGES ###
######################################

# Load packages
library(tidyverse)
library(gganimate)
library(ggforce)
library(magick)

# Load data
track_outlines_viz = read_csv("Data/Prepped Input/track_outlines_for_viz.csv")
track_outlines_mod = read_csv("Data/Prepped Input/track_outlines_for_model.csv")
tracking_data = read_csv("Data/Prepped Input/tracking_data_cleaned.csv")
dwp = read_csv("Data/Output/full_race_simulation_updated.csv")

# Load data visualization functions
source("Code/Data Visualization/data_visualization_functions.R")



##################################
### STEP 1: CREATE TRACK PLOTS ###
##################################

# Extract and reformat track finish lines
finish_lines = track_outlines_viz %>%
  arrange(track_id) %>%
  filter(outline_type == "finish_line") %>%
  mutate(dummy = "finish", dummy2 = rep(1:2,9)) %>%
  pivot_wider(id_cols = c(track_id, course_type), names_from = c(dummy, dummy2), values_from = c(x,y))


# Create Saratoga track outline
aqueduct_track = ggplot() +
  geom_polygon(data = track_outlines_viz %>% filter(track_id == "AQU" & course_type == "D" & outline_type == "outer" & run_up_type != "direct"), aes(x = x, y = y), fill = "burlywood1") +
  geom_polygon(data = track_outlines_viz %>% filter(track_id == "AQU" & course_type == "D" & outline_type == "outer" & run_up_type != "none"), aes(x = x, y = y), fill = "burlywood1") +
  geom_polygon(data = track_outlines_viz %>% filter(track_id == "AQU" & course_type == "D" & outline_type == "inner"), aes(x = x, y = y), fill = "#26B76A") +
  geom_polygon(data = track_outlines_viz %>% filter(track_id == "AQU" & course_type == "O" & outline_type == "outer"), aes(x = x, y = y), fill = "forestgreen") +
  geom_polygon(data = track_outlines_viz %>% filter(track_id == "AQU" & course_type == "O" & outline_type == "inner"), aes(x = x, y = y), fill = "#26B76A") +
  geom_polygon(data = track_outlines_viz %>% filter(track_id == "AQU" & course_type == "I" & outline_type == "outer"), aes(x = x, y = y), fill = "forestgreen") +
  geom_polygon(data = track_outlines_viz %>% filter(track_id == "AQU" & course_type == "I" & outline_type == "inner" & run_up_type != "direct"), aes(x = x, y = y), fill = "#26B76A") +
  geom_polygon(data = track_outlines_viz %>% filter(track_id == "AQU" & course_type == "I" & outline_type == "inner" & run_up_type != "none"), aes(x = x, y = y), fill = "forestgreen") +
  geom_polygon(data = track_outlines_viz %>% filter(track_id == "AQU" & course_type == "extra" & outline_type == "lake1"), aes(x = x, y = y), fill = "skyblue") +
  geom_polygon(data = track_outlines_viz %>% filter(track_id == "AQU" & course_type == "extra" & outline_type == "lake2"), aes(x = x, y = y), fill = "skyblue") +
  geom_segment(data = finish_lines %>% filter(track_id == "AQU"), aes(x = x_finish_1, xend = x_finish_2, y = y_finish_1, yend = y_finish_2), colour = "red") +
  #geom_point(data = tracking_data_s8 %>% filter(track_id == "AQU"), aes(x = x, y = y, colour = course_type), alpha = 0.05) +
  #scale_colour_manual(values = c("yellow", "yellow", "yellow")) +
  coord_fixed() +
  theme_void()
aqueduct_track

saratoga_track = ggplot() +
  geom_polygon(data = track_outlines_viz %>% filter(track_id == "SAR" & course_type == "D" & outline_type == "outer" & run_up_type != "direct"), aes(x = x, y = y), fill = "burlywood1") +
  geom_polygon(data = track_outlines_viz %>% filter(track_id == "SAR" & course_type == "D" & outline_type == "outer" & run_up_type != "none"), aes(x = x, y = y), fill = "burlywood1") +
  geom_polygon(data = track_outlines_viz %>% filter(track_id == "SAR" & course_type == "D" & outline_type == "inner"), aes(x = x, y = y), fill = "#26B76A") +
  geom_polygon(data = track_outlines_viz %>% filter(track_id == "SAR" & course_type == "O" & outline_type == "outer"), aes(x = x, y = y), fill = "forestgreen") +
  geom_polygon(data = track_outlines_viz %>% filter(track_id == "SAR" & course_type == "O" & outline_type == "inner"), aes(x = x, y = y), fill = "#26B76A") +
  geom_polygon(data = track_outlines_viz %>% filter(track_id == "SAR" & course_type == "I" & outline_type == "outer"), aes(x = x, y = y), fill = "forestgreen") +
  geom_polygon(data = track_outlines_viz %>% filter(track_id == "SAR" & course_type == "I" & outline_type == "inner"), aes(x = x, y = y), fill = "#26B76A") +
  geom_polygon(data = track_outlines_viz %>% filter(track_id == "SAR" & course_type == "extra" & outline_type == "lake"), aes(x = x, y = y), fill = "skyblue") +
  geom_segment(data = finish_lines %>% filter(track_id == "SAR"), aes(x = x_finish_1, xend = x_finish_2, y = y_finish_1, yend = y_finish_2), colour = "red") +
  #geom_point(data = tracking_data_s8 %>% filter(track_id == "SAR"), aes(x = x, y = y, colour = course_type)) +
  #scale_colour_manual(values = c("red", "blue", "purple")) +
  coord_fixed() +
  theme_void()
saratoga_track

belmont_track = ggplot() +
  geom_polygon(data = track_outlines_viz %>% filter(track_id == "BEL" & course_type == "D" & outline_type == "outer" & run_up_type != "direct"), aes(x = x, y = y), fill = "burlywood1") +
  geom_polygon(data = track_outlines_viz %>% filter(track_id == "BEL" & course_type == "D" & outline_type == "outer" & run_up_type != "none"), aes(x = x, y = y), fill = "burlywood1") +
  geom_polygon(data = track_outlines_viz %>% filter(track_id == "BEL" & course_type == "D" & outline_type == "inner"), aes(x = x, y = y), fill = "forestgreen") +
  geom_polygon(data = track_outlines_viz %>% filter(track_id == "BEL" & course_type == "O" & outline_type == "outer" & run_up_type %in% c("none", "partial1", "partial2")), aes(x = x, y = y), fill = "#26B76A") +
  geom_polygon(data = track_outlines_viz %>% filter(track_id == "BEL" & course_type == "O" & outline_type == "outer" & run_up_type %in% c("direct1", "partial1")), aes(x = x, y = y), fill = "#26B76A") +
  geom_polygon(data = track_outlines_viz %>% filter(track_id == "BEL" & course_type == "O" & outline_type == "outer" & run_up_type %in% c("direct2", "partial2")), aes(x = x, y = y), fill = "#26B76A") +
  geom_polygon(data = track_outlines_viz %>% filter(track_id == "BEL" & course_type == "O" & outline_type == "inner"), aes(x = x, y = y), fill = "forestgreen") +
  geom_polygon(data = track_outlines_viz %>% filter(track_id == "BEL" & course_type == "I" & outline_type == "outer" & run_up_type != "direct"), aes(x = x, y = y), fill = "#26B76A") +
  geom_polygon(data = track_outlines_viz %>% filter(track_id == "BEL" & course_type == "I" & outline_type == "outer" & run_up_type != "none"), aes(x = x, y = y), fill = "#26B76A") +
  geom_polygon(data = track_outlines_viz %>% filter(track_id == "BEL" & course_type == "I" & outline_type == "inner"), aes(x = x, y = y), fill = "forestgreen") +
  geom_polygon(data = track_outlines_viz %>% filter(track_id == "BEL" & course_type == "extra" & outline_type == "lake1"), aes(x = x, y = y), fill = "skyblue") +
  geom_polygon(data = track_outlines_viz %>% filter(track_id == "BEL" & course_type == "extra" & outline_type == "lake2"), aes(x = x, y = y), fill = "skyblue") +
  geom_polygon(data = track_outlines_viz %>% filter(track_id == "BEL" & course_type == "extra" & outline_type == "island1"), aes(x = x, y = y), fill = "forestgreen") +
  geom_polygon(data = track_outlines_viz %>% filter(track_id == "BEL" & course_type == "extra" & outline_type == "island2"), aes(x = x, y = y), fill = "forestgreen") +
  geom_polygon(data = track_outlines_viz %>% filter(track_id == "BEL" & course_type == "extra" & outline_type == "island3"), aes(x = x, y = y), fill = "forestgreen") +
  geom_polygon(data = track_outlines_viz %>% filter(track_id == "BEL" & course_type == "extra" & outline_type == "island4"), aes(x = x, y = y), fill = "forestgreen") +
  geom_segment(data = finish_lines %>% filter(track_id == "BEL"), aes(x = x_finish_1, xend = x_finish_2, y = y_finish_1, yend = y_finish_2), colour = "red") +
  #geom_point(data = tracking_data_s8 %>% filter(track_id == "BEL"), aes(x = x, y = y, colour = course_type), alpha = 0.05) +
  #scale_colour_manual(values = c("yellow", "yellow", "white")) +
  coord_fixed() +
  theme_void()
belmont_track



#####################################
### STEP 2: CREATE RACE ANIMATION ###
#####################################

# Set specs for gganimate
options(gganimate.dev_args = list(width = 12, height = 7, units = 'in', res=320))

race = "AQU_20190410_8"

# Create race animation
race_animation = animate_race(
  race = race,
  tracking_data = tracking_data,
  track_outlines_mod = track_outlines_mod,
  track_outlines_viz = track_outlines_viz
)

# Obtain max and min frame in race
max_frame = tracking_data %>%
  filter(race_id == race) %>%
  select(frame_id) %>%
  unlist() %>%
  max()

min_frame = tracking_data %>%
  filter(race_id == race) %>%
  select(frame_id) %>%
  unlist() %>%
  min()

# Render race as mp4 and save
race_mp4 = animate(race_animation, renderer = ffmpeg_renderer(), fps = 8, duration = (max_frame - min_frame)/8)
anim_save(paste("Figures/Race GIFs/", race, ".mp4", sep = ""), race_mp4)



#####################################
### STEP 4: WRITEUP VISUALIZATION ###
#####################################

# Plot data preparation infographic base
data_prep_plot = data_prep_viz(
  race = "BEL_20190516_3",
  frame = 125,
  tracking_data,
  track_outlines_mod,
  track_outlines_viz
)

ggsave("Figures/Writeup/data_preparation_viz.png", data_prep_plot, height = 10, width = 10)


# Generate heatmap GIF for distance metrics
race = "BEL_20190516_3"
race_df = tracking_data %>% filter(race_id == race)
frame_end = max(race_df$frame_id)
frame_start = 1
options(gganimate.dev_args = list(width = 11, height = 8, units = 'in', res=320))

distance_heatmap = animate_race_heatmap(race, frame_start = frame_start, frame_end = frame_end, granularity = 0.3, tracking_data, track_outlines_mod, track_outlines_viz)

race_mp4 = animate(distance_heatmap, renderer = ffmpeg_renderer(), fps = 8, duration = (frame_end - frame_start)/8)
anim_save(paste("Figures/Writeup/", race, "_heatmap.mp4", sep = ""), race_mp4)

anim_save(paste("Figures/Writeup/", race, "_heatmap.gif", sep = ""), distance_heatmap,
          fps = 8, duration = (frame_end - frame_start)/8)


# Generate dynamic win probability square heatmap + race animation
win_prob_gif = animate_win_prob(
  race,
  dwp,
  tracking_data,
  track_outlines_mod,
  track_outlines_viz
)

anim_save(paste("Figures/Writeup/", race, "_dynamic_win_prob.gif", sep = ""), win_prob_gif)














race_mp4 = animate(dwp_animation, renderer = ffmpeg_renderer(), fps = 4, duration = (max(dwp$frame_id) - min(dwp$frame_id))/4)
anim_save(paste("Figures/Writeup/", race, "_dynamic_win_prob.mp4", sep = ""), race_mp4)

dwp %>%
  filter(frame_id == 200) %>%
  mutate(horse_jockey = recode_factor(
    horse_jockey,
    "There He Goes / Kendrick Carmouche" = "There He Goes / K. Carmouche",
    "Curlin's New Moon / Manuel Franco" = "Curlin's New Moon / M. Franco",
    "Purchasing Power / Javier Castellano" = "Purchasing Power / J. Castellano",
    "Somebody / Reylu Gutierrez" = "Somebody / R. Gutierrez",
    "Craigville Beach / Eric Cancel" = "Craigville Beach / E. Cancel",
    "Hoard / Joey R. Martinez" = "Hoard / J.R. Martinez",
    "Change of Venue / Dylan Davis" = "Change of Venue / D. Davis"
  )) %>%
  ggplot() +
  geom_tile(aes(x = simulated_placement, y = horse_jockey, fill = prob_mask), colour = "grey70") +
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
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 15)) +
  coord_equal()

ggsave("Figures/Writeup/dynamic_win_prob_example.png", height = 8, width = 8, dpi = 400)


frame_plot = plot_race_frame_i(
  race = "BEL_20190516_3",
  frame = 55,
  coverup = FALSE,
  tracking_data,
  track_outlines_mod,
  track_outlines_viz
)

ggsave("Figures/Writeup/forward_movement.png", frame_plot, height = 9, width = 11, dpi = 400)

frame_plot = plot_race_frame_ii(
  race = "AQU_20190412_3",
  frame = 321,
  coverup = FALSE,
  tracking_data,
  track_outlines_mod,
  track_outlines_viz
)
frame_plot

ggsave("Figures/Writeup/zoom_on_horse_ex2.png", frame_plot, height = 9, width = 11, dpi = 400)








