

##################################
### STEP 1: CREATE TRACK PLOTS ###
##################################


#####################################
### STEP 2: CREATE RACE ANIMATION ###
#####################################

# Create function to get segments to form horse polygons
get_horse_polygon = function(x_init, y_init, angle, width = 0.64, length = 2.5) {
  
  # Create horse polygon
  y_vals = c(
    0, -0.06, -0.07, -0.12, -0.20, -0.25, -0.275, -0.2825, -0.29, -0.295,
    -0.30, -0.305, -0.31, -0.31, -0.315, -0.315, -0.315, -0.315,
    -0.32, -0.32, -0.315, -0.315, -0.31, -0.31, -0.305, -0.30,
    -0.30, -0.29, -0.29, -0.28, -0.25, -0.21, -0.17, -0.13, -0.11,
    -0.10, -0.095, -0.09, -0.088, -0.09, -0.092, -0.102, -0.11,
    -0.12, -0.13, -0.13,-0.12, -0.10, -0.08, -0.056, -0.025
  )
  
  horse_polygon = data.frame(
    x = c(seq(-1.2,1.3,0.05), rev(seq(-1.2,1.3,0.05))),
    y = c(y_vals, -rev(y_vals))
  ) %>%
    mutate(
      x = x * length / 2.4,
      y = y * width / 0.63
    )
  
  # Convert angle to radians
  rads = -angle * pi/180
  
  # Create conversion matrix
  conversionmatrix = matrix(c(cos(rads), sin(rads), -sin(rads), cos(rads)), ncol=2, nrow=2)
  
  # Rotate the horse coordinates
  rotated_horse = as.data.frame(as.matrix(horse_polygon)%*%conversionmatrix) %>%
    rename(x = V1, y = V2) %>%
    mutate(x = x + x_init, y = y + y_init)
  
  return(rotated_horse)
  
}


# Create function to define drafting region
draft = function(distance_behind, horse_width = 1.2, max_distance_behind = 7) {
  
  (horse_width / 2) * log((max_distance_behind + 1) - distance_behind) / log(max_distance_behind + 1)
  
}


# Create function to get segments to form a comet trail behind each horse
get_trail_segments = function(x_init, y_init, angle, width = 0.64, length = 7) {
  
  # Get raw coordinates for trail 
  x = seq(-length,0,0.05)
  y = draft(-x, horse_width = width, max_distance_behind = length)
  
  # Convert angle to radians
  rads = -angle * pi/180
  
  # Create conversion matrix
  conversionmatrix = matrix(c(cos(rads), sin(rads), -sin(rads), cos(rads)), ncol=2, nrow=2)
  
  # Rotate the drag coordinates for both sides
  rotated_drag1 = as.data.frame(cbind(x,y) %*% conversionmatrix) %>%
    rename(x1 = V1, y1 = V2)
  
  rotated_drag2 = as.data.frame(cbind(x,-y) %*% conversionmatrix) %>%
    rename(x2 = V1, y2 = V2)
  
  rotated_drag = bind_cols(rotated_drag1, rotated_drag2) %>%
    mutate(
      x1 = x1 + x_init,
      x2 = x2 + x_init,
      y1 = y1 + y_init,
      y2 = y2 + y_init
    ) %>%
    mutate(alpha = row_number())
  
  return(rotated_drag)
  
}


# Animate a race with magnifying glass over horses
animate_race = function(race,
                        tracking_data,
                        track_outlines_mod,
                        track_outlines_viz) {
  
  ## BASIC DATA SETUP ##
  
  # Extract data relevant to the subject race
  race_data = tracking_data %>%
    filter(race_id == race) %>%
    mutate(horse_jockey = paste(horse_name, jockey, sep = " / ")) %>%
    group_by(horse_jockey) %>%
    arrange(horse_jockey, frame_id) %>%
    fill(distance_next, .direction = "downup") %>%
    mutate(distance_next2 = loess(distance_next ~ frame_id)$fitted) %>%
    ungroup() %>%
    group_by(frame_id) %>%
    mutate(rel_acceleration = acceleration - mean(acceleration)) %>%
    mutate(rel_distance = (distance_next2 - mean(distance_next2)) / sd(distance_next2)) %>%
    ungroup() %>%
    select(
      track_id, course_type, horse_jockey, frame_id, rank,
      rel_acceleration, rel_distance, x, y, angle, distance_next, dnf
    )
  
  race_track_zoom = track_outlines_mod %>%
    filter(track_id == race_data$track_id[[1]] & course_type == race_data$course_type[[1]]) %>%
    slice(which(distance %% 1 == 0))
  
  race_track_full = track_outlines_mod %>%
    filter(track_id == race_data$track_id[[1]] & course_type == race_data$course_type[[1]]) %>%
    slice(which(distance %% 10 == 0))
  
  race_track_viz = track_outlines_viz %>%
    filter(track_id == race_data$track_id[[1]])
  
  # Extract and reformat track finish lines
  finish_lines = track_outlines_viz %>%
    arrange(track_id) %>%
    filter(outline_type == "finish_line") %>%
    mutate(dummy = "finish", dummy2 = rep(1:2,9)) %>%
    pivot_wider(id_cols = c(track_id, course_type), names_from = c(dummy, dummy2), values_from = c(x,y)) %>%
    filter(track_id == race_data$track_id[[1]] & course_type == race_data$course_type[[1]]) %>%
    mutate(x_finish_1 = x_finish_2)
  
  # Create data frames with horse shapes and comet trails
  horse_polygons = race_data %>%
    select(frame_id, horse_jockey, x, y, angle) %>%
    mutate(polygon = pmap(list(x, y, angle, 0.64, 2.4), get_horse_polygon)) %>%
    select(frame_id, horse_jockey, polygon) %>%
    unnest(polygon) %>%
    ungroup()
  
  horse_trails = race_data %>%
    select(frame_id, horse_jockey, x, y, angle, distance_next, rel_acceleration, rel_distance) %>%
    mutate(trail_length = case_when(
      is.na(distance_next) ~ 1,
      distance_next <= 0 ~ 0,
      TRUE ~ distance_next
    )) %>%
    mutate(trail = pmap(list(x, y, angle, 0.64, trail_length), get_trail_segments)) %>%
    select(frame_id, horse_jockey, rel_acceleration, rel_distance, distance_next, trail) %>%
    unnest(trail) %>%
    ungroup()
  
  
  ## ADJUST COORDINATES TO NARROW IN ON HORSES ##
  
  # Get various track information
  max_x = max(race_track_viz$x); min_x = min(race_track_viz$x)
  max_y = max(race_track_viz$y); min_y = min(race_track_viz$y)
  scaled_radius = (max_y - min_y)
  left_x = max_x + (max_x - min_x) * 0.1
  mid_x = left_x + scaled_radius
  right_x = left_x + scaled_radius * 2
  high_y = max_y
  mid_y = min_y
  low_y = min_y - (max_y - min_y)
  
  # Determine mean positioning of horses during race
  horse_centres = race_data %>%
    filter(!dnf) %>%
    group_by(frame_id) %>%
    summarize(
      max_dist = max(dist(data.frame(x, y))),
      x_width = max(x) - min(x),
      y_width = max(y) - min(y),
      x_centre = (max(x) + min(x)) / 2,
      y_centre = (max(y) + min(y)) / 2
    ) %>%
    mutate(max_dist = ifelse(max_dist == -Inf, 0, max_dist)) %>%
    mutate(magnify_radius_raw = (max_dist + 10) / 2) %>%
    mutate(magnify_radius = loess(magnify_radius_raw ~ frame_id)$fitted) %>%
    mutate(scale_factor = scaled_radius / magnify_radius) %>%
    mutate(text_size = 25 * 1/magnify_radius) %>%
    ungroup() %>%
    select(frame_id, x_centre, y_centre, magnify_radius, scale_factor, text_size)
  
  # Adjust coordinates for all data frames needed to create the viz
  race_data_adj = race_data %>%
    left_join(horse_centres, by = "frame_id") %>%
    mutate(x_adj = scale_factor * (x - x_centre) + mid_x) %>%
    mutate(y_adj = scale_factor * (y - y_centre) + mid_y)
  
  horse_polygons_adj = horse_polygons %>%
    left_join(horse_centres, by = "frame_id") %>%
    mutate(x_adj = scale_factor * (x - x_centre) + mid_x) %>%
    mutate(y_adj = scale_factor * (y - y_centre) + mid_y)
  
  horse_trails_adj = horse_trails %>%
    left_join(horse_centres, by = "frame_id") %>%
    mutate(x1_adj = scale_factor * (x1 - x_centre) + mid_x) %>%
    mutate(y1_adj = scale_factor * (y1 - y_centre) + mid_y) %>%
    mutate(x2_adj = scale_factor * (x2 - x_centre) + mid_x) %>%
    mutate(y2_adj = scale_factor * (y2 - y_centre) + mid_y)
  
  race_track_viz_adj = horse_centres %>%
    mutate(tracks = map(frame_id, ~race_track_viz)) %>%
    unnest(tracks) %>%
    ungroup() %>%
    mutate(x_adj = scale_factor * (x - x_centre) + mid_x) %>%
    mutate(y_adj = scale_factor * (y - y_centre) + mid_y)
  
  finish_lines_adj = horse_centres %>%
    mutate(tracks = map(frame_id, ~finish_lines)) %>%
    unnest(tracks) %>%
    ungroup() %>%
    mutate(x_finish_1_adj = scale_factor * (x_finish_1 - x_centre) + mid_x) %>%
    mutate(y_finish_1_adj = scale_factor * (y_finish_1 - y_centre) + mid_y) %>%
    mutate(x_finish_2_adj = scale_factor * (x_finish_2 - x_centre) + mid_x) %>%
    mutate(y_finish_2_adj = scale_factor * (y_finish_2 - y_centre) + mid_y)
  
  race_track_zoom_adj = horse_centres %>%
    mutate(tracks = map(frame_id, ~race_track_zoom)) %>%
    unnest(tracks) %>%
    ungroup() %>%
    mutate(x_inner_adj = scale_factor * (x_inner - x_centre) + mid_x) %>%
    mutate(y_inner_adj = scale_factor * (y_inner - y_centre) + mid_y) %>%
    mutate(x_outer_adj = scale_factor * (x_outer - x_centre) + mid_x) %>%
    mutate(y_outer_adj = scale_factor * (y_outer - y_centre) + mid_y)
  
  race_track_full_adj = horse_centres %>%
    mutate(tracks = map(frame_id, ~race_track_full)) %>%
    unnest(tracks) %>%
    ungroup() %>%
    mutate(x_inner_adj = scale_factor * (x_inner - x_centre) + mid_x) %>%
    mutate(y_inner_adj = scale_factor * (y_inner - y_centre) + mid_y) %>%
    mutate(x_outer_adj = scale_factor * (x_outer - x_centre) + mid_x) %>%
    mutate(y_outer_adj = scale_factor * (y_outer - y_centre) + mid_y)
  
  # Create anti-circle to cover up track that bleeds outside of magnifying circle
  x = seq(-1,1,0.01)
  y1 = sqrt(1 - x^2)
  y2 = -sqrt(1 - x^2)
  
  anticircle1 = data.frame(
    x = c(-Inf, -Inf, x, Inf, Inf),
    y = c(Inf, 0, y1, 0, Inf)
  ) %>%
    mutate(x = x * scaled_radius + mid_x) %>%
    mutate(y = y * scaled_radius + mid_y)
  
  anticircle2 = data.frame(
    x = c(-Inf, -Inf, x, Inf, Inf),
    y = c(-Inf, 0, y2, 0, -Inf)
  ) %>%
    mutate(x = x * scaled_radius + mid_x) %>%
    mutate(y = y * scaled_radius + mid_y)
  
  
  
  ## ADD IN DYNAMIC TRACK ##
  
  if (race_data$track_id[[1]] == "AQU") {
    
    animation_v1 = ggplot() +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "D" & outline_type == "outer" & run_up_type != "direct"), aes(x = x_adj, y = y_adj), fill = "burlywood1") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "D" & outline_type == "outer" & run_up_type != "none"), aes(x = x_adj, y = y_adj), fill = "burlywood1") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "D" & outline_type == "inner"), aes(x = x_adj, y = y_adj), fill = "#26B76A") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "O" & outline_type == "outer"), aes(x = x_adj, y = y_adj), fill = "forestgreen") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "O" & outline_type == "inner"), aes(x = x_adj, y = y_adj), fill = "#26B76A") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "I" & outline_type == "outer"), aes(x = x_adj, y = y_adj), fill = "forestgreen") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "I" & outline_type == "inner" & run_up_type != "direct"), aes(x = x_adj, y = y_adj), fill = "#26B76A") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "I" & outline_type == "inner" & run_up_type != "none"), aes(x = x_adj, y = y_adj), fill = "forestgreen") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "extra" & outline_type == "lake1"), aes(x = x_adj, y = y_adj), fill = "skyblue") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "extra" & outline_type == "lake2"), aes(x = x_adj, y = y_adj), fill = "skyblue")
    
    
  } else if (race_data$track_id[[1]] == "BEL") {
    
    animation_v1 = ggplot() +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "D" & outline_type == "outer" & run_up_type != "direct"), aes(x = x_adj, y = y_adj), fill = "burlywood1") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "D" & outline_type == "outer" & run_up_type != "none"), aes(x = x_adj, y = y_adj), fill = "burlywood1") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "D" & outline_type == "inner"), aes(x = x_adj, y = y_adj), fill = "forestgreen") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "O" & outline_type == "outer" & run_up_type %in% c("none", "partial1", "partial2")), aes(x = x_adj, y = y_adj), fill = "#26B76A") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "O" & outline_type == "outer" & run_up_type %in% c("direct1", "partial1")), aes(x = x_adj, y = y_adj), fill = "#26B76A") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "O" & outline_type == "outer" & run_up_type %in% c("direct2", "partial2")), aes(x = x_adj, y = y_adj), fill = "#26B76A") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "O" & outline_type == "inner"), aes(x = x_adj, y = y_adj), fill = "forestgreen") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "I" & outline_type == "outer" & run_up_type != "direct"), aes(x = x_adj, y = y_adj), fill = "#26B76A") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "I" & outline_type == "outer" & run_up_type != "none"), aes(x = x_adj, y = y_adj), fill = "#26B76A") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "I" & outline_type == "inner"), aes(x = x_adj, y = y_adj), fill = "forestgreen") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "extra" & outline_type == "lake1"), aes(x = x_adj, y = y_adj), fill = "skyblue") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "extra" & outline_type == "lake2"), aes(x = x_adj, y = y_adj), fill = "skyblue") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "extra" & outline_type == "island1"), aes(x = x_adj, y = y_adj), fill = "forestgreen") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "extra" & outline_type == "island2"), aes(x = x_adj, y = y_adj), fill = "forestgreen") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "extra" & outline_type == "island3"), aes(x = x_adj, y = y_adj), fill = "forestgreen") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "extra" & outline_type == "island4"), aes(x = x_adj, y = y_adj), fill = "forestgreen")
    
  } else {
    
    animation_v1 = ggplot() +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "D" & outline_type == "outer" & run_up_type != "direct"), aes(x = x_adj, y = y_adj), fill = "burlywood1") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "D" & outline_type == "outer" & run_up_type != "none"), aes(x = x_adj, y = y_adj), fill = "burlywood1") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "D" & outline_type == "inner"), aes(x = x_adj, y = y_adj), fill = "#26B76A") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "O" & outline_type == "outer"), aes(x = x_adj, y = y_adj), fill = "forestgreen") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "O" & outline_type == "inner"), aes(x = x_adj, y = y_adj), fill = "#26B76A") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "I" & outline_type == "outer"), aes(x = x_adj, y = y_adj), fill = "forestgreen") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "I" & outline_type == "inner"), aes(x = x_adj, y = y_adj), fill = "#26B76A") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "extra" & outline_type == "lake"), aes(x = x_adj, y = y_adj), fill = "skyblue")
  }
  
  animation_v2 = animation_v1 +
    ## DYNAMIC FINISH LINES AND TRACK OUTLINES ##
    geom_segment(data = finish_lines_adj, aes(x = x_finish_1_adj, xend = x_finish_2_adj, y = y_finish_1_adj, yend = y_finish_2_adj), colour = "red") +
    geom_segment(data = race_track_zoom_adj, aes(x = x_inner_adj, y = y_inner_adj, xend = x_outer_adj, yend = y_outer_adj), colour = "grey80", size = 0.5) +
    geom_segment(data = race_track_full_adj, aes(x = x_inner_adj, y = y_inner_adj, xend = x_outer_adj, yend = y_outer_adj), colour = "grey60", size = 1.2) +
    geom_path(data = race_track_zoom_adj, aes(x = x_inner_adj, y = y_inner_adj, group = stage), size = 1.2, colour = "grey60") +
    geom_path(data = race_track_zoom_adj, aes(x = x_outer_adj, y = y_outer_adj, group = stage), size = 1.2, colour = "grey60") +
    ## MAGNIFIED HORSES ##
    geom_segment(data = horse_trails_adj, aes(x = x1_adj, xend = x2_adj, y = y1_adj, yend = y2_adj, alpha = alpha, colour = rel_distance), size = 2, show.legend = FALSE) +
    scale_colour_gradient2(low = "blue", mid = "grey80", high = "red") +
    ggnewscale::new_scale_colour() +
    geom_polygon(data = horse_polygons_adj, aes(x = x_adj, y = y_adj, colour = horse_jockey), fill = "chocolate4", show.legend = FALSE) +
    geom_circle(data = race_data_adj, aes(x0 = x_adj, y0 = y_adj, r = 0.32 * scale_factor, fill = horse_jockey)) +
    geom_text(data = race_data_adj, aes(x = x_adj, y = y_adj, label = rank, size = text_size), colour = "white", show.legend = FALSE) +
    scale_size_continuous(range = c(min(horse_centres$text_size), max(horse_centres$text_size))) +
    ## ANTI-CIRCLE AND MAGNIFIED CIRCLE ##
    geom_polygon(data = anticircle1, aes(x = x, y = y), fill = "white") +
    geom_polygon(data = anticircle2, aes(x = x, y = y), fill = "white") +
    geom_circle(aes(x0 = mid_x, y0 = mid_y, r = scaled_radius))
  
  
  ## ADD IN STATIC TRACK ##
  
  if (race_data$track_id[[1]] == "AQU") {
    
    animation_v3 = animation_v2 +
      geom_polygon(data = race_track_viz %>% filter(course_type == "D" & outline_type == "outer" & run_up_type != "direct"), aes(x = x, y = y), fill = "burlywood1") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "D" & outline_type == "outer" & run_up_type != "none"), aes(x = x, y = y), fill = "burlywood1") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "D" & outline_type == "inner"), aes(x = x, y = y), fill = "#26B76A") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "O" & outline_type == "outer"), aes(x = x, y = y), fill = "forestgreen") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "O" & outline_type == "inner"), aes(x = x, y = y), fill = "#26B76A") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "I" & outline_type == "outer"), aes(x = x, y = y), fill = "forestgreen") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "I" & outline_type == "inner" & run_up_type != "direct"), aes(x = x, y = y), fill = "#26B76A") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "I" & outline_type == "inner" & run_up_type != "none"), aes(x = x, y = y), fill = "forestgreen") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "extra" & outline_type == "lake1"), aes(x = x, y = y), fill = "skyblue") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "extra" & outline_type == "lake2"), aes(x = x, y = y), fill = "skyblue")
    
  } else if (race_data$track_id[[1]] == "BEL") {
    
    animation_v3 = animation_v2 +
      geom_polygon(data = race_track_viz %>% filter(course_type == "D" & outline_type == "outer" & run_up_type != "direct"), aes(x = x, y = y), fill = "burlywood1") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "D" & outline_type == "outer" & run_up_type != "none"), aes(x = x, y = y), fill = "burlywood1") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "D" & outline_type == "inner"), aes(x = x, y = y), fill = "forestgreen") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "O" & outline_type == "outer" & run_up_type %in% c("none", "partial1", "partial2")), aes(x = x, y = y), fill = "#26B76A") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "O" & outline_type == "outer" & run_up_type %in% c("direct1", "partial1")), aes(x = x, y = y), fill = "#26B76A") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "O" & outline_type == "outer" & run_up_type %in% c("direct2", "partial2")), aes(x = x, y = y), fill = "#26B76A") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "O" & outline_type == "inner"), aes(x = x, y = y), fill = "forestgreen") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "I" & outline_type == "outer" & run_up_type != "direct"), aes(x = x, y = y), fill = "#26B76A") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "I" & outline_type == "outer" & run_up_type != "none"), aes(x = x, y = y), fill = "#26B76A") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "I" & outline_type == "inner"), aes(x = x, y = y), fill = "forestgreen") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "extra" & outline_type == "lake1"), aes(x = x, y = y), fill = "skyblue") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "extra" & outline_type == "lake2"), aes(x = x, y = y), fill = "skyblue") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "extra" & outline_type == "island1"), aes(x = x, y = y), fill = "forestgreen") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "extra" & outline_type == "island2"), aes(x = x, y = y), fill = "forestgreen") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "extra" & outline_type == "island3"), aes(x = x, y = y), fill = "forestgreen") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "extra" & outline_type == "island4"), aes(x = x, y = y), fill = "forestgreen")
    
  } else {
    
    animation_v3 = animation_v2 +
      geom_polygon(data = race_track_viz %>% filter(course_type == "D" & outline_type == "outer" & run_up_type != "direct"), aes(x = x, y = y), fill = "burlywood1") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "D" & outline_type == "outer" & run_up_type != "none"), aes(x = x, y = y), fill = "burlywood1") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "D" & outline_type == "inner"), aes(x = x, y = y), fill = "#26B76A") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "O" & outline_type == "outer"), aes(x = x, y = y), fill = "forestgreen") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "O" & outline_type == "inner"), aes(x = x, y = y), fill = "#26B76A") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "I" & outline_type == "outer"), aes(x = x, y = y), fill = "forestgreen") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "I" & outline_type == "inner"), aes(x = x, y = y), fill = "#26B76A") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "extra" & outline_type == "lake"), aes(x = x, y = y), fill = "skyblue")
  }
  
  animation_v4 = animation_v3 +
    ## ADD FINISH LINE AND TRACK OUTLINE ##
    geom_segment(data = finish_lines, aes(x = x_finish_1, xend = x_finish_2, y = y_finish_1, yend = y_finish_2), colour = "red") +
    geom_segment(data = race_track_full, aes(x = x_inner, y = y_inner, xend = x_outer, yend = y_outer), colour = "grey60", size = 0.4) +
    ## PLACEMENT CIRCLE AND CONNECTING LINE ##
    geom_segment(data = horse_centres, aes(x = x_centre + magnify_radius, xend = left_x, y = y_centre, yend = mid_y)) +
    geom_circle(data = horse_centres, aes(x0 = x_centre, y0 = y_centre, r = magnify_radius)) +
    ## SMALL HORSES ##
    geom_polygon(data = horse_polygons_adj, aes(x = x, y = y, colour = horse_jockey), fill = "chocolate4", show.legend = FALSE) +
    ## FORMATTING ##
    transition_time(frame_id) +
    labs(fill = "Horse / Jockey") +
    coord_fixed(xlim = c(min_x, right_x), ylim = c(low_y, high_y)) +
    theme_void() +
    scale_alpha_continuous(range = c(0,1)) +
    theme(
      legend.position = c(0.15,0.2),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8)
    )
  
  return(animation_v4)
  
}



#####################################
### STEP 3: WRITEUP VISUALIZATION ###
#####################################

# Create background visual for data preparation infographic
data_prep_viz = function(race = "BEL_20190516_3",
                         frame = 110,
                         tracking_data,
                         track_outlines_mod,
                         track_outlines_viz) {
  
  ## BASIC DATA SETUP ##
  
  # Extract data relevant to the subject race
  race_data = tracking_data %>%
    filter(race_id == race & frame_id == frame) %>%
    mutate(horse_jockey = paste(horse_name, jockey, sep = " / ")) %>%
    arrange(horse_jockey, frame_id) %>%
    group_by(frame_id) %>%
    mutate(rel_acceleration = acceleration - mean(acceleration)) %>%
    mutate(rel_distance = (distance_next - mean(distance_next)) / sd(distance_next)) %>%
    ungroup() %>%
    select(
      track_id, course_type, horse_jockey, frame_id, rank,
      rel_acceleration, rel_distance, x, y, angle, distance_next, dnf,
      track_x_inner, track_y_inner
    )
  
  race_track_full = track_outlines_mod %>%
    filter(track_id == race_data$track_id[[1]]) %>%
    slice(which(distance %% 5 == 0)) %>%
    mutate(orig_stage = stage) %>%
    mutate(stage = ifelse(grepl("run up", stage), "run up", stage)) %>%
    mutate(stage = case_when(
      stage == "left bend" ~ "Left Turn",
      stage == "right bend" ~ "Right Turn",
      stage == "upper straightaway" ~ "Home Stretch",
      stage == "lower straightaway" ~ "Back Stretch",
      stage == "run up" ~ "Chute",
    )) %>%
    mutate(stage = factor(stage, levels = c("Left Turn", "Right Turn", "Home Stretch", "Back Stretch", "Chute")))
  
  race_track_zoom = race_track_full %>%
    filter(track_id == race_data$track_id[[1]]) %>%
    slice(which(distance %% 1 == 0))
  
  race_track_viz = track_outlines_viz %>%
    filter(track_id == race_data$track_id[[1]])
  
  # Extract and reformat track finish lines
  finish_lines = track_outlines_viz %>%
    arrange(track_id) %>%
    filter(outline_type == "finish_line") %>%
    mutate(dummy = "finish", dummy2 = rep(1:2,9)) %>%
    pivot_wider(id_cols = c(track_id, course_type), names_from = c(dummy, dummy2), values_from = c(x,y)) %>%
    filter(track_id == race_data$track_id[[1]] & course_type == race_data$course_type[[1]]) %>%
    mutate(x_finish_1 = x_finish_2)
  
  # Create data frames with horse shapes and comet trails
  horse_polygons = race_data %>%
    select(frame_id, horse_jockey, x, y, angle) %>%
    mutate(polygon = pmap(list(x, y, angle, 0.64, 2.4), get_horse_polygon)) %>%
    select(frame_id, horse_jockey, polygon) %>%
    unnest(polygon) %>%
    ungroup()
  
  horse_trails = race_data %>%
    select(frame_id, horse_jockey, x, y, angle, distance_next, rel_acceleration, rel_distance) %>%
    mutate(trail_length = case_when(
      is.na(distance_next) ~ 1,
      distance_next <= 0 ~ 0,
      TRUE ~ distance_next
    )) %>%
    mutate(trail = pmap(list(x, y, angle, 0.64, trail_length), get_trail_segments)) %>%
    select(frame_id, horse_jockey, rel_acceleration, rel_distance, distance_next, trail) %>%
    unnest(trail) %>%
    ungroup()
  
  
  ## ADJUST COORDINATES TO NARROW IN ON HORSES ##
  
  # Get various track information
  max_x = max(race_track_viz$x); min_x = min(race_track_viz$x); med_x = (max(race_track_viz$x) + min(race_track_viz$x)) / 2
  max_y = max(race_track_viz$y); min_y = min(race_track_viz$y); med_y = (max(race_track_viz$y) + min(race_track_viz$y)) / 2
  scaled_radius = (max_x - med_x) / 2
  left_x = med_x - min_x
  mid_x = left_x + scaled_radius
  right_x = max_x
  high_y = min_y - (max_y - min_y) / 4
  mid_y = high_y - scaled_radius
  low_y = high_y - scaled_radius * 2
  
  # Determine mean positioning of horses during race
  horse_centres = race_data %>%
    filter(!dnf) %>%
    group_by(frame_id) %>%
    summarize(
      max_dist = max(dist(data.frame(x, y))),
      x_width = max(x) - min(x),
      y_width = max(y) - min(y),
      x_centre = (max(x) + min(x)) / 2,
      y_centre = (max(y) + min(y)) / 2
    ) %>%
    mutate(max_dist = ifelse(max_dist == -Inf, 0, max_dist)) %>%
    mutate(magnify_radius = (max_dist + 8) / 2) %>%
    mutate(scale_factor = scaled_radius / magnify_radius) %>%
    mutate(text_size = 25 * 1/magnify_radius) %>%
    ungroup() %>%
    select(frame_id, x_centre, y_centre, magnify_radius, scale_factor, text_size)
  
  # Adjust coordinates for all data frames needed to create the viz
  race_data_adj = race_data %>%
    left_join(horse_centres, by = "frame_id") %>%
    mutate(x_adj = scale_factor * (x - x_centre) + mid_x) %>%
    mutate(y_adj = scale_factor * (y - y_centre) + mid_y) %>%
    mutate(x_inner_adj = scale_factor * (track_x_inner - x_centre) + mid_x) %>%
    mutate(y_inner_adj = scale_factor * (track_y_inner - y_centre) + mid_y)
  
  horse_polygons_adj = horse_polygons %>%
    left_join(horse_centres, by = "frame_id") %>%
    mutate(x_adj = scale_factor * (x - x_centre) + mid_x) %>%
    mutate(y_adj = scale_factor * (y - y_centre) + mid_y)
  
  horse_trails_adj = horse_trails %>%
    left_join(horse_centres, by = "frame_id") %>%
    mutate(x1_adj = scale_factor * (x1 - x_centre) + mid_x) %>%
    mutate(y1_adj = scale_factor * (y1 - y_centre) + mid_y) %>%
    mutate(x2_adj = scale_factor * (x2 - x_centre) + mid_x) %>%
    mutate(y2_adj = scale_factor * (y2 - y_centre) + mid_y)
  
  race_track_viz_adj = horse_centres %>%
    mutate(tracks = map(frame_id, ~race_track_viz)) %>%
    unnest(tracks) %>%
    ungroup() %>%
    mutate(x_adj = scale_factor * (x - x_centre) + mid_x) %>%
    mutate(y_adj = scale_factor * (y - y_centre) + mid_y)
  
  finish_lines_adj = horse_centres %>%
    mutate(tracks = map(frame_id, ~finish_lines)) %>%
    unnest(tracks) %>%
    ungroup() %>%
    mutate(x_finish_1_adj = scale_factor * (x_finish_1 - x_centre) + mid_x) %>%
    mutate(y_finish_1_adj = scale_factor * (y_finish_1 - y_centre) + mid_y) %>%
    mutate(x_finish_2_adj = scale_factor * (x_finish_2 - x_centre) + mid_x) %>%
    mutate(y_finish_2_adj = scale_factor * (y_finish_2 - y_centre) + mid_y)
  
  race_track_zoom_adj = horse_centres %>%
    mutate(tracks = map(frame_id, ~race_track_zoom)) %>%
    unnest(tracks) %>%
    ungroup() %>%
    mutate(x_inner_adj = scale_factor * (x_inner - x_centre) + mid_x) %>%
    mutate(y_inner_adj = scale_factor * (y_inner - y_centre) + mid_y) %>%
    mutate(x_outer_adj = scale_factor * (x_outer - x_centre) + mid_x) %>%
    mutate(y_outer_adj = scale_factor * (y_outer - y_centre) + mid_y)
  
  race_track_full_adj = horse_centres %>%
    mutate(tracks = map(frame_id, ~race_track_full)) %>%
    unnest(tracks) %>%
    ungroup() %>%
    mutate(x_inner_adj = scale_factor * (x_inner - x_centre) + mid_x) %>%
    mutate(y_inner_adj = scale_factor * (y_inner - y_centre) + mid_y) %>%
    mutate(x_outer_adj = scale_factor * (x_outer - x_centre) + mid_x) %>%
    mutate(y_outer_adj = scale_factor * (y_outer - y_centre) + mid_y)
  
  # Create anti-circle to cover up track that bleeds outside of magnifying circle
  x = seq(-1,1,0.01)
  y1 = sqrt(1 - x^2)
  y2 = -sqrt(1 - x^2)
  
  anticircle1 = data.frame(
    x = c(-Inf, -Inf, x, Inf, Inf),
    y = c(Inf, 0, y1, 0, Inf)
  ) %>%
    mutate(x = x * scaled_radius + mid_x) %>%
    mutate(y = y * scaled_radius + mid_y)
  
  anticircle2 = data.frame(
    x = c(-Inf, -Inf, x, Inf, Inf),
    y = c(-Inf, 0, y2, 0, -Inf)
  ) %>%
    mutate(x = x * scaled_radius + mid_x) %>%
    mutate(y = y * scaled_radius + mid_y)
  
  
  
  ## ADD IN DYNAMIC TRACK ##
  
  if (race_data$track_id[[1]] == "AQU") {
    
    animation_v1 = ggplot() +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "D" & outline_type == "outer" & run_up_type != "direct"), aes(x = x_adj, y = y_adj), fill = "#FFEED4") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "D" & outline_type == "outer" & run_up_type != "none"), aes(x = x_adj, y = y_adj), fill = "#FFEED4") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "D" & outline_type == "inner"), aes(x = x_adj, y = y_adj), fill = "#A4EDCA") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "O" & outline_type == "outer"), aes(x = x_adj, y = y_adj), fill = "#9BD3A3") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "O" & outline_type == "inner"), aes(x = x_adj, y = y_adj), fill = "#A4EDCA") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "I" & outline_type == "outer"), aes(x = x_adj, y = y_adj), fill = "#9BD3A3") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "I" & outline_type == "inner" & run_up_type != "direct"), aes(x = x_adj, y = y_adj), fill = "#A4EDCA") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "I" & outline_type == "inner" & run_up_type != "none"), aes(x = x_adj, y = y_adj), fill = "#9BD3A3") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "extra" & outline_type == "lake1"), aes(x = x_adj, y = y_adj), fill = "#C8ECF9") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "extra" & outline_type == "lake2"), aes(x = x_adj, y = y_adj), fill = "#C8ECF9")
    
    
  } else if (race_data$track_id[[1]] == "BEL") {
    
    animation_v1 = ggplot() +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "D" & outline_type == "outer" & run_up_type != "direct"), aes(x = x_adj, y = y_adj), fill = "#FFEED4") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "D" & outline_type == "outer" & run_up_type != "none"), aes(x = x_adj, y = y_adj), fill = "#FFEED4") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "D" & outline_type == "inner"), aes(x = x_adj, y = y_adj), fill = "#9BD3A3") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "O" & outline_type == "outer" & run_up_type %in% c("none", "partial1", "partial2")), aes(x = x_adj, y = y_adj), fill = "#A4EDCA") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "O" & outline_type == "outer" & run_up_type %in% c("direct1", "partial1")), aes(x = x_adj, y = y_adj), fill = "#A4EDCA") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "O" & outline_type == "outer" & run_up_type %in% c("direct2", "partial2")), aes(x = x_adj, y = y_adj), fill = "#A4EDCA") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "O" & outline_type == "inner"), aes(x = x_adj, y = y_adj), fill = "#9BD3A3") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "I" & outline_type == "outer" & run_up_type != "direct"), aes(x = x_adj, y = y_adj), fill = "#A4EDCA") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "I" & outline_type == "outer" & run_up_type != "none"), aes(x = x_adj, y = y_adj), fill = "#A4EDCA") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "I" & outline_type == "inner"), aes(x = x_adj, y = y_adj), fill = "#9BD3A3") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "extra" & outline_type == "lake1"), aes(x = x_adj, y = y_adj), fill = "#C8ECF9") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "extra" & outline_type == "lake2"), aes(x = x_adj, y = y_adj), fill = "#C8ECF9") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "extra" & outline_type == "island1"), aes(x = x_adj, y = y_adj), fill = "#9BD3A3") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "extra" & outline_type == "island2"), aes(x = x_adj, y = y_adj), fill = "#9BD3A3") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "extra" & outline_type == "island3"), aes(x = x_adj, y = y_adj), fill = "#9BD3A3") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "extra" & outline_type == "island4"), aes(x = x_adj, y = y_adj), fill = "#9BD3A3")
    
  } else {
    
    animation_v1 = ggplot() +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "D" & outline_type == "outer" & run_up_type != "direct"), aes(x = x_adj, y = y_adj), fill = "#FFEED4") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "D" & outline_type == "outer" & run_up_type != "none"), aes(x = x_adj, y = y_adj), fill = "#FFEED4") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "D" & outline_type == "inner"), aes(x = x_adj, y = y_adj), fill = "#A4EDCA") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "O" & outline_type == "outer"), aes(x = x_adj, y = y_adj), fill = "#9BD3A3") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "O" & outline_type == "inner"), aes(x = x_adj, y = y_adj), fill = "#A4EDCA") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "I" & outline_type == "outer"), aes(x = x_adj, y = y_adj), fill = "#9BD3A3") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "I" & outline_type == "inner"), aes(x = x_adj, y = y_adj), fill = "#A4EDCA") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "extra" & outline_type == "lake"), aes(x = x_adj, y = y_adj), fill = "#C8ECF9")
  }
  
  animation_v2 = animation_v1 +
    ## DYNAMIC FINISH LINES AND TRACK OUTLINES ##
    geom_segment(data = finish_lines_adj, aes(x = x_finish_1_adj, xend = x_finish_2_adj, y = y_finish_1_adj, yend = y_finish_2_adj), colour = "red") +
    #geom_segment(data = race_track_zoom_adj, aes(x = x_inner_adj, y = y_inner_adj, xend = x_outer_adj, yend = y_outer_adj, colour = stage), size = 0.5) +
    geom_segment(data = race_track_full_adj, aes(x = x_inner_adj, y = y_inner_adj, xend = x_outer_adj, yend = y_outer_adj, colour = stage), size = 1.2) +
    geom_path(data = race_track_zoom_adj, aes(x = x_inner_adj, y = y_inner_adj, group = stage, colour = stage), size = 1.2) +
    geom_path(data = race_track_zoom_adj, aes(x = x_outer_adj, y = y_outer_adj, group = stage, colour = stage), size = 1.2) +
    #ggnewscale::new_scale_colour() +
    ## ANTI-CIRCLE AND MAGNIFIED CIRCLE ##
    geom_polygon(data = anticircle1, aes(x = x, y = y), fill = "white") +
    geom_polygon(data = anticircle2, aes(x = x, y = y), fill = "white") +
    geom_circle(aes(x0 = mid_x, y0 = mid_y, r = scaled_radius))
  
  
  ## ADD IN STATIC TRACK ##
  
  if (race_data$track_id[[1]] == "AQU") {
    
    animation_v3 = animation_v2 +
      geom_polygon(data = race_track_viz %>% filter(course_type == "D" & outline_type == "outer" & run_up_type != "direct"), aes(x = x, y = y), fill = "#FFEED4") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "D" & outline_type == "outer" & run_up_type != "none"), aes(x = x, y = y), fill = "#FFEED4") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "D" & outline_type == "inner"), aes(x = x, y = y), fill = "#A4EDCA") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "O" & outline_type == "outer"), aes(x = x, y = y), fill = "#9BD3A3") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "O" & outline_type == "inner"), aes(x = x, y = y), fill = "#A4EDCA") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "I" & outline_type == "outer"), aes(x = x, y = y), fill = "#9BD3A3") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "I" & outline_type == "inner" & run_up_type != "direct"), aes(x = x, y = y), fill = "#A4EDCA") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "I" & outline_type == "inner" & run_up_type != "none"), aes(x = x, y = y), fill = "#9BD3A3") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "extra" & outline_type == "lake1"), aes(x = x, y = y), fill = "#C8ECF9") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "extra" & outline_type == "lake2"), aes(x = x, y = y), fill = "#C8ECF9")
    
  } else if (race_data$track_id[[1]] == "BEL") {
    
    animation_v3 = animation_v2 +
      geom_polygon(data = track_outlines_viz %>% filter(track_id == "BEL" & course_type == "D" & outline_type == "outer" & run_up_type != "direct"), aes(x = x, y = y), fill = "#FFEED4") +
      geom_polygon(data = track_outlines_viz %>% filter(track_id == "BEL" & course_type == "D" & outline_type == "outer" & run_up_type != "none"), aes(x = x, y = y), fill = "#FFEED4") +
      geom_polygon(data = track_outlines_viz %>% filter(track_id == "BEL" & course_type == "D" & outline_type == "inner"), aes(x = x, y = y), fill = "#9BD3A3") +
      geom_polygon(data = track_outlines_viz %>% filter(track_id == "BEL" & course_type == "O" & outline_type == "outer" & run_up_type %in% c("none", "partial1", "partial2")), aes(x = x, y = y), fill = "#A4EDCA") +
      geom_polygon(data = track_outlines_viz %>% filter(track_id == "BEL" & course_type == "O" & outline_type == "outer" & run_up_type %in% c("direct1", "partial1")), aes(x = x, y = y), fill = "#A4EDCA") +
      geom_polygon(data = track_outlines_viz %>% filter(track_id == "BEL" & course_type == "O" & outline_type == "outer" & run_up_type %in% c("direct2", "partial2")), aes(x = x, y = y), fill = "#A4EDCA") +
      geom_polygon(data = track_outlines_viz %>% filter(track_id == "BEL" & course_type == "O" & outline_type == "inner"), aes(x = x, y = y), fill = "#9BD3A3") +
      geom_polygon(data = track_outlines_viz %>% filter(track_id == "BEL" & course_type == "I" & outline_type == "outer" & run_up_type != "direct"), aes(x = x, y = y), fill = "#A4EDCA") +
      geom_polygon(data = track_outlines_viz %>% filter(track_id == "BEL" & course_type == "I" & outline_type == "outer" & run_up_type != "none"), aes(x = x, y = y), fill = "#A4EDCA") +
      geom_polygon(data = track_outlines_viz %>% filter(track_id == "BEL" & course_type == "I" & outline_type == "inner"), aes(x = x, y = y), fill = "#9BD3A3") +
      geom_polygon(data = track_outlines_viz %>% filter(track_id == "BEL" & course_type == "extra" & outline_type == "lake1"), aes(x = x, y = y), fill = "#C8ECF9") +
      geom_polygon(data = track_outlines_viz %>% filter(track_id == "BEL" & course_type == "extra" & outline_type == "lake2"), aes(x = x, y = y), fill = "#C8ECF9") +
      geom_polygon(data = track_outlines_viz %>% filter(track_id == "BEL" & course_type == "extra" & outline_type == "island1"), aes(x = x, y = y), fill = "#9BD3A3") +
      geom_polygon(data = track_outlines_viz %>% filter(track_id == "BEL" & course_type == "extra" & outline_type == "island2"), aes(x = x, y = y), fill = "#9BD3A3") +
      geom_polygon(data = track_outlines_viz %>% filter(track_id == "BEL" & course_type == "extra" & outline_type == "island3"), aes(x = x, y = y), fill = "#9BD3A3") +
      geom_polygon(data = track_outlines_viz %>% filter(track_id == "BEL" & course_type == "extra" & outline_type == "island4"), aes(x = x, y = y), fill = "#9BD3A3") +
      geom_segment(data = race_track_full %>% filter(track_id == "BEL" & course_type == "D"), aes(x = x_inner, xend = x_outer, y = y_inner, yend = y_outer, colour = stage), alpha = 1) +
      geom_path(data = race_track_full %>% filter(track_id == "BEL" & course_type == "D"), aes(x = x_inner, y = y_inner, colour = stage)) +
      geom_path(data = race_track_full %>% filter(track_id == "BEL" & course_type == "D"), aes(x = x_outer, y = y_outer, colour = stage)) +
      geom_segment(data = race_track_full %>% filter(track_id == "BEL" & course_type == "O"), aes(x = x_inner, xend = x_outer, y = y_inner, yend = y_outer, colour = stage), alpha = 0.3) +
      geom_path(data = race_track_full %>% filter(track_id == "BEL" & course_type == "O" & orig_stage == "run up 1"), aes(x = x_inner, y = y_inner, colour = stage), alpha = 0.3) +
      geom_path(data = race_track_full %>% filter(track_id == "BEL" & course_type == "O" & orig_stage == "run up 1"), aes(x = x_outer, y = y_outer, colour = stage), alpha = 0.3) +
      geom_path(data = race_track_full %>% filter(track_id == "BEL" & course_type == "O" & orig_stage != "run up 1"), aes(x = x_inner, y = y_inner, colour = stage), alpha = 0.3) +
      geom_path(data = race_track_full %>% filter(track_id == "BEL" & course_type == "O" & orig_stage != "run up 1"), aes(x = x_outer, y = y_outer, colour = stage), alpha = 0.3) +
      geom_segment(data = race_track_full %>% filter(track_id == "BEL" & course_type == "I"), aes(x = x_inner, xend = x_outer, y = y_inner, yend = y_outer, colour = stage), alpha = 0.3) +
      geom_path(data = race_track_full %>% filter(track_id == "BEL" & course_type == "I"), aes(x = x_inner, y = y_inner, colour = stage), alpha = 0.3) +
      geom_path(data = race_track_full %>% filter(track_id == "BEL" & course_type == "I"), aes(x = x_outer, y = y_outer, colour = stage), alpha = 0.3) +
      geom_segment(data = finish_lines %>% filter(track_id == "BEL"), aes(x = x_finish_1, xend = x_finish_1, y = y_finish_1, yend = y_finish_2), colour = "red", size = 1.5)
    
  } else {
    
    animation_v3 = animation_v2 +
      geom_polygon(data = race_track_viz %>% filter(course_type == "D" & outline_type == "outer" & run_up_type != "direct"), aes(x = x, y = y), fill = "#FFEED4") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "D" & outline_type == "outer" & run_up_type != "none"), aes(x = x, y = y), fill = "#FFEED4") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "D" & outline_type == "inner"), aes(x = x, y = y), fill = "#A4EDCA") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "O" & outline_type == "outer"), aes(x = x, y = y), fill = "#9BD3A3") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "O" & outline_type == "inner"), aes(x = x, y = y), fill = "#A4EDCA") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "I" & outline_type == "outer"), aes(x = x, y = y), fill = "#9BD3A3") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "I" & outline_type == "inner"), aes(x = x, y = y), fill = "#A4EDCA") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "extra" & outline_type == "lake"), aes(x = x, y = y), fill = "#C8ECF9")
  }
  
  animation_v4 = animation_v3 +
    ## ADD FINISH LINE AND TRACK OUTLINE ##
    #geom_segment(data = finish_lines, aes(x = x_finish_1, xend = x_finish_2, y = y_finish_1, yend = y_finish_2), colour = "red") +
    #geom_segment(data = race_track_full, aes(x = x_inner, y = y_inner, xend = x_outer, yend = y_outer), colour = "grey60", size = 0.4) +
    ## PLACEMENT CIRCLE AND CONNECTING LINE ##
    geom_segment(data = horse_centres, aes(x = x_centre, xend = mid_x, y = y_centre - magnify_radius, yend = high_y)) +
    geom_circle(data = horse_centres, aes(x0 = x_centre, y0 = y_centre, r = magnify_radius)) +
    ## SMALL HORSES ##
    geom_polygon(data = horse_polygons_adj, aes(x = x, y = y, group = horse_jockey), fill = "chocolate4", show.legend = FALSE) +
    ## MAGNIFIED HORSES ##
    labs(colour = "Track Stage") +
    geom_segment(data = race_data_adj, aes(x = x_adj, xend = x_inner_adj, y = y_adj, yend = y_inner_adj)) +
    ggnewscale::new_scale_colour() +
    geom_segment(data = horse_trails_adj, aes(x = x1_adj, xend = x2_adj, y = y1_adj, yend = y2_adj, alpha = alpha, colour = rel_distance), size = 2, show.legend = FALSE) +
    scale_colour_gradient2(low = "blue", mid = "grey80", high = "red") +
    ggnewscale::new_scale_colour() +
    geom_polygon(data = horse_polygons_adj, aes(x = x_adj, y = y_adj, group = horse_jockey), fill = "chocolate4", show.legend = FALSE) +
    geom_circle(data = race_data_adj, aes(x0 = x_adj, y0 = y_adj, r = 0.32 * scale_factor, fill = horse_jockey), show.legend = FALSE) +
    geom_text(data = race_data_adj, aes(x = x_adj, y = y_adj, label = rank, size = text_size), colour = "white", show.legend = FALSE) +
    scale_size_continuous(range = c(min(horse_centres$text_size), max(horse_centres$text_size))) +
    ## FORMATTING ##
    coord_fixed(xlim = c(min_x, max_x), ylim = c(low_y, max_y)) +
    theme_void() +
    scale_alpha_continuous(range = c(0,1))
  
  return(animation_v4)
  
}



# Animate a race with magnifying glass over horses and distance heatmap
animate_race_heatmap = function(race,
                                frame_start,
                                frame_end,
                                granularity,
                                tracking_data,
                                track_outlines_mod,
                                track_outlines_viz,
                                omitted_stages = c("left bend")) {
  
  ## BASIC DATA SETUP ##
  
  # Extract data relevant to the subject race
  race_data = tracking_data %>%
    filter(race_id == race) %>%
    mutate(horse_jockey = paste(horse_name, jockey, sep = " / ")) %>%
    group_by(horse_jockey) %>%
    arrange(horse_jockey, frame_id) %>%
    fill(distance_next, .direction = "downup") %>%
    mutate(distance_next2 = loess(distance_next ~ frame_id)$fitted) %>%
    ungroup() %>%
    group_by(frame_id) %>%
    mutate(rel_acceleration = acceleration - mean(acceleration)) %>%
    mutate(rel_distance = (distance_next2 - mean(distance_next2)) / sd(distance_next2)) %>%
    ungroup() %>%
    select(
      track_id, course_type, horse_jockey, frame_id, rank,
      rel_acceleration, rel_distance, x, y, angle, distance_next, dnf,
      track_marker, dist_to_track_inner
    )
  
  # Determine where the max distance marker occurs (i.e. distance marker flips back to zero)
  track_max = track_outlines_mod %>%
    group_by(track_id, course_type) %>%
    filter(distance == max(distance)) %>%
    ungroup() %>%
    select(track_id, course_type, max_marker = distance)
  
  # Determine where the run-up ends and the full track begins
  run_up_ends = track_outlines_mod %>%
    filter(grepl("run up", stage) & distance == 0) %>%
    select(track_id, course_type, run_up_type = stage, ru_x_inner = x_inner, ru_y_inner = y_inner, ru_x_outer = x_outer, ru_y_outer = y_outer)
  
  full_track_starts = run_up_ends %>%
    left_join(track_outlines_mod %>% filter(!grepl("run up", stage)), by = c("track_id", "course_type")) %>%
    mutate(euclid = sqrt((x_inner - ru_x_inner)^2 + (y_inner - ru_y_inner)^2 + (x_outer - ru_x_outer)^2 + (y_outer - ru_y_outer)^2)) %>%
    group_by(track_id, course_type, run_up_type) %>%
    filter(euclid == min(euclid)) %>%
    ungroup() %>%
    select(track_id, course_type, run_up_type, run_up_marker = distance)
  
  
  race_track_granular = track_outlines_mod %>%
    filter(track_id == race_data$track_id[[1]] & course_type == race_data$course_type[[1]]) %>%
    filter(!(stage %in% omitted_stages)) %>%
    mutate(distance10 = as.integer(round(distance,1)*10)) %>%
    slice(which(distance10 %% as.integer(granularity*10) == 0))
  
  race_track_zoom = track_outlines_mod %>%
    filter(track_id == race_data$track_id[[1]] & course_type == race_data$course_type[[1]]) %>%
    slice(which(distance %% 1 == 0))
  
  race_track_full = track_outlines_mod %>%
    filter(track_id == race_data$track_id[[1]] & course_type == race_data$course_type[[1]]) %>%
    slice(which(distance %% 10 == 0))
  
  race_track_viz = track_outlines_viz %>%
    filter(track_id == race_data$track_id[[1]])
  
  # Extract and reformat track finish lines
  finish_lines = track_outlines_viz %>%
    arrange(track_id) %>%
    filter(outline_type == "finish_line") %>%
    mutate(dummy = "finish", dummy2 = rep(1:2,9)) %>%
    pivot_wider(id_cols = c(track_id, course_type), names_from = c(dummy, dummy2), values_from = c(x,y)) %>%
    filter(track_id == race_data$track_id[[1]] & course_type == race_data$course_type[[1]]) %>%
    mutate(x_finish_1 = x_finish_2)
  
  race_data = race_data %>%
    filter(frame_id >= frame_start & frame_id <= frame_end)
  
  # Create data frames with horse shapes and comet trails
  horse_polygons = race_data %>%
    select(frame_id, horse_jockey, x, y, angle) %>%
    mutate(polygon = pmap(list(x, y, angle, 0.64, 2.4), get_horse_polygon)) %>%
    select(frame_id, horse_jockey, polygon) %>%
    unnest(polygon) %>%
    ungroup()
  
  horse_trails = race_data %>%
    select(frame_id, horse_jockey, x, y, angle, distance_next, rel_acceleration, rel_distance) %>%
    mutate(trail_length = case_when(
      is.na(distance_next) ~ 1,
      distance_next <= 0 ~ 0,
      TRUE ~ distance_next
    )) %>%
    mutate(trail = pmap(list(x, y, angle, 0.64, trail_length), get_trail_segments)) %>%
    select(frame_id, horse_jockey, rel_acceleration, rel_distance, distance_next, trail) %>%
    unnest(trail) %>%
    ungroup()
  
  
  ## ADJUST COORDINATES TO NARROW IN ON HORSES ##
  
  # Get various track information
  max_x = max(race_track_viz$x); min_x = min(race_track_viz$x)
  max_y = max(race_track_viz$y); min_y = min(race_track_viz$y)
  scaled_radius = (max_x - min_x) / 6
  mid_x = (max_x + min_x) / 2
  left_x = mid_x - scaled_radius
  right_x = mid_x + scaled_radius
  high_y = min_y - (max_y - min_y) / 3
  mid_y = min_y - (max_y - min_y) / 3 - scaled_radius
  low_y =  min_y - (max_y - min_y) / 3 - scaled_radius * 2
  shift = scaled_radius * 2 + (max_x - min_x) / 8
  track_radius = scaled_radius + (max_x - min_x) / 8
  extra_radius = scaled_radius + (max_x - min_x) / 16
  
  # Determine mean positioning of horses during race
  if (frame_start == frame_end) {
    horse_centres = race_data %>%
      filter(!dnf) %>%
      group_by(frame_id) %>%
      summarize(
        max_dist = max(dist(data.frame(x, y))),
        x_width = max(x) - min(x),
        y_width = max(y) - min(y),
        x_centre = (max(x) + min(x)) / 2,
        y_centre = (max(y) + min(y)) / 2
      ) %>%
      mutate(max_dist = ifelse(max_dist == -Inf, 0, max_dist)) %>%
      #mutate(magnify_radius_raw = (max_dist + 10) / 2) %>%
      #mutate(magnify_radius = loess(magnify_radius_raw ~ frame_id)$fitted) %>%
      mutate(magnify_radius = (max_dist + 10) / 2) %>%
      mutate(scale_factor = scaled_radius / magnify_radius) %>%
      mutate(text_size = 21 * 1/magnify_radius) %>%
      ungroup() %>%
      select(frame_id, x_centre, y_centre, magnify_radius, scale_factor, text_size)
  } else {
    horse_centres = race_data %>%
      filter(!dnf) %>%
      group_by(frame_id) %>%
      summarize(
        max_dist = max(dist(data.frame(x, y))),
        x_width = max(x) - min(x),
        y_width = max(y) - min(y),
        x_centre = (max(x) + min(x)) / 2,
        y_centre = (max(y) + min(y)) / 2
      ) %>%
      mutate(max_dist = ifelse(max_dist == -Inf, 0, max_dist)) %>%
      mutate(magnify_radius_raw = (max_dist + 10) / 2) %>%
      mutate(magnify_radius = loess(magnify_radius_raw ~ frame_id)$fitted) %>%
      #mutate(magnify_radius = (max_dist + 10) / 2) %>%
      mutate(scale_factor = scaled_radius / magnify_radius) %>%
      mutate(text_size = 25 * 1/magnify_radius) %>%
      ungroup() %>%
      select(frame_id, x_centre, y_centre, magnify_radius, scale_factor, text_size)
  }
  
  
  # Adjust coordinates for all data frames needed to create the viz
  race_data_adj = race_data %>%
    left_join(horse_centres, by = "frame_id") %>%
    mutate(x_adj = scale_factor * (x - x_centre) + mid_x) %>%
    mutate(y_adj = scale_factor * (y - y_centre) + mid_y)
  
  horse_polygons_adj = horse_polygons %>%
    left_join(horse_centres, by = "frame_id") %>%
    mutate(x_adj = scale_factor * (x - x_centre) + mid_x) %>%
    mutate(y_adj = scale_factor * (y - y_centre) + mid_y)
  
  horse_trails_adj = horse_trails %>%
    left_join(horse_centres, by = "frame_id") %>%
    mutate(x1_adj = scale_factor * (x1 - x_centre) + mid_x) %>%
    mutate(y1_adj = scale_factor * (y1 - y_centre) + mid_y) %>%
    mutate(x2_adj = scale_factor * (x2 - x_centre) + mid_x) %>%
    mutate(y2_adj = scale_factor * (y2 - y_centre) + mid_y)
  
  race_track_viz_adj = horse_centres %>%
    mutate(tracks = map(frame_id, ~race_track_viz)) %>%
    unnest(tracks) %>%
    ungroup() %>%
    mutate(x_adj = scale_factor * (x - x_centre) + mid_x) %>%
    mutate(y_adj = scale_factor * (y - y_centre) + mid_y)
  
  finish_lines_adj = horse_centres %>%
    mutate(tracks = map(frame_id, ~finish_lines)) %>%
    unnest(tracks) %>%
    ungroup() %>%
    mutate(x_finish_1_adj = scale_factor * (x_finish_1 - x_centre) + mid_x) %>%
    mutate(y_finish_1_adj = scale_factor * (y_finish_1 - y_centre) + mid_y) %>%
    mutate(x_finish_2_adj = scale_factor * (x_finish_2 - x_centre) + mid_x) %>%
    mutate(y_finish_2_adj = scale_factor * (y_finish_2 - y_centre) + mid_y)
  
  race_track_zoom_adj = horse_centres %>%
    mutate(tracks = map(frame_id, ~race_track_zoom)) %>%
    unnest(tracks) %>%
    ungroup() %>%
    mutate(x_inner_adj = scale_factor * (x_inner - x_centre) + mid_x) %>%
    mutate(y_inner_adj = scale_factor * (y_inner - y_centre) + mid_y) %>%
    mutate(x_outer_adj = scale_factor * (x_outer - x_centre) + mid_x) %>%
    mutate(y_outer_adj = scale_factor * (y_outer - y_centre) + mid_y)
  
  race_track_full_adj = horse_centres %>%
    mutate(tracks = map(frame_id, ~race_track_full)) %>%
    unnest(tracks) %>%
    ungroup() %>%
    mutate(x_inner_adj = scale_factor * (x_inner - x_centre) + mid_x) %>%
    mutate(y_inner_adj = scale_factor * (y_inner - y_centre) + mid_y) %>%
    mutate(x_outer_adj = scale_factor * (x_outer - x_centre) + mid_x) %>%
    mutate(y_outer_adj = scale_factor * (y_outer - y_centre) + mid_y)
  
  # Create anti-circle to cover up track that bleeds outside of magnifying circle
  x = seq(-1,1,0.01)
  y1 = sqrt(1 - x^2)
  y2 = -sqrt(1 - x^2)
  
  anticircle1 = data.frame(
    x = c(-Inf, -Inf, x, Inf, Inf),
    y = c(Inf, 0, y1, 0, Inf)
  ) %>%
    mutate(x = x * scaled_radius + mid_x - shift) %>%
    mutate(y = y * scaled_radius + mid_y) %>%
    mutate(x = ifelse(x == Inf, mid_x - scaled_radius, x))
  
  anticircle2 = data.frame(
    x = c(-Inf, -Inf, x, Inf, Inf),
    y = c(-Inf, 0, y2, 0, -Inf)
  ) %>%
    mutate(x = x * scaled_radius + mid_x - shift) %>%
    mutate(y = y * scaled_radius + mid_y) %>%
    mutate(x = ifelse(x == Inf, mid_x - scaled_radius, x))
  
  anticircle3 = data.frame(
    x = c(-Inf, -Inf, x, Inf, Inf),
    y = c(Inf, 0, y1, 0, Inf)
  ) %>%
    mutate(x = x * scaled_radius + mid_x) %>%
    mutate(y = y * scaled_radius + mid_y) %>%
    mutate(x = ifelse(x == -Inf, (mid_x - track_radius), x)) %>%
    mutate(x = ifelse(x == Inf, (mid_x + track_radius), x))
  
  anticircle4 = data.frame(
    x = c(-Inf, -Inf, x, Inf, Inf),
    y = c(-Inf, 0, y2, 0, -Inf)
  ) %>%
    mutate(x = x * scaled_radius + mid_x) %>%
    mutate(y = y * scaled_radius + mid_y) %>%
    mutate(x = ifelse(x == -Inf, (mid_x - track_radius), x)) %>%
    mutate(x = ifelse(x == Inf, (mid_x + track_radius), x))
  
  anticircle5 = data.frame(
    x = c(-Inf, -Inf, x, Inf, Inf),
    y = c(Inf, 0, y1, 0, Inf)
  ) %>%
    mutate(x = x * scaled_radius + mid_x + shift) %>%
    mutate(y = y * scaled_radius + mid_y) %>%
    mutate(x = ifelse(x == -Inf, mid_x + scaled_radius, x))
  
  anticircle6 = data.frame(
    x = c(-Inf, -Inf, x, Inf, Inf),
    y = c(-Inf, 0, y2, 0, -Inf)
  ) %>%
    mutate(x = x * scaled_radius + mid_x + shift) %>%
    mutate(y = y * scaled_radius + mid_y) %>%
    mutate(x = ifelse(x == -Inf, mid_x + scaled_radius, x))
  
  left_label = data.frame(
    x = c(-1, -1, x, 1, 1),
    y = c(-1.5, 0, y2, 0, -1.5)
  ) %>%
    mutate(x = x * scaled_radius + mid_x - shift) %>%
    mutate(y = y * scaled_radius + mid_y)
  
  centre_label = data.frame(
    x = c(-1, -1, x, 1, 1),
    y = c(-1.5, 0, y2, 0, -1.5)
  ) %>%
    mutate(x = x * scaled_radius + mid_x) %>%
    mutate(y = y * scaled_radius + mid_y)
  
  right_label = data.frame(
    x = c(-1, -1, x, 1, 1),
    y = c(-1.5, 0, y2, 0, -1.5)
  ) %>%
    mutate(x = x * scaled_radius + mid_x + shift) %>%
    mutate(y = y * scaled_radius + mid_y)
  
  
  ## CREATE HEATMAP GRID ##
  
  euclid_grid = horse_centres %>%
    mutate(tracks = map(frame_id, ~race_track_granular)) %>%
    mutate(tracks = pmap(list(tracks, x_centre, y_centre, magnify_radius), function(data,x,y,r) {
      data %>%
        mutate(inner_dist = sqrt((x_inner - x)^2 + (y_inner - y)^2)) %>%
        mutate(outer_dist = sqrt((x_outer - x)^2 + (y_outer - y)^2)) %>%
        filter(abs(inner_dist) <= sqrt(r^2 + r^2) | outer_dist <= sqrt(r^2 + r^2))
    })) %>%
    unnest(tracks) %>%
    ungroup() %>%
    mutate(lateral_data = pmap(list(x_inner,y_inner,x_outer,y_outer), function(xi,yi,xo,yo) {
      data.frame(prop = seq(granularity,sqrt((xi-xo)^2 + (yi-yo)^2),granularity)/sqrt((xi-xo)^2 + (yi-yo)^2)) %>%
        mutate(
          x_loc = prop * (xo - xi) + xi,
          y_loc = prop * (yo - yi) + yi
        )
    })) %>%
    unnest(lateral_data) %>%
    ungroup() %>%
    left_join(race_data %>% select(frame_id, horse_jockey, x, y), by = "frame_id") %>%
    mutate(horse_dist = sqrt((x_loc - x)^2 + (y_loc - y)^2)) %>%
    group_by(frame_id, x_loc, y_loc) %>%
    slice(which.min(horse_dist)) %>%
    ungroup() %>%
    mutate(x_adj = scale_factor * (x_loc - x_centre) + mid_x) %>%
    mutate(y_adj = scale_factor * (y_loc - y_centre) + mid_y) %>%
    mutate(horse_dist = ifelse(horse_dist > 10, 10, horse_dist)) %>%
    arrange(frame_id, x_loc, y_loc)
    
  forward_grid = horse_centres %>%
    mutate(tracks = map(frame_id, ~race_track_granular)) %>%
    mutate(tracks = pmap(list(tracks, x_centre, y_centre, magnify_radius), function(data,x,y,r) {
      data %>%
        mutate(inner_dist = sqrt((x_inner - x)^2 + (y_inner - y)^2)) %>%
        mutate(outer_dist = sqrt((x_outer - x)^2 + (y_outer - y)^2)) %>%
        filter(inner_dist <= sqrt(r^2 + r^2) | outer_dist <= sqrt(r^2 + r^2))
    })) %>%
    unnest(tracks) %>%
    ungroup() %>%
    mutate(lateral_data = pmap(list(x_inner,y_inner,x_outer,y_outer), function(xi,yi,xo,yo) {
      data.frame(prop = seq(granularity,sqrt((xi-xo)^2 + (yi-yo)^2),granularity)/sqrt((xi-xo)^2 + (yi-yo)^2)) %>%
        mutate(
          x_loc = prop * (xo - xi) + xi,
          y_loc = prop * (yo - yi) + yi
        )
    })) %>%
    unnest(lateral_data) %>%
    ungroup() %>%
    left_join(race_data %>% select(frame_id, horse_jockey, x, y, track_marker), by = "frame_id") %>%
    left_join(track_max, by = c("track_id", "course_type")) %>%
    group_by(horse_jockey) %>%
    mutate(run_up_type = ifelse(grepl("run up", first(unique(stage))), first(unique(stage)), NA)) %>%
    left_join(full_track_starts, by = c("track_id", "course_type", "run_up_type")) %>%
    mutate(fwd_dist = case_when(
      grepl("run up", stage) & grepl("run up", lead(stage)) ~ track_marker - distance,
      grepl("run up", stage) & !grepl("run up", lead(stage)) ~ track_marker + (distance - run_up_marker),
      track_marker >= max_marker - 10 & distance <= 10 ~ distance + (max_marker - track_marker),
      TRUE ~ distance - track_marker
    )) %>%
    mutate(fwd_dist = abs(fwd_dist)) %>%
    group_by(frame_id, x_loc, y_loc) %>%
    slice(which.min(fwd_dist)) %>%
    ungroup() %>%
    mutate(x_adj = scale_factor * (x_loc - x_centre) + mid_x) %>%
    mutate(y_adj = scale_factor * (y_loc - y_centre) + mid_y) %>%
    mutate(fwd_dist = ifelse(fwd_dist > 10, 10, fwd_dist)) %>%
    arrange(frame_id, x_loc, y_loc)
  
  lateral_grid = horse_centres %>%
    mutate(tracks = map(frame_id, ~race_track_granular)) %>%
    mutate(tracks = pmap(list(tracks, x_centre, y_centre, magnify_radius), function(data,x,y,r) {
      data %>%
        mutate(inner_dist = sqrt((x_inner - x)^2 + (y_inner - y)^2)) %>%
        mutate(outer_dist = sqrt((x_outer - x)^2 + (y_outer - y)^2)) %>%
        filter(inner_dist <= sqrt(r^2 + r^2) | outer_dist <= sqrt(r^2 + r^2))
    })) %>%
    unnest(tracks) %>%
    ungroup() %>%
    mutate(lateral_data = pmap(list(x_inner,y_inner,x_outer,y_outer), function(xi,yi,xo,yo) {
      data.frame(dist = seq(granularity,sqrt((xi-xo)^2 + (yi-yo)^2),granularity)) %>%
        mutate(
          prop = dist / sqrt((xi-xo)^2 + (yi-yo)^2),
          x_loc = prop * (xo - xi) + xi,
          y_loc = prop * (yo - yi) + yi
        )
    })) %>%
    unnest(lateral_data) %>%
    ungroup() %>%
    left_join(race_data %>% select(frame_id, horse_jockey, x, y, dist_to_track_inner), by = "frame_id") %>%
    mutate(lat_dist = abs(dist - dist_to_track_inner)) %>%
    group_by(frame_id, x_loc, y_loc) %>%
    slice(which.min(lat_dist)) %>%
    ungroup() %>%
    mutate(x_adj = scale_factor * (x_loc - x_centre) + mid_x) %>%
    mutate(y_adj = scale_factor * (y_loc - y_centre) + mid_y) %>%
    mutate(lat_dist = ifelse(lat_dist > 10, 10, lat_dist)) %>%
    arrange(frame_id, x_loc, y_loc)
  
  ## ADD IN DYNAMIC TRACK ##
  
  track_percise = track_outlines_mod %>%
    filter(track_id == race_data$track_id[[1]]) %>%
    slice(which(distance %% 1 == 0))
  
  percise_adj = horse_centres %>%
    mutate(tracks = map(frame_id, ~track_percise)) %>%
    unnest(tracks) %>%
    ungroup() %>%
    mutate(x_inner_adj = scale_factor * (x_inner - x_centre) + mid_x) %>%
    mutate(y_inner_adj = scale_factor * (y_inner - y_centre) + mid_y) %>%
    mutate(x_outer_adj = scale_factor * (x_outer - x_centre) + mid_x) %>%
    mutate(y_outer_adj = scale_factor * (y_outer - y_centre) + mid_y)
  
  shoot_adj1 = percise_adj %>%
    filter(grepl("run up", stage))
  
  shoot_adj2 = data.frame(
    frame_id = c(shoot_adj1$frame_id, rev(shoot_adj1$frame_id)),
    x_adj = c(shoot_adj1$x_inner_adj, rev(shoot_adj1$x_outer_adj)),
    y_adj = c(shoot_adj1$y_inner_adj, rev(shoot_adj1$y_outer_adj)),
    stage = c(shoot_adj1$stage, rev(shoot_adj1$stage)),
    course_type = c(shoot_adj1$course_type, rev(shoot_adj1$course_type))
  )
  
  if (race_data$track_id[[1]] == "AQU") {
    
    animation_v1 = ggplot() +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "D" & outline_type == "outer" & run_up_type != "direct"), aes(x = x_adj, y = y_adj), fill = "burlywood1") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "D" & outline_type == "outer" & run_up_type != "none"), aes(x = x_adj, y = y_adj), fill = "burlywood1") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "D" & outline_type == "inner"), aes(x = x_adj, y = y_adj), fill = "#26B76A") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "O" & outline_type == "outer"), aes(x = x_adj, y = y_adj), fill = "forestgreen") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "O" & outline_type == "inner"), aes(x = x_adj, y = y_adj), fill = "#26B76A") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "I" & outline_type == "outer"), aes(x = x_adj, y = y_adj), fill = "forestgreen") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "I" & outline_type == "inner" & run_up_type != "direct"), aes(x = x_adj, y = y_adj), fill = "#26B76A") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "I" & outline_type == "inner" & run_up_type != "none"), aes(x = x_adj, y = y_adj), fill = "forestgreen") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "extra" & outline_type == "lake1"), aes(x = x_adj, y = y_adj), fill = "skyblue") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "extra" & outline_type == "lake2"), aes(x = x_adj, y = y_adj), fill = "skyblue")
    
    
  } else if (race_data$track_id[[1]] == "BEL") {
    
    chute_data = shoot_adj2 %>%
      filter(course_type == "D" & stage == "run up" & x_adj >= mid_x - extra_radius)
    
    if (nrow(chute_data) == 0) {
      animation_v1 = ggplot() +
        geom_polygon(data = percise_adj %>% filter(course_type == "D" & stage != "run up" & (x_inner_adj >= mid_x - extra_radius | x_outer_adj >= mid_x - extra_radius) & !(x_inner_adj <= mid_x - extra_radius | x_outer_adj <= mid_x - extra_radius)), aes(x = x_outer_adj - shift, y = y_outer_adj), fill = "burlywood1") +
        geom_polygon(data = percise_adj %>% filter(course_type == "D" & stage != "run up" & (x_inner_adj >= mid_x - extra_radius | x_outer_adj >= mid_x - extra_radius) & !(x_inner_adj <= mid_x - extra_radius | x_outer_adj <= mid_x - extra_radius)), aes(x = x_inner_adj - shift, y = y_inner_adj), fill = "forestgreen") +
        geom_polygon(data = percise_adj %>% filter(course_type == "O" & stage != "run up" & x_outer_adj >= mid_x - extra_radius), aes(x = x_outer_adj - shift, y = y_outer_adj), fill = "#26B76A") +
        geom_polygon(data = percise_adj %>% filter(course_type == "O" & stage != "run up" & x_inner_adj >= mid_x - extra_radius), aes(x = x_inner_adj - shift, y = y_inner_adj), fill = "forestgreen") +
        geom_polygon(data = percise_adj %>% filter(course_type == "I" & stage != "run up" & x_outer_adj >= mid_x - extra_radius), aes(x = x_outer_adj - shift, y = y_outer_adj), fill = "#26B76A") +
        geom_polygon(data = percise_adj %>% filter(course_type == "I" & stage != "run up" & x_inner_adj >= mid_x - extra_radius), aes(x = x_inner_adj - shift, y = y_inner_adj), fill = "forestgreen") +
        geom_tile(data = forward_grid, aes(x = x_adj - shift, y = y_adj, fill = fwd_dist, width = scale_factor * (granularity + 0.1), height = scale_factor * (granularity + 0.1)), show.legend = FALSE) +
        scale_fill_viridis_c(
          alpha = 1, direction = -1
        ) +
        ggnewscale::new_scale_fill() +
        geom_segment(data = finish_lines_adj %>% mutate(show = x_finish_1_adj >= mid_x - extra_radius), aes(x = x_finish_1_adj - shift, xend = x_finish_1_adj - shift, y = y_finish_1_adj, yend = y_finish_2_adj, size = show), colour = "red", show.legend = FALSE) +
        scale_size_manual(values = c("FALSE" = 0, "TRUE" = 1.2)) +
        geom_segment(data = race_track_zoom_adj %>% filter((x_inner_adj >= mid_x - extra_radius | x_outer_adj >= mid_x - extra_radius) & !(x_outer_adj <= mid_x - extra_radius | x_inner_adj <= mid_x - extra_radius) & !(stage %in% omitted_stages)), aes(x = x_inner_adj - shift, y = y_inner_adj, xend = x_outer_adj - shift, yend = y_outer_adj), colour = "grey80", size = 0, alpha = 0.5) +
        geom_segment(data = race_track_full_adj %>% filter((x_inner_adj >= mid_x - extra_radius | x_outer_adj >= mid_x - extra_radius) & !(x_outer_adj <= mid_x - extra_radius | x_inner_adj <= mid_x - extra_radius) & !(stage %in% omitted_stages)), aes(x = x_inner_adj - shift, y = y_inner_adj, xend = x_outer_adj - shift, yend = y_outer_adj), colour = "grey60", size = 1.2, alpha = 0.5) +
        geom_path(data = race_track_zoom_adj %>% filter((x_inner_adj >= mid_x - extra_radius * 0.9 | x_outer_adj >= mid_x - track_radius + extra_radius * 0.9) & !(stage %in% omitted_stages)), aes(x = x_inner_adj - shift, y = y_inner_adj, group = stage), size = 1.2, colour = "grey60", alpha = 1) +
        geom_path(data = race_track_zoom_adj %>% filter((x_inner_adj >= mid_x - extra_radius * 0.9 | x_outer_adj >= mid_x - track_radius + extra_radius * 0.9) & !(stage %in% omitted_stages)), aes(x = x_outer_adj - shift, y = y_outer_adj, group = stage), size = 1.2, colour = "grey60", alpha = 1) +
        
        geom_polygon(data = percise_adj %>% filter(course_type == "D" & stage != "run up" & stage != "run up" & (x_inner_adj >= mid_x - extra_radius | x_outer_adj >= mid_x - extra_radius) & !(x_inner_adj <= mid_x - extra_radius | x_outer_adj <= mid_x - extra_radius)), aes(x = x_outer_adj, y = y_outer_adj), fill = "burlywood1") +
        geom_polygon(data = percise_adj %>% filter(course_type == "D" & stage != "run up" & stage != "run up" & (x_inner_adj >= mid_x - extra_radius | x_outer_adj >= mid_x - extra_radius) & !(x_inner_adj <= mid_x - extra_radius | x_outer_adj <= mid_x - extra_radius)), aes(x = x_inner_adj, y = y_inner_adj), fill = "forestgreen") +
        geom_polygon(data = percise_adj %>% filter(course_type == "O" & stage != "run up" & x_outer_adj >= mid_x - extra_radius), aes(x = x_outer_adj, y = y_outer_adj), fill = "#26B76A") +
        geom_polygon(data = percise_adj %>% filter(course_type == "O" & stage != "run up" & x_inner_adj >= mid_x - extra_radius), aes(x = x_inner_adj, y = y_inner_adj), fill = "forestgreen") +
        geom_polygon(data = percise_adj %>% filter(course_type == "I" & stage != "run up" & x_outer_adj >= mid_x - extra_radius), aes(x = x_outer_adj, y = y_outer_adj), fill = "#26B76A") +
        geom_polygon(data = percise_adj %>% filter(course_type == "I" & stage != "run up" & x_inner_adj >= mid_x - extra_radius), aes(x = x_inner_adj, y = y_inner_adj), fill = "forestgreen") +
        geom_tile(data = euclid_grid, aes(x = x_adj, y = y_adj, fill = horse_dist, width = scale_factor * (granularity + 0.1), height = scale_factor * (granularity + 0.1))) +
        labs(fill = "Distance to\nNearest Horse") +
        scale_fill_viridis_c(
          alpha = 1, direction = -1,
          limits = c(0,10),
          breaks = c(0, 2.5, 5, 7.5, 10),
          guide = guide_colorbar(barwidth = 0.8, barheight = 14)
        ) +
        ggnewscale::new_scale_fill() +
        geom_segment(data = finish_lines_adj %>% mutate(show = x_finish_1_adj >= mid_x - extra_radius), aes(x = x_finish_1_adj, xend = x_finish_1_adj, y = y_finish_1_adj, yend = y_finish_2_adj, size = show), colour = "red", show.legend = FALSE) +
        geom_segment(data = race_track_zoom_adj %>% filter((x_inner_adj >= mid_x - extra_radius | x_outer_adj >= mid_x - extra_radius) & !(x_outer_adj <= mid_x - extra_radius | x_inner_adj <= mid_x - extra_radius) & !(stage %in% omitted_stages)), aes(x = x_inner_adj, y = y_inner_adj, xend = x_outer_adj, yend = y_outer_adj), colour = "grey80", size = 0.5, alpha = 0.5) +
        geom_segment(data = race_track_full_adj %>% filter((x_inner_adj >= mid_x - extra_radius | x_outer_adj >= mid_x - extra_radius) & !(x_outer_adj <= mid_x - extra_radius | x_inner_adj <= mid_x - extra_radius) & !(stage %in% omitted_stages)), aes(x = x_inner_adj, y = y_inner_adj, xend = x_outer_adj, yend = y_outer_adj), colour = "grey60", size = 1.2, alpha = 0.5) +
        geom_path(data = race_track_zoom_adj %>% filter((x_inner_adj >= mid_x - scaled_radius | x_outer_adj >= mid_x - scaled_radius) & !(x_outer_adj <= mid_x - extra_radius | x_inner_adj <= mid_x - extra_radius) & !(stage %in% omitted_stages)), aes(x = x_inner_adj, y = y_inner_adj, group = stage), size = 1.2, colour = "grey60", alpha = 1) +
        geom_path(data = race_track_zoom_adj %>% filter((x_inner_adj >= mid_x - scaled_radius | x_outer_adj >= mid_x - scaled_radius) & !(x_outer_adj <= mid_x - extra_radius | x_inner_adj <= mid_x - extra_radius) & !(stage %in% omitted_stages)), aes(x = x_outer_adj, y = y_outer_adj, group = stage), size = 1.2, colour = "grey60", alpha = 1) +
        
        geom_polygon(data = percise_adj %>% filter(course_type == "D" & stage != "run up" & (x_inner_adj >= mid_x - extra_radius | x_outer_adj >= mid_x - extra_radius) & !(x_inner_adj <= mid_x - extra_radius | x_outer_adj <= mid_x - extra_radius)), aes(x = x_outer_adj + shift, y = y_outer_adj), fill = "burlywood1") +
        geom_polygon(data = percise_adj %>% filter(course_type == "D" & stage != "run up" & (x_inner_adj >= mid_x - extra_radius | x_outer_adj >= mid_x - extra_radius) & !(x_inner_adj <= mid_x - extra_radius | x_outer_adj <= mid_x - extra_radius)), aes(x = x_inner_adj + shift, y = y_inner_adj), fill = "forestgreen") +
        geom_polygon(data = percise_adj %>% filter(course_type == "O" & stage != "run up" & x_outer_adj >= mid_x - extra_radius), aes(x = x_outer_adj + shift, y = y_outer_adj), fill = "#26B76A") +
        geom_polygon(data = percise_adj %>% filter(course_type == "O" & stage != "run up" & x_inner_adj >= mid_x - extra_radius), aes(x = x_inner_adj + shift, y = y_inner_adj), fill = "forestgreen") +
        geom_polygon(data = percise_adj %>% filter(course_type == "I" & stage != "run up" & x_outer_adj >= mid_x - extra_radius), aes(x = x_outer_adj + shift, y = y_outer_adj), fill = "#26B76A") +
        geom_polygon(data = percise_adj %>% filter(course_type == "I" & stage != "run up" & x_inner_adj >= mid_x - extra_radius), aes(x = x_inner_adj + shift, y = y_inner_adj), fill = "forestgreen") +
        geom_tile(data = lateral_grid, aes(x = x_adj + shift, y = y_adj, fill = lat_dist, width = scale_factor * (granularity + 0.1), height = scale_factor * (granularity + 0.1)), show.legend = FALSE) +
        scale_fill_viridis_c(alpha = 1, direction = -1) +
        ggnewscale::new_scale_fill() +
        geom_segment(data = finish_lines_adj %>% mutate(show = x_finish_1_adj >= mid_x - extra_radius), aes(x = x_finish_1_adj + shift, xend = x_finish_1_adj + shift, y = y_finish_1_adj, yend = y_finish_2_adj, size = show), colour = "red", show.legend = FALSE) +
        ggnewscale::new_scale("size") +
        geom_segment(data = race_track_zoom_adj %>% filter((x_inner_adj >= mid_x - extra_radius | x_outer_adj >= mid_x - extra_radius) & !(x_outer_adj <= mid_x - extra_radius | x_inner_adj <= mid_x - extra_radius) & !(stage %in% omitted_stages)), aes(x = x_inner_adj + shift, y = y_inner_adj, xend = x_outer_adj + shift, yend = y_outer_adj), colour = "grey80", size = 0.5, alpha = 0.5) +
        geom_segment(data = race_track_full_adj %>% filter((x_inner_adj >= mid_x - extra_radius | x_outer_adj >= mid_x - extra_radius) & !(x_outer_adj <= mid_x - extra_radius | x_inner_adj <= mid_x - extra_radius) & !(stage %in% omitted_stages)), aes(x = x_inner_adj + shift, y = y_inner_adj, xend = x_outer_adj + shift, yend = y_outer_adj), colour = "grey60", size = 1.2, alpha = 0.5) +
        geom_path(data = race_track_zoom_adj %>% filter((x_inner_adj >= mid_x - scaled_radius | x_outer_adj >= mid_x - scaled_radius) & !(x_outer_adj <= mid_x - extra_radius | x_inner_adj <= mid_x - extra_radius) & !(stage %in% omitted_stages)), aes(x = x_inner_adj + shift, y = y_inner_adj, group = stage), size = 1.2, colour = "grey60", alpha = 1) +
        geom_path(data = race_track_zoom_adj %>% filter((x_inner_adj >= mid_x - scaled_radius | x_outer_adj >= mid_x - scaled_radius) & !(x_outer_adj <= mid_x - extra_radius | x_inner_adj <= mid_x - extra_radius) & !(stage %in% omitted_stages)), aes(x = x_outer_adj + shift, y = y_outer_adj, group = stage), size = 1.2, colour = "grey60", alpha = 1)
    } else {
      animation_v1 = ggplot() +
        geom_polygon(data = shoot_adj2 %>% filter(course_type == "D" & stage == "run up" & x_adj >= mid_x - extra_radius), aes(x = x_adj - shift, y = y_adj), fill = "burlywood1") +
        geom_polygon(data = percise_adj %>% filter(course_type == "D" & stage != "run up" & (x_inner_adj >= mid_x - extra_radius | x_outer_adj >= mid_x - extra_radius) & !(x_inner_adj <= mid_x - extra_radius | x_outer_adj <= mid_x - extra_radius)), aes(x = x_outer_adj - shift, y = y_outer_adj), fill = "burlywood1") +
        geom_polygon(data = percise_adj %>% filter(course_type == "D" & stage != "run up" & (x_inner_adj >= mid_x - extra_radius | x_outer_adj >= mid_x - extra_radius) & !(x_inner_adj <= mid_x - extra_radius | x_outer_adj <= mid_x - extra_radius)), aes(x = x_inner_adj - shift, y = y_inner_adj), fill = "forestgreen") +
        geom_polygon(data = percise_adj %>% filter(course_type == "O" & stage != "run up" & x_outer_adj >= mid_x - extra_radius), aes(x = x_outer_adj - shift, y = y_outer_adj), fill = "#26B76A") +
        geom_polygon(data = shoot_adj2 %>% filter(course_type == "O" & stage == "run up 1" & x_adj >= mid_x - extra_radius), aes(x = x_adj - shift, y = y_adj), fill = "#26B76A") +
        geom_polygon(data = shoot_adj2 %>% filter(course_type == "O" & stage == "run up 2" & x_adj >= mid_x - extra_radius), aes(x = x_adj - shift, y = y_adj), fill = "#26B76A") +
        geom_polygon(data = percise_adj %>% filter(course_type == "O" & stage != "run up" & x_inner_adj >= mid_x - extra_radius), aes(x = x_inner_adj - shift, y = y_inner_adj), fill = "forestgreen") +
        geom_polygon(data = shoot_adj2 %>% filter(course_type == "I" & stage == "run up" & x_adj >= mid_x - extra_radius), aes(x = x_adj - shift, y = y_adj), fill = "#26B76A") +
        geom_polygon(data = percise_adj %>% filter(course_type == "I" & stage != "run up" & x_outer_adj >= mid_x - extra_radius), aes(x = x_outer_adj - shift, y = y_outer_adj), fill = "#26B76A") +
        geom_polygon(data = percise_adj %>% filter(course_type == "I" & stage != "run up" & x_inner_adj >= mid_x - extra_radius), aes(x = x_inner_adj - shift, y = y_inner_adj), fill = "forestgreen") +
        geom_tile(data = forward_grid, aes(x = x_adj - shift, y = y_adj, fill = fwd_dist, width = scale_factor * (granularity + 0.1), height = scale_factor * (granularity + 0.1)), show.legend = FALSE) +
        scale_fill_viridis_c(
          alpha = 1, direction = -1
        ) +
        ggnewscale::new_scale_fill() +
        geom_segment(data = finish_lines_adj %>% mutate(show = x_finish_1_adj >= mid_x - extra_radius), aes(x = x_finish_1_adj - shift, xend = x_finish_1_adj - shift, y = y_finish_1_adj, yend = y_finish_2_adj, size = show), colour = "red", show.legend = FALSE) +
        scale_size_manual(values = c("FALSE" = 0, "TRUE" = 1.2)) +        geom_segment(data = race_track_zoom_adj %>% filter((x_inner_adj >= mid_x - extra_radius | x_outer_adj >= mid_x - extra_radius) & !(x_outer_adj <= mid_x - extra_radius | x_inner_adj <= mid_x - extra_radius) & !(stage %in% omitted_stages)), aes(x = x_inner_adj - shift, y = y_inner_adj, xend = x_outer_adj - shift, yend = y_outer_adj), colour = "grey80", size = 0.5, alpha = 0.5) +
        geom_segment(data = race_track_full_adj %>% filter((x_inner_adj >= mid_x - extra_radius | x_outer_adj >= mid_x - extra_radius) & !(x_outer_adj <= mid_x - extra_radius | x_inner_adj <= mid_x - extra_radius) & !(stage %in% omitted_stages)), aes(x = x_inner_adj - shift, y = y_inner_adj, xend = x_outer_adj - shift, yend = y_outer_adj), colour = "grey60", size = 1.2, alpha = 0.5) +
        geom_path(data = race_track_zoom_adj %>% filter((x_inner_adj >= mid_x - extra_radius * 0.9 | x_outer_adj >= mid_x - track_radius + extra_radius * 0.9) & !(stage %in% omitted_stages)), aes(x = x_inner_adj - shift, y = y_inner_adj, group = stage), size = 1.2, colour = "grey60", alpha = 1) +
        geom_path(data = race_track_zoom_adj %>% filter((x_inner_adj >= mid_x - extra_radius * 0.9 | x_outer_adj >= mid_x - track_radius + extra_radius * 0.9) & !(stage %in% omitted_stages)), aes(x = x_outer_adj - shift, y = y_outer_adj, group = stage), size = 1.2, colour = "grey60", alpha = 1) +
        
        geom_polygon(data = shoot_adj2 %>% filter(course_type == "D" & stage == "run up" & x_adj >= mid_x - extra_radius), aes(x = x_adj, y = y_adj), fill = "burlywood1") +
        geom_polygon(data = percise_adj %>% filter(course_type == "D" & stage != "run up" & stage != "run up" & (x_inner_adj >= mid_x - extra_radius | x_outer_adj >= mid_x - extra_radius) & !(x_inner_adj <= mid_x - extra_radius | x_outer_adj <= mid_x - extra_radius)), aes(x = x_outer_adj, y = y_outer_adj), fill = "burlywood1") +
        geom_polygon(data = percise_adj %>% filter(course_type == "D" & stage != "run up" & stage != "run up" & (x_inner_adj >= mid_x - extra_radius | x_outer_adj >= mid_x - extra_radius) & !(x_inner_adj <= mid_x - extra_radius | x_outer_adj <= mid_x - extra_radius)), aes(x = x_inner_adj, y = y_inner_adj), fill = "forestgreen") +
        geom_polygon(data = percise_adj %>% filter(course_type == "O" & stage != "run up" & x_outer_adj >= mid_x - extra_radius), aes(x = x_outer_adj, y = y_outer_adj), fill = "#26B76A") +
        geom_polygon(data = shoot_adj2 %>% filter(course_type == "O" & stage == "run up 1" & x_adj >= mid_x - extra_radius), aes(x = x_adj, y = y_adj), fill = "#26B76A") +
        geom_polygon(data = shoot_adj2 %>% filter(course_type == "O" & stage == "run up 2" & x_adj >= mid_x - extra_radius), aes(x = x_adj, y = y_adj), fill = "#26B76A") +
        geom_polygon(data = percise_adj %>% filter(course_type == "O" & stage != "run up" & x_inner_adj >= mid_x - extra_radius), aes(x = x_inner_adj, y = y_inner_adj), fill = "forestgreen") +
        geom_polygon(data = shoot_adj2 %>% filter(course_type == "I" & stage == "run up" & x_adj >= mid_x - extra_radius), aes(x = x_adj, y = y_adj), fill = "#26B76A") +
        geom_polygon(data = percise_adj %>% filter(course_type == "I" & stage != "run up" & x_outer_adj >= mid_x - extra_radius), aes(x = x_outer_adj, y = y_outer_adj), fill = "#26B76A") +
        geom_polygon(data = percise_adj %>% filter(course_type == "I" & stage != "run up" & x_inner_adj >= mid_x - extra_radius), aes(x = x_inner_adj, y = y_inner_adj), fill = "forestgreen") +
        geom_tile(data = euclid_grid, aes(x = x_adj, y = y_adj, fill = horse_dist, width = scale_factor * (granularity + 0.1), height = scale_factor * (granularity + 0.1))) +
        labs(fill = "Distance to\nNearest Horse") +
        scale_fill_viridis_c(
          alpha = 1, direction = -1,
          limits = c(0,10),
          breaks = c(0, 2.5, 5, 7.5, 10),
          guide = guide_colorbar(barwidth = 0.8, barheight = 14)
        ) +
        ggnewscale::new_scale_fill() +
        geom_segment(data = finish_lines_adj %>% mutate(show = x_finish_1_adj >= mid_x - extra_radius), aes(x = x_finish_1_adj, xend = x_finish_1_adj, y = y_finish_1_adj, yend = y_finish_2_adj, size = show), colour = "red", show.legend = FALSE) +
        geom_segment(data = race_track_zoom_adj %>% filter((x_inner_adj >= mid_x - extra_radius | x_outer_adj >= mid_x - extra_radius) & !(x_outer_adj <= mid_x - extra_radius | x_inner_adj <= mid_x - extra_radius) & !(stage %in% omitted_stages)), aes(x = x_inner_adj, y = y_inner_adj, xend = x_outer_adj, yend = y_outer_adj), colour = "grey80", size = 0.5, alpha = 0.5) +
        geom_segment(data = race_track_full_adj %>% filter((x_inner_adj >= mid_x - extra_radius | x_outer_adj >= mid_x - extra_radius) & !(x_outer_adj <= mid_x - extra_radius | x_inner_adj <= mid_x - extra_radius) & !(stage %in% omitted_stages)), aes(x = x_inner_adj, y = y_inner_adj, xend = x_outer_adj, yend = y_outer_adj), colour = "grey60", size = 1.2, alpha = 0.5) +
        geom_path(data = race_track_zoom_adj %>% filter((x_inner_adj >= mid_x - scaled_radius | x_outer_adj >= mid_x - scaled_radius) & !(x_outer_adj <= mid_x - extra_radius | x_inner_adj <= mid_x - extra_radius) & !(stage %in% omitted_stages)), aes(x = x_inner_adj, y = y_inner_adj, group = stage), size = 1.2, colour = "grey60", alpha = 1) +
        geom_path(data = race_track_zoom_adj %>% filter((x_inner_adj >= mid_x - scaled_radius | x_outer_adj >= mid_x - scaled_radius) & !(x_outer_adj <= mid_x - extra_radius | x_inner_adj <= mid_x - extra_radius) & !(stage %in% omitted_stages)), aes(x = x_outer_adj, y = y_outer_adj, group = stage), size = 1.2, colour = "grey60", alpha = 1) +
        
        geom_polygon(data = shoot_adj2 %>% filter(course_type == "D" & stage == "run up" & x_adj >= mid_x - extra_radius), aes(x = x_adj + shift, y = y_adj), fill = "burlywood1") +
        geom_polygon(data = percise_adj %>% filter(course_type == "D" & stage != "run up" & (x_outer_adj >= mid_x - extra_radius | x_outer_adj >= mid_x - extra_radius) & !(x_inner_adj <= mid_x - extra_radius | x_outer_adj <= mid_x - extra_radius)), aes(x = x_outer_adj + shift, y = y_outer_adj), fill = "burlywood1") +
        geom_polygon(data = percise_adj %>% filter(course_type == "D" & stage != "run up" & (x_inner_adj >= mid_x - extra_radius | x_outer_adj >= mid_x - extra_radius) & !(x_inner_adj <= mid_x - extra_radius | x_outer_adj <= mid_x - extra_radius)), aes(x = x_inner_adj + shift, y = y_inner_adj), fill = "forestgreen") +
        geom_polygon(data = percise_adj %>% filter(course_type == "O" & stage != "run up" & x_inner_adj >= mid_x - extra_radius), aes(x = x_outer_adj + shift, y = y_outer_adj), fill = "#26B76A") +
        geom_polygon(data = shoot_adj2 %>% filter(course_type == "O" & stage == "run up 1" & x_adj >= mid_x - extra_radius), aes(x = x_adj + shift, y = y_adj), fill = "#26B76A") +
        geom_polygon(data = shoot_adj2 %>% filter(course_type == "O" & stage == "run up 2" & x_adj >= mid_x - extra_radius), aes(x = x_adj + shift, y = y_adj), fill = "#26B76A") +
        geom_polygon(data = percise_adj %>% filter(course_type == "O" & stage != "run up" & x_outer_adj >= mid_x - extra_radius), aes(x = x_inner_adj + shift, y = y_inner_adj), fill = "forestgreen") +
        geom_polygon(data = shoot_adj2 %>% filter(course_type == "I" & stage == "run up" & x_adj >= mid_x - extra_radius), aes(x = x_adj + shift, y = y_adj), fill = "#26B76A") +
        geom_polygon(data = percise_adj %>% filter(course_type == "I" & stage != "run up" & x_inner_adj >= mid_x - extra_radius), aes(x = x_outer_adj + shift, y = y_outer_adj), fill = "#26B76A") +
        geom_polygon(data = percise_adj %>% filter(course_type == "I" & stage != "run up" & x_inner_adj >= mid_x - extra_radius), aes(x = x_inner_adj + shift, y = y_inner_adj), fill = "forestgreen") +
        geom_tile(data = lateral_grid, aes(x = x_adj + shift, y = y_adj, fill = lat_dist, width = scale_factor * (granularity + 0.1), height = scale_factor * (granularity + 0.1)), show.legend = FALSE) +
        scale_fill_viridis_c(alpha = 1, direction = -1) +
        ggnewscale::new_scale_fill() +
        geom_segment(data = finish_lines_adj %>% mutate(show = x_finish_1_adj >= mid_x - extra_radius), aes(x = x_finish_1_adj + shift, xend = x_finish_1_adj + shift, y = y_finish_1_adj, yend = y_finish_2_adj, size = show), colour = "red", show.legend = FALSE) +
        ggnewscale::new_scale("size") +        geom_segment(data = race_track_zoom_adj %>% filter((x_inner_adj >= mid_x - extra_radius | x_outer_adj >= mid_x - extra_radius) & !(x_outer_adj <= mid_x - extra_radius | x_inner_adj <= mid_x - extra_radius) & !(stage %in% omitted_stages)), aes(x = x_inner_adj + shift, y = y_inner_adj, xend = x_outer_adj + shift, yend = y_outer_adj), colour = "grey80", size = 0.5, alpha = 0.5) +
        geom_segment(data = race_track_full_adj %>% filter((x_inner_adj >= mid_x - extra_radius | x_outer_adj >= mid_x - extra_radius) & !(x_outer_adj <= mid_x - extra_radius | x_inner_adj <= mid_x - extra_radius) & !(stage %in% omitted_stages)), aes(x = x_inner_adj + shift, y = y_inner_adj, xend = x_outer_adj + shift, yend = y_outer_adj), colour = "grey60", size = 1.2, alpha = 0.5) +
        geom_path(data = race_track_zoom_adj %>% filter((x_inner_adj >= mid_x - scaled_radius | x_outer_adj >= mid_x - scaled_radius) & !(x_outer_adj <= mid_x - extra_radius | x_inner_adj <= mid_x - extra_radius) & !(stage %in% omitted_stages)), aes(x = x_inner_adj + shift, y = y_inner_adj, group = stage), size = 1.2, colour = "grey60", alpha = 1) +
        geom_path(data = race_track_zoom_adj %>% filter((x_inner_adj >= mid_x - scaled_radius | x_outer_adj >= mid_x - scaled_radius) & !(x_outer_adj <= mid_x - extra_radius | x_inner_adj <= mid_x - extra_radius) & !(stage %in% omitted_stages)), aes(x = x_outer_adj + shift, y = y_outer_adj, group = stage), size = 1.2, colour = "grey60", alpha = 1)
    }
    
    
      
    
  } else {
    
    animation_v1 = ggplot() +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "D" & outline_type == "outer" & run_up_type != "direct"), aes(x = x_adj, y = y_adj), fill = "burlywood1") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "D" & outline_type == "outer" & run_up_type != "none"), aes(x = x_adj, y = y_adj), fill = "burlywood1") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "D" & outline_type == "inner"), aes(x = x_adj, y = y_adj), fill = "#26B76A") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "O" & outline_type == "outer"), aes(x = x_adj, y = y_adj), fill = "forestgreen") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "O" & outline_type == "inner"), aes(x = x_adj, y = y_adj), fill = "#26B76A") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "I" & outline_type == "outer"), aes(x = x_adj, y = y_adj), fill = "forestgreen") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "I" & outline_type == "inner"), aes(x = x_adj, y = y_adj), fill = "#26B76A") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "extra" & outline_type == "lake"), aes(x = x_adj, y = y_adj), fill = "skyblue")
  }
  
  animation_v2 = animation_v1 +
    ## DYNAMIC FINISH LINES AND TRACK OUTLINES ##
    # Above now
    ## MAGNIFIED HORSES ##
    #geom_segment(data = horse_trails_adj %>% filter(x1_adj >= mid_x - extra_radius | x2_adj >= mid_x - extra_radius), aes(x = x1_adj, xend = x2_adj, y = y1_adj, yend = y2_adj, alpha = alpha, colour = rel_distance), size = 2, show.legend = FALSE) +
    #geom_segment(data = horse_trails_adj %>% filter(x1_adj >= mid_x - extra_radius | x2_adj >= mid_x - extra_radius), aes(x = x1_adj - shift, xend = x2_adj - shift, y = y1_adj, yend = y2_adj, alpha = alpha, colour = rel_distance), size = 2, show.legend = FALSE) +
    #geom_segment(data = horse_trails_adj %>% filter(x1_adj >= mid_x - extra_radius | x2_adj >= mid_x - extra_radius), aes(x = x1_adj + shift, xend = x2_adj + shift, y = y1_adj, yend = y2_adj, alpha = alpha, colour = rel_distance), size = 2, show.legend = FALSE) +
    scale_colour_gradient2(low = "blue", mid = "grey80", high = "red") +
    ggnewscale::new_scale_colour() +
    geom_polygon(data = horse_polygons_adj %>% filter(x_adj >= mid_x - extra_radius), aes(x = x_adj, y = y_adj, colour = horse_jockey), fill = "chocolate4", show.legend = FALSE) +
    geom_polygon(data = horse_polygons_adj %>% filter(x_adj >= mid_x - extra_radius), aes(x = x_adj - shift, y = y_adj, colour = horse_jockey), fill = "chocolate4", show.legend = FALSE) +
    geom_polygon(data = horse_polygons_adj %>% filter(x_adj >= mid_x - extra_radius), aes(x = x_adj + shift, y = y_adj, colour = horse_jockey), fill = "chocolate4", show.legend = FALSE) +
    geom_circle(data = race_data_adj %>% filter(x_adj >= mid_x - extra_radius), aes(x0 = x_adj, y0 = y_adj, r = 0.32 * scale_factor, fill = horse_jockey), show.legend = FALSE) +
    geom_text(data = race_data_adj %>% filter(x_adj >= mid_x - extra_radius), aes(x = x_adj, y = y_adj, label = rank, size = text_size), colour = "white", show.legend = FALSE) +
    geom_circle(data = race_data_adj %>% filter(x_adj >= mid_x - extra_radius), aes(x0 = x_adj - shift, y0 = y_adj, r = 0.32 * scale_factor, fill = horse_jockey), show.legend = FALSE) +
    geom_text(data = race_data_adj %>% filter(x_adj >= mid_x - extra_radius), aes(x = x_adj - shift, y = y_adj, label = rank, size = text_size), colour = "white", show.legend = FALSE) +
    geom_circle(data = race_data_adj %>% filter(x_adj >= mid_x - extra_radius), aes(x0 = x_adj + shift, y0 = y_adj, r = 0.32 * scale_factor, fill = horse_jockey), show.legend = FALSE) +
    geom_text(data = race_data_adj %>% filter(x_adj >= mid_x - extra_radius), aes(x = x_adj + shift, y = y_adj, label = rank, size = text_size), colour = "white", show.legend = FALSE) +
    scale_size_continuous(range = c(min(horse_centres$text_size), max(horse_centres$text_size))) +
    ## ANTI-CIRCLE AND MAGNIFIED CIRCLE ##
    geom_polygon(data = anticircle1, aes(x = x, y = y), fill = "white") +
    geom_polygon(data = anticircle2, aes(x = x, y = y), fill = "white") +
    geom_polygon(data = anticircle3, aes(x = x, y = y), fill = "white") +
    geom_polygon(data = anticircle4, aes(x = x, y = y), fill = "white") +
    geom_polygon(data = anticircle5, aes(x = x, y = y), fill = "white") +
    geom_polygon(data = anticircle6, aes(x = x, y = y), fill = "white") +
    geom_circle(aes(x0 = mid_x, y0 = mid_y, r = scaled_radius)) +
    geom_circle(aes(x0 = mid_x - shift, y0 = mid_y, r = scaled_radius)) +
    geom_circle(aes(x0 = mid_x + shift, y0 = mid_y, r = scaled_radius))
  
  
  ## ADD IN STATIC TRACK ##
  
  if (race_data$track_id[[1]] == "AQU") {
    
    animation_v3 = animation_v2 +
      geom_polygon(data = race_track_viz %>% filter(course_type == "D" & outline_type == "outer" & run_up_type != "direct"), aes(x = x, y = y), fill = "burlywood1") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "D" & outline_type == "outer" & run_up_type != "none"), aes(x = x, y = y), fill = "burlywood1") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "D" & outline_type == "inner"), aes(x = x, y = y), fill = "#26B76A") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "O" & outline_type == "outer"), aes(x = x, y = y), fill = "forestgreen") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "O" & outline_type == "inner"), aes(x = x, y = y), fill = "#26B76A") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "I" & outline_type == "outer"), aes(x = x, y = y), fill = "forestgreen") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "I" & outline_type == "inner" & run_up_type != "direct"), aes(x = x, y = y), fill = "#26B76A") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "I" & outline_type == "inner" & run_up_type != "none"), aes(x = x, y = y), fill = "forestgreen") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "extra" & outline_type == "lake1"), aes(x = x, y = y), fill = "skyblue") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "extra" & outline_type == "lake2"), aes(x = x, y = y), fill = "skyblue")
    
  } else if (race_data$track_id[[1]] == "BEL") {
    
    animation_v3 = animation_v2 +
      geom_polygon(data = race_track_viz %>% filter(course_type == "D" & outline_type == "outer" & run_up_type != "direct"), aes(x = x, y = y), fill = "burlywood1") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "D" & outline_type == "outer" & run_up_type != "none"), aes(x = x, y = y), fill = "burlywood1") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "D" & outline_type == "inner"), aes(x = x, y = y), fill = "forestgreen") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "O" & outline_type == "outer" & run_up_type %in% c("none", "partial1", "partial2")), aes(x = x, y = y), fill = "#26B76A") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "O" & outline_type == "outer" & run_up_type %in% c("direct1", "partial1")), aes(x = x, y = y), fill = "#26B76A") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "O" & outline_type == "outer" & run_up_type %in% c("direct2", "partial2")), aes(x = x, y = y), fill = "#26B76A") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "O" & outline_type == "inner"), aes(x = x, y = y), fill = "forestgreen") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "I" & outline_type == "outer" & run_up_type != "direct"), aes(x = x, y = y), fill = "#26B76A") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "I" & outline_type == "outer" & run_up_type != "none"), aes(x = x, y = y), fill = "#26B76A") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "I" & outline_type == "inner"), aes(x = x, y = y), fill = "forestgreen") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "extra" & outline_type == "lake1"), aes(x = x, y = y), fill = "skyblue") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "extra" & outline_type == "lake2"), aes(x = x, y = y), fill = "skyblue") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "extra" & outline_type == "island1"), aes(x = x, y = y), fill = "forestgreen") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "extra" & outline_type == "island2"), aes(x = x, y = y), fill = "forestgreen") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "extra" & outline_type == "island3"), aes(x = x, y = y), fill = "forestgreen") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "extra" & outline_type == "island4"), aes(x = x, y = y), fill = "forestgreen")
    
  } else {
    
    animation_v3 = animation_v2 +
      geom_polygon(data = race_track_viz %>% filter(course_type == "D" & outline_type == "outer" & run_up_type != "direct"), aes(x = x, y = y), fill = "burlywood1") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "D" & outline_type == "outer" & run_up_type != "none"), aes(x = x, y = y), fill = "burlywood1") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "D" & outline_type == "inner"), aes(x = x, y = y), fill = "#26B76A") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "O" & outline_type == "outer"), aes(x = x, y = y), fill = "forestgreen") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "O" & outline_type == "inner"), aes(x = x, y = y), fill = "#26B76A") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "I" & outline_type == "outer"), aes(x = x, y = y), fill = "forestgreen") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "I" & outline_type == "inner"), aes(x = x, y = y), fill = "#26B76A") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "extra" & outline_type == "lake"), aes(x = x, y = y), fill = "skyblue")
  }
  
  animation_v4 = animation_v3 +
    ## ADD FINISH LINE AND TRACK OUTLINE ##
    geom_segment(data = finish_lines, aes(x = x_finish_1, xend = x_finish_2, y = y_finish_1, yend = y_finish_2), colour = "red") +
    geom_segment(data = race_track_full, aes(x = x_inner, y = y_inner, xend = x_outer, yend = y_outer), colour = "grey60", size = 0.4) +
    ## PLACEMENT CIRCLE AND CONNECTING LINE ##
    geom_segment(data = horse_centres, aes(x = x_centre, xend = mid_x, y = y_centre - magnify_radius, yend = high_y + (max_y - min_y) / 6)) +
    geom_segment(data = horse_centres, aes(x = mid_x, xend = mid_x, y = high_y, yend = high_y + (max_y - min_y) / 6)) +
    geom_segment(data = horse_centres, aes(x = mid_x - shift, xend = mid_x, y = high_y + (max_y - min_y) / 9, yend = high_y + (max_y - min_y) / 6)) +
    geom_segment(data = horse_centres, aes(x = mid_x + shift, xend = mid_x, y = high_y + (max_y - min_y) / 9, yend = high_y + (max_y - min_y) / 6)) +
    geom_segment(data = horse_centres, aes(x = mid_x - shift, xend = mid_x - shift, y = high_y, yend = high_y + (max_y - min_y) / 9)) +
    geom_segment(data = horse_centres, aes(x = mid_x + shift, xend = mid_x + shift, y = high_y, yend = high_y + (max_y - min_y) / 9)) +
    geom_circle(data = horse_centres, aes(x0 = x_centre, y0 = y_centre, r = magnify_radius)) +
    ## SMALL HORSES ##
    geom_polygon(data = horse_polygons_adj, aes(x = x, y = y, colour = horse_jockey), fill = "chocolate4", show.legend = FALSE) +
    ## LABELS ##
    geom_polygon(data = right_label, aes(x = x, y = y), fill = "#9BD3A3", colour = "black") +
    geom_text(aes(x = (max(right_label$x) + min(right_label$x)) / 2, y = (max(right_label$y) + min(right_label$y)) / 1.7, label = "Lateral Distance"), size = 5.5) +
    geom_polygon(data = centre_label, aes(x = x, y = y), fill = "#9BD3A3", colour = "black") +
    geom_text(aes(x = (max(centre_label$x) + min(centre_label$x)) / 2, y = (max(centre_label$y) + min(centre_label$y)) / 1.7, label = "Euclidean Distance"), size = 5.5) +
    geom_polygon(data = left_label, aes(x = x, y = y), fill = "#9BD3A3", colour = "black") +
    geom_text(aes(x = (max(left_label$x) + min(left_label$x)) / 2, y = (max(left_label$y) + min(left_label$y)) / 1.7, label = "Forward Distance"), size = 5.5) +
    ## FORMATTING ##
    labs(fill = "Horse / Jockey") +
    coord_fixed(xlim = c(mid_x - track_radius * 2, mid_x + track_radius * 2), ylim = c(min(right_label$y - 100), max_y)) +
    theme_void() +
    scale_alpha_continuous(range = c(0,1))
  
  if (frame_start == frame_end ) {
    return(animation_v4)
  } else {
    return(
      animation_v4 +
        transition_time(frame_id)
    )
  }
  
}



# Animate a race with magnifying glass over horses
animate_win_prob = function(race,
                            dwp,
                            tracking_data,
                            track_outlines_mod,
                            track_outlines_viz) {
  
  ## BASIC DATA SETUP ##
  
  # Extract data relevant to the subject race
  race_data = tracking_data %>%
    filter(race_id == race) %>%
    mutate(horse_jockey = paste(horse_name, jockey, sep = " / ")) %>%
    group_by(horse_jockey) %>%
    arrange(horse_jockey, frame_id) %>%
    fill(distance_next, .direction = "downup") %>%
    mutate(distance_next2 = loess(distance_next ~ frame_id)$fitted) %>%
    ungroup() %>%
    group_by(frame_id) %>%
    mutate(rel_acceleration = acceleration - mean(acceleration)) %>%
    mutate(rel_distance = (distance_next2 - mean(distance_next2)) / sd(distance_next2)) %>%
    ungroup() %>%
    select(
      track_id, course_type, horse_jockey, frame_id, rank,
      rel_acceleration, rel_distance, x, y, angle, distance_next, dnf
    )
  
  race_track_zoom = track_outlines_mod %>%
    filter(track_id == race_data$track_id[[1]] & course_type == race_data$course_type[[1]]) %>%
    slice(which(distance %% 1 == 0))
  
  race_track_full = track_outlines_mod %>%
    filter(track_id == race_data$track_id[[1]] & course_type == race_data$course_type[[1]]) %>%
    slice(which(distance %% 10 == 0))
  
  race_track_viz = track_outlines_viz %>%
    filter(track_id == race_data$track_id[[1]])
  
  # Extract and reformat track finish lines
  finish_lines = track_outlines_viz %>%
    arrange(track_id) %>%
    filter(outline_type == "finish_line") %>%
    mutate(dummy = "finish", dummy2 = rep(1:2,9)) %>%
    pivot_wider(id_cols = c(track_id, course_type), names_from = c(dummy, dummy2), values_from = c(x,y)) %>%
    filter(track_id == race_data$track_id[[1]] & course_type == race_data$course_type[[1]]) %>%
    mutate(x_finish_1 = x_finish_2)
  
  # Create data frames with horse shapes and comet trails
  horse_polygons = race_data %>%
    select(frame_id, horse_jockey, x, y, angle) %>%
    mutate(polygon = pmap(list(x, y, angle, 0.64, 2.4), get_horse_polygon)) %>%
    select(frame_id, horse_jockey, polygon) %>%
    unnest(polygon) %>%
    ungroup()
  
  horse_trails = race_data %>%
    select(frame_id, horse_jockey, x, y, angle, distance_next, rel_acceleration, rel_distance) %>%
    mutate(trail_length = case_when(
      is.na(distance_next) ~ 1,
      distance_next <= 0 ~ 0,
      TRUE ~ distance_next
    )) %>%
    mutate(trail = pmap(list(x, y, angle, 0.64, trail_length), get_trail_segments)) %>%
    select(frame_id, horse_jockey, rel_acceleration, rel_distance, distance_next, trail) %>%
    unnest(trail) %>%
    ungroup()
  
  
  ## ADJUST COORDINATES TO NARROW IN ON HORSES ##
  
  # Get various track information
  max_x = max(race_track_viz$x); min_x = min(race_track_viz$x)
  max_y = max(race_track_viz$y); min_y = min(race_track_viz$y)
  scaled_radius = (max_x - min_x) * 4
  left_x = min_x
  mid_x = min_x + scaled_radius
  right_x = min_x + 2 * scaled_radius
  high_y = min_y - (max_y - min_y) / 4
  mid_y = high_y - scaled_radius
  low_y = min_y - 2 * scaled_radius
  
  # Determine mean positioning of horses during race
  horse_centres = race_data %>%
    filter(!dnf) %>%
    group_by(frame_id) %>%
    summarize(
      max_dist = max(dist(data.frame(x, y))),
      x_width = max(x) - min(x),
      y_width = max(y) - min(y),
      x_centre = (max(x) + min(x)) / 2,
      y_centre = (max(y) + min(y)) / 2
    ) %>%
    mutate(max_dist = ifelse(max_dist == -Inf, 0, max_dist)) %>%
    mutate(magnify_radius_raw = (max_dist + 10) / 2) %>%
    mutate(magnify_radius = loess(magnify_radius_raw ~ frame_id)$fitted) %>%
    mutate(scale_factor = scaled_radius / magnify_radius) %>%
    mutate(text_size = 25 * 1/magnify_radius) %>%
    ungroup() %>%
    select(frame_id, x_centre, y_centre, magnify_radius, scale_factor, text_size)
  
  # Adjust coordinates for all data frames needed to create the viz
  race_data_adj = race_data %>%
    left_join(horse_centres, by = "frame_id") %>%
    mutate(x_adj = scale_factor * (x - x_centre) + mid_x) %>%
    mutate(y_adj = scale_factor * (y - y_centre) + mid_y)
  
  horse_polygons_adj = horse_polygons %>%
    left_join(horse_centres, by = "frame_id") %>%
    mutate(x_adj = scale_factor * (x - x_centre) + mid_x) %>%
    mutate(y_adj = scale_factor * (y - y_centre) + mid_y)
  
  horse_trails_adj = horse_trails %>%
    left_join(horse_centres, by = "frame_id") %>%
    mutate(x1_adj = scale_factor * (x1 - x_centre) + mid_x) %>%
    mutate(y1_adj = scale_factor * (y1 - y_centre) + mid_y) %>%
    mutate(x2_adj = scale_factor * (x2 - x_centre) + mid_x) %>%
    mutate(y2_adj = scale_factor * (y2 - y_centre) + mid_y)
  
  race_track_viz_adj = horse_centres %>%
    mutate(tracks = map(frame_id, ~race_track_viz)) %>%
    unnest(tracks) %>%
    ungroup() %>%
    mutate(x_adj = scale_factor * (x - x_centre) + mid_x) %>%
    mutate(y_adj = scale_factor * (y - y_centre) + mid_y)
  
  finish_lines_adj = horse_centres %>%
    mutate(tracks = map(frame_id, ~finish_lines)) %>%
    unnest(tracks) %>%
    ungroup() %>%
    mutate(x_finish_1_adj = scale_factor * (x_finish_1 - x_centre) + mid_x) %>%
    mutate(y_finish_1_adj = scale_factor * (y_finish_1 - y_centre) + mid_y) %>%
    mutate(x_finish_2_adj = scale_factor * (x_finish_2 - x_centre) + mid_x) %>%
    mutate(y_finish_2_adj = scale_factor * (y_finish_2 - y_centre) + mid_y)
  
  race_track_zoom_adj = horse_centres %>%
    mutate(tracks = map(frame_id, ~race_track_zoom)) %>%
    unnest(tracks) %>%
    ungroup() %>%
    mutate(x_inner_adj = scale_factor * (x_inner - x_centre) + mid_x) %>%
    mutate(y_inner_adj = scale_factor * (y_inner - y_centre) + mid_y) %>%
    mutate(x_outer_adj = scale_factor * (x_outer - x_centre) + mid_x) %>%
    mutate(y_outer_adj = scale_factor * (y_outer - y_centre) + mid_y)
  
  race_track_full_adj = horse_centres %>%
    mutate(tracks = map(frame_id, ~race_track_full)) %>%
    unnest(tracks) %>%
    ungroup() %>%
    mutate(x_inner_adj = scale_factor * (x_inner - x_centre) + mid_x) %>%
    mutate(y_inner_adj = scale_factor * (y_inner - y_centre) + mid_y) %>%
    mutate(x_outer_adj = scale_factor * (x_outer - x_centre) + mid_x) %>%
    mutate(y_outer_adj = scale_factor * (y_outer - y_centre) + mid_y)
  
  # Create anti-circle to cover up track that bleeds outside of magnifying circle
  x = seq(-1,1,0.01)
  y1 = sqrt(1 - x^2)
  y2 = -sqrt(1 - x^2)
  
  anticircle1 = data.frame(
    x = c(-Inf, -Inf, x, Inf, Inf),
    y = c(Inf, 0, y1, 0, Inf)
  ) %>%
    mutate(x = x * scaled_radius + mid_x) %>%
    mutate(y = y * scaled_radius + mid_y)
  
  anticircle2 = data.frame(
    x = c(-Inf, -Inf, x, Inf, Inf),
    y = c(-Inf, 0, y2, 0, -Inf)
  ) %>%
    mutate(x = x * scaled_radius + mid_x) %>%
    mutate(y = y * scaled_radius + mid_y)
  
  # Determine final placement order
  placement_order = dwp %>%
    select(horse_jockey, actual_placement) %>%
    unique() %>%
    arrange(actual_placement)
  
  
  ## ADD IN DYNAMIC TRACK ##
  
  if (race_data$track_id[[1]] == "AQU") {
    
    animation_v1 = ggplot() +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "D" & outline_type == "outer" & run_up_type != "direct"), aes(x = x_adj, y = y_adj), fill = "burlywood1") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "D" & outline_type == "outer" & run_up_type != "none"), aes(x = x_adj, y = y_adj), fill = "burlywood1") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "D" & outline_type == "inner"), aes(x = x_adj, y = y_adj), fill = "#26B76A") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "O" & outline_type == "outer"), aes(x = x_adj, y = y_adj), fill = "forestgreen") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "O" & outline_type == "inner"), aes(x = x_adj, y = y_adj), fill = "#26B76A") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "I" & outline_type == "outer"), aes(x = x_adj, y = y_adj), fill = "forestgreen") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "I" & outline_type == "inner" & run_up_type != "direct"), aes(x = x_adj, y = y_adj), fill = "#26B76A") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "I" & outline_type == "inner" & run_up_type != "none"), aes(x = x_adj, y = y_adj), fill = "forestgreen") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "extra" & outline_type == "lake1"), aes(x = x_adj, y = y_adj), fill = "skyblue") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "extra" & outline_type == "lake2"), aes(x = x_adj, y = y_adj), fill = "skyblue")
    
    
  } else if (race_data$track_id[[1]] == "BEL") {
    
    animation_v1 = ggplot() +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "D" & outline_type == "outer" & run_up_type != "direct"), aes(x = x_adj, y = y_adj), fill = "burlywood1") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "D" & outline_type == "outer" & run_up_type != "none"), aes(x = x_adj, y = y_adj), fill = "burlywood1") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "D" & outline_type == "inner"), aes(x = x_adj, y = y_adj), fill = "forestgreen") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "O" & outline_type == "outer" & run_up_type %in% c("none", "partial1", "partial2")), aes(x = x_adj, y = y_adj), fill = "#26B76A") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "O" & outline_type == "outer" & run_up_type %in% c("direct1", "partial1")), aes(x = x_adj, y = y_adj), fill = "#26B76A") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "O" & outline_type == "outer" & run_up_type %in% c("direct2", "partial2")), aes(x = x_adj, y = y_adj), fill = "#26B76A") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "O" & outline_type == "inner"), aes(x = x_adj, y = y_adj), fill = "forestgreen") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "I" & outline_type == "outer" & run_up_type != "direct"), aes(x = x_adj, y = y_adj), fill = "#26B76A") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "I" & outline_type == "outer" & run_up_type != "none"), aes(x = x_adj, y = y_adj), fill = "#26B76A") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "I" & outline_type == "inner"), aes(x = x_adj, y = y_adj), fill = "forestgreen") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "extra" & outline_type == "lake1"), aes(x = x_adj, y = y_adj), fill = "skyblue") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "extra" & outline_type == "lake2"), aes(x = x_adj, y = y_adj), fill = "skyblue") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "extra" & outline_type == "island1"), aes(x = x_adj, y = y_adj), fill = "forestgreen") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "extra" & outline_type == "island2"), aes(x = x_adj, y = y_adj), fill = "forestgreen") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "extra" & outline_type == "island3"), aes(x = x_adj, y = y_adj), fill = "forestgreen") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "extra" & outline_type == "island4"), aes(x = x_adj, y = y_adj), fill = "forestgreen")
    
  } else {
    
    animation_v1 = ggplot() +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "D" & outline_type == "outer" & run_up_type != "direct"), aes(x = x_adj, y = y_adj), fill = "burlywood1") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "D" & outline_type == "outer" & run_up_type != "none"), aes(x = x_adj, y = y_adj), fill = "burlywood1") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "D" & outline_type == "inner"), aes(x = x_adj, y = y_adj), fill = "#26B76A") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "O" & outline_type == "outer"), aes(x = x_adj, y = y_adj), fill = "forestgreen") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "O" & outline_type == "inner"), aes(x = x_adj, y = y_adj), fill = "#26B76A") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "I" & outline_type == "outer"), aes(x = x_adj, y = y_adj), fill = "forestgreen") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "I" & outline_type == "inner"), aes(x = x_adj, y = y_adj), fill = "#26B76A") +
      geom_polygon(data = race_track_viz_adj %>% filter(course_type == "extra" & outline_type == "lake"), aes(x = x_adj, y = y_adj), fill = "skyblue")
  }
  
  animation_v2 = animation_v1 +
    ## DYNAMIC FINISH LINES AND TRACK OUTLINES ##
    geom_segment(data = finish_lines_adj, aes(x = x_finish_1_adj, xend = x_finish_2_adj, y = y_finish_1_adj, yend = y_finish_2_adj), colour = "red") +
    geom_segment(data = race_track_zoom_adj, aes(x = x_inner_adj, y = y_inner_adj, xend = x_outer_adj, yend = y_outer_adj), colour = "grey80", size = 0.5) +
    geom_segment(data = race_track_full_adj, aes(x = x_inner_adj, y = y_inner_adj, xend = x_outer_adj, yend = y_outer_adj), colour = "grey60", size = 1.2) +
    geom_path(data = race_track_zoom_adj, aes(x = x_inner_adj, y = y_inner_adj, group = stage), size = 1.2, colour = "grey60") +
    geom_path(data = race_track_zoom_adj, aes(x = x_outer_adj, y = y_outer_adj, group = stage), size = 1.2, colour = "grey60") +
    ## MAGNIFIED HORSES ##
    geom_segment(data = horse_trails_adj, aes(x = x1_adj, xend = x2_adj, y = y1_adj, yend = y2_adj, alpha = alpha, colour = rel_distance), size = 2, show.legend = FALSE) +
    scale_colour_gradient2(low = "blue", mid = "grey80", high = "red") +
    ggnewscale::new_scale_colour() +
    geom_polygon(data = horse_polygons_adj, aes(x = x_adj, y = y_adj, colour = factor(horse_jockey, levels = placement_order$horse_jockey)), fill = "chocolate4", show.legend = FALSE) +
    geom_circle(data = race_data_adj, aes(x0 = x_adj, y0 = y_adj, r = 0.32 * scale_factor, fill = factor(horse_jockey, levels = placement_order$horse_jockey))) +
    geom_text(data = race_data_adj, aes(x = x_adj, y = y_adj, label = rank, size = text_size), colour = "white", show.legend = FALSE) +
    scale_size_continuous(range = c(min(horse_centres$text_size), max(horse_centres$text_size))) +
    ## ANTI-CIRCLE AND MAGNIFIED CIRCLE ##
    geom_polygon(data = anticircle1, aes(x = x, y = y), fill = "white") +
    geom_polygon(data = anticircle2, aes(x = x, y = y), fill = "white") +
    geom_circle(aes(x0 = mid_x, y0 = mid_y, r = scaled_radius))
  
  
  ## ADD IN STATIC TRACK ##
  
  if (race_data$track_id[[1]] == "AQU") {
    
    animation_v3 = animation_v2 +
      geom_polygon(data = race_track_viz %>% filter(course_type == "D" & outline_type == "outer" & run_up_type != "direct"), aes(x = x, y = y), fill = "burlywood1") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "D" & outline_type == "outer" & run_up_type != "none"), aes(x = x, y = y), fill = "burlywood1") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "D" & outline_type == "inner"), aes(x = x, y = y), fill = "#26B76A") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "O" & outline_type == "outer"), aes(x = x, y = y), fill = "forestgreen") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "O" & outline_type == "inner"), aes(x = x, y = y), fill = "#26B76A") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "I" & outline_type == "outer"), aes(x = x, y = y), fill = "forestgreen") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "I" & outline_type == "inner" & run_up_type != "direct"), aes(x = x, y = y), fill = "#26B76A") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "I" & outline_type == "inner" & run_up_type != "none"), aes(x = x, y = y), fill = "forestgreen") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "extra" & outline_type == "lake1"), aes(x = x, y = y), fill = "skyblue") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "extra" & outline_type == "lake2"), aes(x = x, y = y), fill = "skyblue")
    
  } else if (race_data$track_id[[1]] == "BEL") {
    
    animation_v3 = animation_v2 +
      geom_polygon(data = race_track_viz %>% filter(course_type == "D" & outline_type == "outer" & run_up_type != "direct"), aes(x = x, y = y), fill = "burlywood1") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "D" & outline_type == "outer" & run_up_type != "none"), aes(x = x, y = y), fill = "burlywood1") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "D" & outline_type == "inner"), aes(x = x, y = y), fill = "forestgreen") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "O" & outline_type == "outer" & run_up_type %in% c("none", "partial1", "partial2")), aes(x = x, y = y), fill = "#26B76A") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "O" & outline_type == "outer" & run_up_type %in% c("direct1", "partial1")), aes(x = x, y = y), fill = "#26B76A") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "O" & outline_type == "outer" & run_up_type %in% c("direct2", "partial2")), aes(x = x, y = y), fill = "#26B76A") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "O" & outline_type == "inner"), aes(x = x, y = y), fill = "forestgreen") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "I" & outline_type == "outer" & run_up_type != "direct"), aes(x = x, y = y), fill = "#26B76A") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "I" & outline_type == "outer" & run_up_type != "none"), aes(x = x, y = y), fill = "#26B76A") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "I" & outline_type == "inner"), aes(x = x, y = y), fill = "forestgreen") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "extra" & outline_type == "lake1"), aes(x = x, y = y), fill = "skyblue") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "extra" & outline_type == "lake2"), aes(x = x, y = y), fill = "skyblue") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "extra" & outline_type == "island1"), aes(x = x, y = y), fill = "forestgreen") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "extra" & outline_type == "island2"), aes(x = x, y = y), fill = "forestgreen") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "extra" & outline_type == "island3"), aes(x = x, y = y), fill = "forestgreen") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "extra" & outline_type == "island4"), aes(x = x, y = y), fill = "forestgreen")
    
  } else {
    
    animation_v3 = animation_v2 +
      geom_polygon(data = race_track_viz %>% filter(course_type == "D" & outline_type == "outer" & run_up_type != "direct"), aes(x = x, y = y), fill = "burlywood1") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "D" & outline_type == "outer" & run_up_type != "none"), aes(x = x, y = y), fill = "burlywood1") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "D" & outline_type == "inner"), aes(x = x, y = y), fill = "#26B76A") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "O" & outline_type == "outer"), aes(x = x, y = y), fill = "forestgreen") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "O" & outline_type == "inner"), aes(x = x, y = y), fill = "#26B76A") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "I" & outline_type == "outer"), aes(x = x, y = y), fill = "forestgreen") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "I" & outline_type == "inner"), aes(x = x, y = y), fill = "#26B76A") +
      geom_polygon(data = race_track_viz %>% filter(course_type == "extra" & outline_type == "lake"), aes(x = x, y = y), fill = "skyblue")
  }
  
  animation_v4 = animation_v3 +
    ## ADD FINISH LINE AND TRACK OUTLINE ##
    geom_segment(data = finish_lines, aes(x = x_finish_1, xend = x_finish_2, y = y_finish_1, yend = y_finish_2), colour = "red") +
    geom_segment(data = race_track_full, aes(x = x_inner, y = y_inner, xend = x_outer, yend = y_outer), colour = "grey60", size = 0.4) +
    ## PLACEMENT CIRCLE AND CONNECTING LINE ##
    geom_segment(data = horse_centres, aes(x = x_centre, xend = mid_x - scaled_radius * sqrt(2)/2, y = y_centre - magnify_radius, yend = mid_y + scaled_radius * sqrt(2)/2)) +
    geom_circle(data = horse_centres, aes(x0 = x_centre, y0 = y_centre, r = magnify_radius)) +
    ## SMALL HORSES ##
    geom_polygon(data = horse_polygons_adj, aes(x = x, y = y, colour = horse_jockey), fill = "chocolate4", show.legend = FALSE) +
    ## FORMATTING ##
    transition_time(frame_id) +
    labs(fill = "Horse / Jockey") +
    coord_fixed(xlim = c(min_x, right_x), ylim = c(low_y, max_y)) +
    theme_void() +
    scale_alpha_continuous(range = c(0,1))
  
  
  ## CREATE DYNAMIC WIN PROBABILITY GIF ##
  
  
  
  # Order horses by placement
  dwp = dwp %>%
    mutate(horse_jockey = factor(horse_jockey, levels = placement_order$horse_jockey))
  
  # Find number of horses in race
  n_race_horses = length(unique(dwp$horse_jockey))
  
  # Fill in empty frames
  if (max(race_data$frame_id) > max(dwp$frame_id)) {
    missing_seq = seq(max(dwp$frame_id) + 1, max(race_data$frame_id))
    
    final_outcome = data.frame(horse_jockey = placement_order$horse_jockey, actual_placement = placement_order$actual_placement) %>%
      mutate(sim = map(horse_jockey, ~data.frame(simulated_placement = 1:n_race_horses))) %>%
      unnest() %>%
      ungroup() %>%
      mutate(prob = ifelse(actual_placement == simulated_placement, 1, 0)) %>%
      mutate(prob_mask = ifelse(prob <= 0.1, prob*10, prob + 0.9))
    
    final_outcome = bind_rows(final_outcome, final_outcome, final_outcome) %>%
      mutate(frame_id = rep(missing_seq, each = n()/3)) %>%
      unique()
    
    dwp = bind_rows(dwp, final_outcome)
  }
  
  # Create square heatmap of placement probabilities
  dwp_animation = ggplot(dwp) +
    geom_tile(aes(x = factor(simulated_placement), y = factor(horse_jockey, levels = placement_order$horse_jockey), fill = prob_mask), colour = "grey70") +
    scale_fill_viridis_c(
      breaks = c(0.00001, 0.5, 1, 1.5, 1.9), limits= c(0,1.9),
      labels = c("0", "0.05", "0.1", "0.5", "1"), option = "magma",
      guide = guide_colorbar(barwidth = 0.8, barheight = 14),
      na.value = "black"
    ) +
    scale_y_discrete(limits=rev, expand = c(0,0)) +
    scale_x_discrete(expand = c(0,0), breaks = 1:7) +
    theme_bw() +
    labs(x = "Finishing Place", y = "", fill = "Probability") +
    theme(axis.text.x = element_text(size = 14), axis.title = element_text(size = 12), axis.text.y = element_text(size = 10)) +
    coord_equal() +
    transition_time(frame_id)
  
  # Animate both gifs
  min_frame = min(dwp$frame_id)
  max_frame = max(dwp$frame_id)
  dwp_gif = animate(dwp_animation, nframes = max_frame - min_frame + 1, width = 7, height = 5, units = "in", res = 300, renderer = magick_renderer(), end_pause = 16)
  race_gif = animate(animation_v4, nframes = max_frame - min_frame + 1, width = 7, height = 5, units = "in", res = 300, renderer = magick_renderer(), end_pause = 16)
  
  new_gif = image_append(c(dwp_gif[1], race_gif[1]))
  
  for (i in 2:(max_frame-1)) {
    combined = image_append(c(dwp_gif[i], race_gif[i]))
    new_gif = c(new_gif, combined)
  }
  
  return(new_gif)
  
}





