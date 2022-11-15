
######################################
### STEP 1: BASIC DATA PREPARATION ###
######################################




########################################################
### STEP 2: CONVERT LONGITUDE AND LATITUDE TO METRES ###
########################################################

# Find the Haversine translation factors for each race course
# We can multiply these values by the horses' longitude/latitude coordinates to get distance in terms of metres
find_haversine_translations = function(data) {
  
  mean_longitude = mean(data$longitude)
  mean_latitude = mean(data$latitude)
  
  hav_longitude = distHaversine(p1 = c(mean_longitude, mean_latitude), p2 = c(mean_longitude + 1, mean_latitude))
  hav_latitude = distHaversine(p1 = c(mean_longitude, mean_latitude), p2 = c(mean_longitude, mean_latitude + 1))
  
  return(c("longitude" = hav_longitude, "latitude" = hav_latitude))
  
}



###########################################################################
### STEP 3: CALCULATE SPEED, ACCELERATION, ANGLES AND EXPECTED LOCATION ###
###########################################################################




###################################################################
### STEP 4: REORIENT TRACK SO THAT STRAIGHTAWAYS ARE HORIZONTAL ###
###################################################################

# Determine straightaway angle for a particular track
find_straightaway_angle = function(data, track) {
  
  data %>%
    filter(track_id == track) %>%
    mutate(angle = ifelse(angle > 180, angle - 180, angle)) %>%
    mutate(angle = round(angle,1)) %>%
    group_by(angle) %>%
    tally() %>%
    ungroup() %>%
    slice(which.max(n)) %>%
    select(angle) %>%
    unlist() %>%
    unname()
  
}



# Reorient the coordinates
reorient_coordinates = function(x_vec, y_vec, dir_vec, dir_orig, x_orig = 0, y_orig = 0) {
  
  # Centre xy-coordinates at origin of interest
  x_vec = x_vec - x_orig
  y_vec = y_vec - y_orig
  
  # Obtain projected locations
  x_proj = sin(dir_orig * pi / 180)
  y_proj = -cos(dir_orig * pi / 180)
  
  x_vec_proj = x_vec + x_proj
  y_vec_proj = y_vec + y_proj
  
  # Perform linear transformation on data
  a = y_proj
  b = -x_proj
  cc = x_proj
  d = y_proj
  
  x_vec_trans = a * x_vec + b * y_vec
  y_vec_trans = cc * x_vec + d * y_vec
  
  # Compute new directions
  dir_trans = 0
  dir_vec_trans = dir_vec + (dir_trans - dir_orig)
  
  return(
    data.frame(
      x_reor = x_vec_trans,
      y_reor = y_vec_trans,
      angle_reor = dir_vec_trans
    )
  )
  
}



################################################################
## STEP 5: BREAK DOWN TRACK INTO STAGES ALONG EACH 0.1 METRES ##
################################################################

# Convert cartesian coordinates (x,y) to polar coordinates (r, theta)
cart2polar <- function(x, y) {
  data.frame(r = sqrt(x^2 + y^2), theta = atan2(y, x))
}



# Interpolate along inner track to get a point 'dist' metres away from the beginning of the track outline
interpolate_inner_track = function(dist, inner_track) {
  
  closest_points = inner_track %>%
    mutate(is_greater = c_euclid > dist) %>%
    group_by(is_greater) %>%
    filter(abs(c_euclid - dist) == min(abs(c_euclid - dist))) %>%
    ungroup() %>%
    arrange(c_euclid)
  
  if (nrow(closest_points) == 1) {
    closest_points = inner_track %>%
      mutate(is_greater = c_euclid >= dist) %>%
      group_by(is_greater) %>%
      filter(abs(c_euclid - dist) == min(abs(c_euclid - dist))) %>%
      ungroup() %>%
      arrange(c_euclid)
  }
  
  euclid_prop = (dist - closest_points$c_euclid[1]) / (closest_points$c_euclid[2] - closest_points$c_euclid[1])
  
  x = closest_points$x[1] + euclid_prop * (closest_points$x[2] - closest_points$x[1])
  y = closest_points$y[1] + euclid_prop * (closest_points$y[2] - closest_points$y[1])
  
  return(data.frame(x = x, y = y))
  
}



# Determine stage of the track the horse is in (straightaway or bend) and calculate xy-coordinates of circle centre for bends
define_track_position = function(x, y, inner_track) {
  
  radius = (max(inner_track$y) - min(inner_track$y)) / 2
  
  x0 = min(inner_track$x) + radius
  y0 = min(inner_track$y) + radius
  
  x1 = max(inner_track$x) - radius
  y1 = min(inner_track$y) + radius
  
  type = case_when(
    x < min(x) + radius ~ "left bend",
    x > max(x) - radius ~ "right bend",
    y > max(y) - min(y) ~ "upper straightaway",
    y < max(y) - min(y) ~ "lower straightaway" 
  )
  
  x_circle = case_when(
    type == "left bend" ~ x0,
    type == "right bend" ~ x1,
    TRUE ~ x1 - x0
  )
  
  y_circle = case_when(
    type == "left bend" ~ y0,
    type == "right bend" ~ y1,
    TRUE ~ y1
  )
  
  polar_coords = cart2polar(x - x_circle, y - y_circle)
  
  return(data.frame(type, x_circle, y_circle, radius, r = polar_coords$r, theta = polar_coords$theta))
}



# Check if two lines intersect
check_intersection = function(x1, y1, x2, y2, fx1, fy1, fx2, fy2) {
  
  slope = (y2 - y1) / (x2 - x1)
  intercept = y2 - slope * x2
  
  
  fslope = (fy2 - fy1) / (fx2 - fx1)
  fintercept = fy2 - fslope * fx2
  
  intersect_x = case_when(
    slope %in% c(Inf, -Inf) ~ x1,
    fslope %in% c(Inf, -Inf) ~ fx1,
    TRUE ~ (fintercept - intercept) / (slope - fslope)
  )
  
  intersect_y = case_when(
    slope %in% c(Inf, -Inf) ~ fslope * intersect_x + fintercept,
    fslope %in% c(Inf, -Inf) ~ slope * intersect_x + intercept,
    TRUE ~ fslope * intersect_x + fintercept
  )
  
  is_intersect = ((intersect_x <= fx1 & intersect_x >= fx2) | (intersect_x <= fx2 & intersect_x >= fx1)) &
    ((intersect_x <= x1 & intersect_x >= x2) | (intersect_x <= x2 & intersect_x >= x1)) &
    ((intersect_y <= fy1 & intersect_y >= fy2) | (intersect_y <= fy2 & intersect_y >= fy1)) &
    ((intersect_y <= y1 & intersect_y >= y2) | (intersect_y <= y2 & intersect_y >= y1))
  
  return(is_intersect)
  
}



# Determine where two lines intersect
find_intersection = function(x1, y1, x2, y2, fx1, fy1, fx2, fy2) {
  
  slope = (y2 - y1) / (x2 - x1)
  intercept = y2 - slope * x2
  
  
  fslope = (fy2 - fy1) / (fx2 - fx1)
  fintercept = fy2 - fslope * fx2
  
  intersect_x = case_when(
    slope %in% c(Inf, -Inf) ~ x1,
    fslope %in% c(Inf, -Inf) ~ fx1,
    TRUE ~ (fintercept - intercept) / (slope - fslope)
  )
  
  intersect_y = case_when(
    slope %in% c(Inf, -Inf) ~ fslope * intersect_x + fintercept,
    fslope %in% c(Inf, -Inf) ~ slope * intersect_x + intercept,
    TRUE ~ fslope * intersect_x + fintercept
  )
  
  return(c(intersect_x, intersect_y))
  
}



# Match outer track to inner track so that we can connect them with perpendicular segments to the horses
match_outer_track = function(outer_track, type, x, y, x_circle, y_circle) {
  
  coords_list = list()
  
  for (i in 1:length(type)) {
    
    # Create line running perpendicular to horses
    if (type[i] == "lower straightaway") {
      x1 = x[i]; y1 = y[i]
      x2 = x[i]; y2 = y[i] - 100
    } else if (type[i] == "upper straightaway") {
      x1 = x[i]; y1 = y[i]
      x2 = x[i]; y2 = y[i] + 100
    } else if (type[i] == "left bend") {
      x1 = x[i]; y1 = y[i]
      x2 = ifelse(x[i] + (x[i] - x_circle[i]) <= x[i], x[i] + (x[i] - x_circle[i]), x[i])
      y2 = y[i] + (y[i] - y_circle[i])
    } else if (type[i] == "right bend") {
      x1 = x[i]; y1 = y[i]
      x2 = ifelse(x[i] + (x[i] - x_circle[i]) >= x[i], x[i] + (x[i] - x_circle[i]), x[i])
      y2 = y[i] + (y[i] - y_circle[i])
    } else {
      warning("Type not available!")
      return(data.frame(x_outer = NA, y_outer = NA))
    }
    
    # Determine where the line intersects on the outer track outline
    intersector = outer_track %>%
      mutate(is_intersect = check_intersection(x1, y1, x2, y2, x, y, xend, yend)) %>%
      filter(is_intersect)
    
    # Only use first point if there are more than two intersecting points
    if (nrow(intersector) > 1) {
      warning("More than 1 intersection for point, using closest!")
      intersector = intersector[1,]
    }
    
    # Determine the exact intersection point
    intersection = find_intersection(x1, y1, x2, y2, intersector$x, intersector$y, intersector$xend, intersector$yend)
    
    # Add to data
    coords_list[[i]] = data.frame(x_outer = intersection[1], y_outer = intersection[2])
    
  }
  
  return(coords_list)
  
}



# Interpolate along inner track to get a point 'dist' metres away from the beginning of the track outline
interpolate_run_up = function(dist, lower_points) {
  
  closest_points = lower_points %>%
    mutate(is_greater = c_euclid > dist) %>%
    group_by(is_greater) %>%
    filter(abs(c_euclid - dist) == min(abs(c_euclid - dist))) %>%
    ungroup() %>%
    arrange(c_euclid)
  
  euclid_prop = (dist - closest_points$c_euclid[1]) / (closest_points$c_euclid[2] - closest_points$c_euclid[1])
  
  x = closest_points$x[1] + euclid_prop * (closest_points$x[2] - closest_points$x[1])
  y = closest_points$y[1] + euclid_prop * (closest_points$y[2] - closest_points$y[1])
  
  return(data.frame(x = x, y = y))
  
}



# Match outer track to inner track so that we can connect them with perpendicular segments to the horses
match_run_up = function(x, y, track_id, course_type, upper_points) {
  
  coords_list = list()
  
  for (i in 1:length(x)) {
    
    # Create line running perpendicular to horses
    if (course_type == "D") {
      x1 = x[i]; y1 = y[i]
      x2 = x[i]; y2 = y[i] + 100
    } else if (course_type == "I" & track_id == "BEL") {
      x1 = x[i]; y1 = y[i]
      x2 = x[i] - 100
      y2 = y[i] + 100/2
    } else if (course_type == "I" & track_id == "AQU") {
      if (x[i] < 716) {
        x1 = x[i]; y1 = y[i]
        x2 = x[i]; y2 = y[i] + 100
      } else {
        x1 = x[i]; y1 = y[i]
        x2 = x[i] + 100
        y2 = y[i] + 100/0.385
      }
    } else if (course_type == "O1" & track_id == "BEL") {
      x1 = x[i]; y1 = y[i]
      x2 = x[i] - 100
      y2 = y[i] - 100*0.4
    } else if (course_type == "O2" & track_id == "BEL") {
      x1 = x[i]; y1 = y[i]
      x2 = x[i] - 100
      y2 = y[i] - 100*1.126
    }
    
    # Determine where the line intersects on the outer track outline
    intersector = upper_points %>%
      mutate(is_intersect = check_intersection(x1, y1, x2, y2, x, y, xend, yend)) %>%
      filter(is_intersect)
    
    # Only use first point if there are more than two intersecting points
    if (nrow(intersector) > 1) {
      warning("More than 1 intersection, using first!")
      intersector = intersector[1,]
    }
    
    # Determine the exact intersection point
    intersection = find_intersection(x1, y1, x2, y2, intersector$x, intersector$y, intersector$xend, intersector$yend)
    
    # Add to data
    coords_list[[i]] = data.frame(x_outer = intersection[1], y_outer = intersection[2])
    
  }
  
  return(coords_list)
  
}



# Full function to clean and uniformize tracks
clean_tracks = function(track_id, course_type, data, granularity) {
  
  ## CREATE INNER AND OUTER TRACK ##
  
  # Extract inner track
  inner_raw = data %>%
    filter(run_up_type != "direct" & outline_type == "inner") %>%
    add_row(.[1,]) %>%
    mutate(euclid = replace_na(sqrt((lag(x) - x)^2 + (lag(y) - y)^2), 0)) %>%
    mutate(c_euclid = cumsum(euclid))
  
  # Extract outer track and create connected segments along track
  outer_raw = data %>%
    filter(!(run_up_type %in% c("direct", "direct1", "direct2")) & outline_type == "outer") %>%
    add_row(.[1,]) %>%
    mutate(xend = lead(x), yend = lead(y)) %>%
    head(-1)
  
  # Obtain points along every granularity metres for inner track and project to find corresponding points on outer track
  smooth_track = data.frame(euclid = c(seq(0, max(inner_raw$c_euclid), granularity), max(inner_raw$c_euclid))) %>%
    mutate(inner_track = map(euclid, ~inner_raw)) %>%
    mutate(interpolated_coords = map2(euclid, inner_track, interpolate_inner_track)) %>%
    select(-inner_track) %>%
    unnest() %>%
    bind_cols(define_track_position(.$x, .$y, inner_raw)) %>%
    mutate(outer_coords = match_outer_track(outer_raw, type, x, y, x_circle, y_circle) ) %>%
    unnest() %>%
    select(stage = type, distance = euclid, x_inner = x, y_inner = y, x_outer, y_outer, x_circle, y_circle)
  
  
  ## CREATE RUN-UP (IF APPLICABLE) ##
  
  if (course_type == "D") {
    
    # Obtain start of lower straightaway track
    first_straightaway = smooth_track %>%
      filter(stage == "lower straightaway") %>%
      filter(x_inner == min(x_inner))
    
    # Create data frame of run-up data
    run_up = data %>%
      filter(run_up_type != "none") %>%
      filter(outline_type == "outer") %>%
      add_row(data.frame(x = first_straightaway$x_inner, y = first_straightaway$y_inner)) %>%
      add_row(data.frame(x = first_straightaway$x_outer, y = first_straightaway$y_outer)) %>%
      mutate(x = ifelse(x < 0, 0, x))
    
    # Organize points at the top and bottom of run-up area
    lower_side = run_up %>%
      filter(y < (max(y) + min(y)) / 2) %>%
      arrange(x) %>%
      mutate(euclid = replace_na(sqrt((x - lag(x))^2 + (y - lag(y))^2), 0)) %>%
      mutate(c_euclid = cumsum(euclid))
    
    upper_segments = run_up %>%
      filter(y > (max(y) + min(y)) / 2) %>%
      arrange(x) %>%
      mutate(xend = lead(x), yend = lead(y)) %>%
      head(-1)
    
    # Smooth out the run-up and take points along at every granularity metres
    smooth_run_up = data.frame(euclid = seq(0, floor(max(lower_side$c_euclid)), granularity)) %>%
      mutate(lower_side = map(euclid, ~lower_side)) %>%
      mutate(interpolated_coords = map2(euclid, lower_side, interpolate_run_up)) %>%
      select(-lower_side) %>%
      unnest() %>%
      mutate(outer_coords = match_run_up(x, y, "AQU", "D", upper_segments)) %>%
      unnest() %>%
      rename(x_inner = x_outer, y_inner = y_outer) %>%
      mutate(stage = "run up") %>%
      select(stage, distance = euclid, x_inner, y_inner, x_outer = x, y_outer = y) %>%
      mutate(
        x_inner = ifelse(is.na(x_inner), x_outer, x_inner),
        y_inner = ifelse(is.na(y_inner), mean(y_inner, na.rm = TRUE), y_inner)
      )
    
    # Combine data into one data frame
    final_track = bind_rows(smooth_track, smooth_run_up)
    
  } else if (course_type == "O" & track_id == "BEL") {
    
    ## RUN-UP 1 ##
    
    # Take point along left edge to match with run-up
    match_on_bend1 = smooth_track %>%
      filter(stage == "left bend") %>%
      slice(which.min(abs(distance - 1915)))
    
    # Create data frame of run-up data
    run_up1 = data %>%
      filter(run_up_type %in% c("direct1", "partial1")) %>%
      filter(outline_type == "outer") %>%
      mutate(type = c(rep("inner", 1), rep("outer", 9), rep("inner", 2))) %>%
      add_row(data.frame(x = match_on_bend1$x_inner, y = match_on_bend1$y_inner, type = "inner")) %>%
      add_row(data.frame(x = match_on_bend1$x_outer, y = match_on_bend1$y_outer, type = "outer"))
    
    # Organize points at the top and bottom of run-up area
    lower_side1 = run_up1 %>%
      filter(type != "outer") %>%
      arrange(desc(y)) %>%
      mutate(euclid = replace_na(sqrt((x - lag(x))^2 + (y - lag(y))^2), 0)) %>%
      mutate(c_euclid = cumsum(euclid))
    
    upper_segments1 = run_up1 %>%
      filter(type != "inner") %>%
      arrange(desc(y)) %>%
      mutate(xend = lead(x), yend = lead(y)) %>%
      head(-1)
    
    # Smooth out the run-up and take points along at every granularity metres
    smooth_run_up1 = data.frame(euclid = seq(0, floor(max(lower_side1$c_euclid)), granularity)) %>%
      mutate(lower_side1 = map(euclid, ~lower_side1)) %>%
      mutate(interpolated_coords = map2(euclid, lower_side1, interpolate_run_up)) %>%
      select(-lower_side1) %>%
      unnest() %>%
      mutate(outer_coords = match_run_up(x, y, "BEL", "O1", upper_segments1)) %>%
      unnest() %>%
      mutate(stage = "run up 1") %>%
      select(stage, distance = euclid, x_inner = x, y_inner = y, x_outer, y_outer) %>%
      mutate(
        x_outer = case_when(
          is.na(x_outer) & row_number() != 1 ~ match_on_bend1$x_outer,
          is.na(x_outer) & row_number() == 1 ~ x_inner,
          TRUE ~ x_outer
        ),
        y_outer = case_when(
          is.na(y_outer) & row_number() != 1 ~ match_on_bend1$y_outer,
          is.na(y_outer) & row_number() == 1 ~ y_inner,
          TRUE ~ y_outer
        )
      )
    
    
    ## RUN-UP 2 ##
    
    # Take point along left edge to match with run-up
    match_on_bend2 = smooth_track %>%
      filter(stage == "left bend") %>%
      slice(which.min(abs(distance - 1990)))
    
    # Create data frame of run-up data
    run_up2 = data %>%
      filter(run_up_type %in% c("direct2", "partial2")) %>%
      filter(outline_type == "outer") %>%
      mutate(type = c(rep("inner", 2), rep("outer", 6), rep("inner", 1))) %>%
      add_row(data.frame(x = match_on_bend2$x_inner, y = match_on_bend2$y_inner, type = "inner")) %>%
      add_row(data.frame(x = match_on_bend2$x_outer, y = match_on_bend2$y_outer, type = "outer"))
    
    # Organize points at the top and bottom of run-up area
    lower_side2 = run_up2 %>%
      filter(type != "outer") %>%
      arrange(desc(y)) %>%
      mutate(euclid = replace_na(sqrt((x - lag(x))^2 + (y - lag(y))^2), 0)) %>%
      mutate(c_euclid = cumsum(euclid))
    
    upper_segments2 = run_up2 %>%
      filter(type != "inner") %>%
      arrange(desc(y)) %>%
      mutate(xend = lead(x), yend = lead(y)) %>%
      head(-1)
    
    ggplot(lower_side2) +
      #geom_path(aes(x = x, y = y)) +
      geom_point(aes(x = x, y = y, colour = type)) +
      geom_point(data = upper_segments2, aes(x = x, y = y, colour = type)) +
      geom_segment(data = smooth_track, aes(x = x_inner, xend = x_outer, y = y_inner, yend = y_outer)) +
      geom_segment(data = match_on_bend2, aes(x = x_inner, xend = x_outer, y = y_inner, yend = y_outer), col = 'red') +
      coord_fixed()
    
    # Smooth out the run-up and take points along at every granularity metres
    smooth_run_up2 = data.frame(euclid = seq(0, floor(max(lower_side2$c_euclid)), granularity)) %>%
      mutate(lower_side2 = map(euclid, ~lower_side2)) %>%
      mutate(interpolated_coords = map2(euclid, lower_side2, interpolate_run_up)) %>%
      select(-lower_side2) %>%
      unnest() %>%
      mutate(outer_coords = match_run_up(x, y, "BEL", "O2", upper_segments2)) %>%
      unnest() %>%
      mutate(stage = "run up 2") %>%
      select(stage, distance = euclid, x_inner = x, y_inner = y, x_outer, y_outer) %>%
      mutate(
        x_outer = case_when(
          is.na(x_outer) & row_number() == nrow(.) ~ match_on_bend2$x_outer,
          TRUE ~ x_outer
        ),
        y_outer = case_when(
          is.na(y_outer) & row_number() == nrow(.) ~ match_on_bend2$y_outer,
          TRUE ~ y_outer
        )
      ) %>%
      filter(!is.na(x_outer))
    
    final_track = bind_rows(smooth_track, smooth_run_up1, smooth_run_up2)
    
  } else if (course_type == "I" & track_id == "BEL") {
    
    # Take point along left edge to match with run-up
    match_on_bend = smooth_track %>%
      filter(stage == "left bend") %>%
      slice(which.min(abs(distance - 1620)))
    
    # Create data frame of run-up data
    run_up = data %>%
      filter(run_up_type != "none") %>%
      filter(outline_type == "outer") %>%
      mutate(type = c(rep("inner", 4), "both", rep("outer", 7))) %>%
      add_row(data.frame(x = match_on_bend$x_inner, y = match_on_bend$y_inner, type = "inner")) %>%
      add_row(data.frame(x = match_on_bend$x_outer, y = match_on_bend$y_outer, type = "outer")) %>%
      mutate(x = ifelse(x < 0, 0, x))
    
    # Organize points at the top and bottom of run-up area
    lower_side = run_up %>%
      filter(type != "outer") %>%
      arrange(desc(y)) %>%
      mutate(euclid = replace_na(sqrt((x - lag(x))^2 + (y - lag(y))^2), 0)) %>%
      mutate(c_euclid = cumsum(euclid))
    
    upper_segments = run_up %>%
      filter(type != "inner") %>%
      arrange(desc(y)) %>%
      mutate(xend = lead(x), yend = lead(y)) %>%
      head(-1)
    
    # Smooth out the run-up and take points along at every granularity metres
    smooth_run_up = data.frame(euclid = seq(0, floor(max(lower_side$c_euclid)), granularity)) %>%
      mutate(lower_side = map(euclid, ~lower_side)) %>%
      mutate(interpolated_coords = map2(euclid, lower_side, interpolate_run_up)) %>%
      select(-lower_side) %>%
      unnest() %>%
      mutate(outer_coords = match_run_up(x, y, "BEL", "I", upper_segments)) %>%
      unnest() %>%
      mutate(stage = "run up") %>%
      select(stage, distance = euclid, x_inner = x, y_inner = y, x_outer, y_outer) %>%
      mutate(
        x_outer = case_when(
          is.na(x_outer) & row_number() != 1 ~ match_on_bend$x_outer,
          is.na(x_outer) & row_number() == 1 ~ x_inner,
          TRUE ~ x_outer
        ),
        y_outer = case_when(
          is.na(y_outer) & row_number() != 1 ~ match_on_bend$y_outer,
          is.na(y_outer) & row_number() == 1 ~ y_inner,
          TRUE ~ y_outer
        )
      )
    
    final_track = bind_rows(smooth_track, smooth_run_up)
    
  } else if (course_type == "I" & track_id == "AQU") {
    
    # Take point along left edge to match with run-up
    match_on_bend = smooth_track %>%
      filter(stage == "upper straightaway") %>%
      slice(which.min(abs(distance - 875)))
    
    # Create data frame of run-up data
    run_up = data %>%
      filter(run_up_type != "none") %>%
      filter(outline_type == "inner") %>%
      mutate(type = c(rep("outer", 4), rep("inner", 7), rep("outer", 2))) %>%
      add_row(data.frame(x = match_on_bend$x_inner, y = match_on_bend$y_inner, type = "inner")) %>%
      add_row(data.frame(x = match_on_bend$x_outer, y = match_on_bend$y_outer, type = "outer"))
    
    # Organize points at the top and bottom of run-up area
    lower_side = run_up %>%
      filter(type != "outer") %>%
      arrange(desc(x)) %>%
      mutate(euclid = replace_na(sqrt((x - lag(x))^2 + (y - lag(y))^2), 0)) %>%
      mutate(c_euclid = cumsum(euclid))
    
    upper_segments = run_up %>%
      filter(type != "inner") %>%
      arrange(desc(x)) %>%
      mutate(xend = lead(x), yend = lead(y)) %>%
      head(-1)
    
    # Smooth out the run-up and take points along at every granularity metres
    smooth_run_up = data.frame(euclid = seq(0, floor(max(lower_side$c_euclid)), granularity)) %>%
      mutate(lower_side = map(euclid, ~lower_side)) %>%
      mutate(interpolated_coords = map2(euclid, lower_side, interpolate_run_up)) %>%
      select(-lower_side) %>%
      unnest() %>%
      mutate(outer_coords = match_run_up(x, y, "AQU", "I", upper_segments)) %>%
      unnest() %>%
      mutate(stage = "run up") %>%
      select(stage, distance = euclid, x_inner = x, y_inner = y, x_outer, y_outer) %>%
      mutate(
        x_outer = case_when(
          is.na(x_outer) & row_number() != 1 ~ match_on_bend$x_outer,
          is.na(x_outer) & row_number() == 1 ~ x_inner,
          TRUE ~ x_outer
        ),
        y_outer = case_when(
          is.na(y_outer) & row_number() != 1 ~ match_on_bend$y_outer,
          is.na(y_outer) & row_number() == 1 ~ y_inner,
          TRUE ~ y_outer
        )
      )
    
    
    final_track = bind_rows(smooth_track, smooth_run_up)
    
  } else {
    
    final_track = smooth_track
    
  }
  
  return(final_track)
  
}





######################################################
## STEP 6: CALCULATE RANK ON A FRAME-BY-FRAME BASIS ##
######################################################

# Used to determine if the horse crosses the finish line in the past frame
check_intersection = function(x1, y1, x2, y2, fx1, fy1, fx2, fy2) {
  
  slope = (y2 - y1) / (x2 - x1)
  intercept = y2 - slope * x2
  
  
  fslope = (fy2 - fy1) / (fx2 - fx1)
  fintercept = fy2 - fslope * fx2
  
  intersect_x = case_when(
    slope %in% c(Inf, -Inf) ~ x1,
    fslope %in% c(Inf, -Inf) ~ fx1,
    TRUE ~ (fintercept - intercept) / (slope - fslope)
  )
  
  intersect_y = case_when(
    slope %in% c(Inf, -Inf) ~ fslope * intersect_x + fintercept,
    fslope %in% c(Inf, -Inf) ~ slope * intersect_x + intercept,
    TRUE ~ fslope * intersect_x + fintercept
  )
  
  is_intersect = ((intersect_x <= fx1 & intersect_x >= fx2) | (intersect_x <= fx2 & intersect_x >= fx1)) &
    ((intersect_x <= x1 & intersect_x >= x2) | (intersect_x <= x2 & intersect_x >= x1)) &
    ((intersect_y <= fy1 & intersect_y >= fy2) | (intersect_y <= fy2 & intersect_y >= fy1)) &
    ((intersect_y <= y1 & intersect_y >= y2) | (intersect_y <= y2 & intersect_y >= y1))
  
  return(is_intersect)
  
}



##############################################
## STEP 7: ANOMALY DETECTION AND IMPUTATION ##
##############################################

## SMOOTH OUT ODDITIES IN RACES ##

# Create function to interpolate tracking coordinates between two points
interpolate_coords = function(mean_motion_df, x1, y1, x2, y2) {
  mean_motion_df %>%
    mutate(
      x = prop_travelled * x2 + (1 - prop_travelled) * x1,
      y = prop_travelled * y2 + (1 - prop_travelled) * y1
    )
}



# (We don't end up using these)

# Impute over flagged frames with empirical regularized interpolation
impute_races = function(tracking_data) {
  
  # Obtain average speed and expected distance travelled by frame at average speed for horses without flagged issues
  correct_motion = tracking_data %>%
    filter(!issue_horse) %>%
    group_by(race_id, frame_id) %>%
    summarize(speed_avg = mean(speed)) %>%
    mutate(exp_distance = replace_na(lag(speed_avg)/4, 0))
  
  # Smooth over frames with issues using other horses as reference for relevant speed and acceleration
  horse_fix = tracking_data_s7a %>%
    filter(race_id == "AQU_20190218_8") %>%
    filter(issue_horse) %>%
    group_by(race_id, horse_id) %>%
    arrange(race_id, horse_id, frame_id) %>%
    left_join(correct_motion, by = c("race_id", "frame_id")) %>%
    # Remove cases with no other non-flagged horses
    filter(!is.na(speed_avg)) %>%
    # Determine when the speed starts to deviate from average and only consider frames where it does
    mutate(speed_deviates = abs(distance_prev - speed_avg/4) > 1) %>%
    # Find all frames within a second of the flagged frames
    mutate(flagged_frame = frame_issue |
             lag(frame_issue) %>% replace_na(FALSE) |
             lag(frame_issue,2) %>% replace_na(FALSE) |
             lag(frame_issue,3) %>% replace_na(FALSE) |
             lag(frame_issue,4) %>% replace_na(FALSE) |
             lead(frame_issue) %>% replace_na(FALSE) |
             lead(frame_issue,2) %>% replace_na(FALSE) |
             lead(frame_issue,3) %>% replace_na(FALSE) |
             lead(frame_issue,4) %>% replace_na(FALSE)
    ) %>%
    # Mark all frames that are directly connected to flagged frames but the speed is still deviating from avg
    mutate(flagged_window = connect_frames(speed_deviates, flagged_frame)) %>%
    # Remove observations not in the flagged window and make sure to separate multiple flagged windows in the same horse/race combo
    filter(flagged_window) %>%
    mutate(frame_jump = replace_na(frame_id - lag(frame_id), 1)) %>%
    mutate(is_jump = frame_jump != 1) %>%
    mutate(window_id = cumsum(is_jump)) %>%
    ungroup() %>%
    group_by(race_id, horse_id, window_id) %>%
    # Compute total distance travelled over flagged window
    mutate(euclid = replace_na(sqrt((x - lag(x))^2 + (y - lag(y))^2), 0)) %>%
    mutate(total_distance = sum(euclid)) %>%
    mutate(total_exp_distance = sum(exp_distance)) %>%
    mutate(prop_distance = exp_distance / total_exp_distance) %>%
    mutate(cum_prop = cumsum(prop_distance)) %>%
    mutate(imputed_distance = cum_prop * total_distance) %>%
    select(race_id, horse_id, window_id, frame_id, x, y, imputed_distance) %>%
    nest() %>%
    mutate(data = map(data, impute_bad_frames)) %>%
    unnest() %>%
    ungroup() %>%
    select(-window_id, -distance)
  
  # Add imputed data to full tracking data
  tracking_data_final = tracking_data %>%
    left_join(horse_fix, by = c("race_id", "horse_id", "frame_id")) %>%
    mutate(is_imputed = !is.na(imputed_x)) %>%
    rename(raw_x = x, raw_y = y) %>%
    mutate(x = ifelse(is_imputed, imputed_x, raw_x)) %>%
    mutate(y = ifelse(is_imputed, imputed_y, raw_y)) %>%
    select(-imputed_x, -imputed_y)
    
  return(tracking_data_final)
  
  
}

# Function specifically to impute the bad frames for a series of flagged frames
impute_bad_frames = function(data) {
  
  # Extract data
  frames = data$frame_id
  raw_x = data$x
  raw_y = data$y
  distance = data$imputed_distance
  
  # Find coordinates for imputed distances
  find_coords = function(dist) {
    
    if (max(unique_coords$c_euclid) == dist) {
      x = last(unique_coords$x)
      y = last(unique_coords$y)
    } else {
      known_points = unique_coords %>%
        mutate(is_greater = c_euclid > dist)
      
      lower_point = known_points %>%
        filter(!is_greater) %>%
        filter(c_euclid == max(c_euclid))
      
      upper_point = known_points %>%
        filter(is_greater) %>%
        filter(c_euclid == min(c_euclid))
      
      prop = (dist - lower_point$c_euclid) / (upper_point$c_euclid - lower_point$c_euclid)
      
      x1 = lower_point$x; y1 = lower_point$y
      x2 = upper_point$x; y2 = upper_point$y
      
      x = x1 + (x2 - x1) * prop
      y = y1 + (y2 - y1) * prop
    }
    
    return(data.frame(
      x = x,
      y = y
    ))
    
  }
  
  # Find all unique coordinates that are known in data
  unique_coords = data.frame(x = raw_x, y = raw_y) %>%
    unique() %>%
    mutate(euclid = replace_na(sqrt((x - lag(x))^2 + (y - lag(y))^2), 0)) %>%
    mutate(c_euclid = cumsum(euclid))
  
  # Impute coordinates that follow the known path from the data
  imputed_coordinates = data.frame(frame_id = frames, distance = distance) %>%
    mutate(end_coords = map(distance, find_coords)) %>%
    unnest()
  
  return(imputed_coordinates %>% rename(imputed_x = x, imputed_y = y))
  
}

# Determine which frames surrounding a flagged frame should also be incorporated in imputation
connect_frames = function(speed_issue, frame_issue) {
  
  final_vec = rep(NA,length(speed_issue))
  
  if (frame_issue[1]) {
    final_vec[1] = TRUE
  }
  
  for (i in 2:length(speed_issue)) {
    
    if (frame_issue[i]) {
      final_vec[i] = TRUE
    } else if (frame_issue[i-1] & speed_issue[i]) {
      final_vec[i] = TRUE
    } else {
      final_vec[i] = FALSE
    }
    
  }
  
  for (j in rev(1:(length(speed_issue) - 1))) {
    
    if ((frame_issue[j+1] & speed_issue[j])) {
      final_vec[j] = TRUE
    }
    
  }
  
  return(final_vec)
  
}



############################################################
## STEP 8: RECALCULATE SPEED, ACCELERATION AND DISTANCES ###
############################################################


##########################################################################
## STEP 9: MATCH TRACKING DATA WITH TRACK AND DETERMINE EXACT DISTANCES ##
##########################################################################

# 
assign_track_position = function(tracking_data, track_outlines) {
  
  track_outlines = ungroup(track_outlines)
  
  # Just look at every 10m to start
  track_outlines_trimmed = track_outlines %>%
    filter(distance %% 10 == 0)
  
  # Take first xy-coordinate
  x_val = tracking_data$x[1]
  y_val = tracking_data$y[1]
  
  # Determine if the closest track marker is in the run-up stage
  initial_stage = track_outlines_trimmed %>%
    filter(track_id == tracking_data$track_id[1] & course_type == tracking_data$course_type[1]) %>%
    mutate(euclid = sqrt((x_val - x_inner)^2 + (y_val - y_inner)^2)) %>%
    arrange(euclid) %>%
    head(1) %>%
    select(stage) %>%
    unlist() %>%
    unname()
  
  # Special case: AQU inner track
  if (tracking_data$track_id[1] == "AQU" & tracking_data$course_type[1] == "I" & tracking_data$race_distance_furlongs[1] == 8) {
    initial_stage = "upper straightaway"
  } else if (tracking_data$track_id[1] == "AQU" & tracking_data$course_type[1] == "I" & tracking_data$x[1] >= 750 & tracking_data$x[1] <= 850 & tracking_data$y[1] >= 200 & tracking_data$y[1] <= 247.5) {
    initial_stage = "run up"
  }
  
  stage = initial_stage
  
  closest_points = data.frame()
  
  # For each frame...
  for (i in 1:nrow(tracking_data)) {
    
    # Extract the xy-coordinate
    x_val = tracking_data$x[i]
    y_val = tracking_data$y[i]
    
    if (grepl("run up", stage)) {
      
      stage_name = stage
      
      track_outlines_trimmed = track_outlines %>%
        filter(grepl(stage_name, stage))
      
      closest_10 = track_outlines_trimmed %>%
        filter(track_id == tracking_data$track_id[i] & course_type == tracking_data$course_type[i]) %>%
        mutate(euclid = sqrt((x_val - x_inner)^2 + (y_val - y_inner)^2)) %>%
        arrange(euclid) %>%
        head(2)
      
      if (closest_10$distance[1] == 0) {
        stage = "regular track"
        
        track_outlines_trimmed = track_outlines %>%
          filter(!grepl("run up", stage)) %>%
          filter(distance %% 10 == 0)
        
        closest_10 = track_outlines_trimmed %>%
          filter(track_id == tracking_data$track_id[i] & course_type == tracking_data$course_type[i]) %>%
          mutate(euclid = sqrt((x_val - x_inner)^2 + (y_val - y_inner)^2)) %>%
          arrange(euclid) %>%
          head(2) %>%
          arrange(distance)
        
        if (!(0 %in% closest_10$distance) | 10 %in% closest_10$distance) {
          closest_01 = track_outlines %>%
            filter(!grepl("run up", stage)) %>%
            filter(track_id == tracking_data$track_id[i] & course_type == tracking_data$course_type[i]) %>%
            filter(distance >= closest_10$distance[1] & distance <= closest_10$distance[2]) %>%
            mutate(euclid = sqrt((x_val - x_inner)^2 + (y_val - y_inner)^2)) %>%
            slice(which.min(euclid)) %>%
            select(-track_id, -course_type) %>%
            mutate(num = i)
        } else {
          closest_01 = track_outlines %>%
            filter(!grepl("run up", stage)) %>%
            filter(track_id == tracking_data$track_id[i] & course_type == tracking_data$course_type[i]) %>%
            filter(distance >= max(closest_10$distance - 10) | distance <= 10) %>%
            mutate(euclid = sqrt((x_val - x_inner)^2 + (y_val - y_inner)^2)) %>%
            slice(which.min(euclid)) %>%
            select(-track_id, -course_type) %>%
            mutate(num = i)
        }
        
        closest_points = rbind(closest_points, closest_01)
      } else {
        
        closest_10 = closest_10 %>%
          arrange(distance)
        
        if (!(max(track_outlines_trimmed$distance) %in% closest_10$distance)) {
          closest_01 = track_outlines %>%
            filter(grepl(stage_name, stage)) %>%
            filter(track_id == tracking_data$track_id[i] & course_type == tracking_data$course_type[i]) %>%
            filter(distance >= closest_10$distance[1] & distance <= closest_10$distance[2]) %>%
            mutate(euclid = sqrt((x_val - x_inner)^2 + (y_val - y_inner)^2)) %>%
            slice(which.min(euclid)) %>%
            select(-track_id, -course_type) %>%
            mutate(num = i)
        } else {
          closest_01 = track_outlines %>%
            filter(grepl(stage_name, stage)) %>%
            filter(track_id == tracking_data$track_id[i] & course_type == tracking_data$course_type[i]) %>%
            filter(distance >= max(closest_10$distance) - 10) %>%
            mutate(euclid = sqrt((x_val - x_inner)^2 + (y_val - y_inner)^2)) %>%
            slice(which.min(euclid)) %>%
            select(-track_id, -course_type) %>%
            mutate(num = i)
        }
        
        closest_points = rbind(closest_points, closest_01)
        
      }
      
    } else {
      
      track_outlines_trimmed = track_outlines %>%
        filter(!grepl("run up", stage)) %>%
        filter(distance %% 10 == 0)
      
      closest_10 = track_outlines_trimmed %>%
        filter(track_id == tracking_data$track_id[i] & course_type == tracking_data$course_type[i]) %>%
        mutate(euclid = sqrt((x_val - x_inner)^2 + (y_val - y_inner)^2)) %>%
        arrange(euclid) %>%
        head(2) %>%
        arrange(distance)
      
      if (!(0 %in% closest_10$distance) | 10 %in% closest_10$distance) {
        closest_01 = track_outlines %>%
          filter(!grepl("run up", stage)) %>%
          filter(track_id == tracking_data$track_id[i] & course_type == tracking_data$course_type[i]) %>%
          filter(distance >= closest_10$distance[1] & distance <= closest_10$distance[2]) %>%
          mutate(euclid = sqrt((x_val - x_inner)^2 + (y_val - y_inner)^2)) %>%
          slice(which.min(euclid)) %>%
          select(-track_id, -course_type) %>%
          mutate(num = i)
      } else {
        closest_01 = track_outlines %>%
          filter(!grepl("run up", stage)) %>%
          filter(track_id == tracking_data$track_id[i] & course_type == tracking_data$course_type[i]) %>%
          filter(distance >= max(closest_10$distance - 10) | distance <= 10) %>%
          mutate(euclid = sqrt((x_val - x_inner)^2 + (y_val - y_inner)^2)) %>%
          slice(which.min(euclid)) %>%
          select(-track_id, -course_type) %>%
          mutate(num = i)
      }
      
      closest_points = rbind(closest_points, closest_01)
      
    }
    
  }
  
  closest_points = closest_points %>%
    select(stage, distance_marker = distance, x_inner, y_inner, x_outer, y_outer, distance_to_inside = euclid)
  
  output_df = tracking_data %>%
    bind_cols(closest_points) %>%
    mutate(distance_to_outside = sqrt((x - x_outer)^2 + (y - y_outer)^2)) %>%
    mutate(track_width = sqrt((x_inner - x_outer)^2 + (y_inner - y_outer)^2)) %>%
    mutate(inside_track = track_width > distance_to_outside)
  
  return(output_df)
  
  
}


# Create function to define drafting region
draft = function(distance_behind, horse_width = 1.2, max_distance_behind = 7) {
  
  (horse_width / 2) * log((max_distance_behind + 1) - distance_behind) / log(max_distance_behind + 1)
  
}


# Calculate pairwise distances between horses
calculate_pairwise_distances = function(frame_data) {
  
  distances = expand.grid(horse_id = frame_data$horse_id, opp_horse = frame_data$horse_id) %>%
    filter(horse_id != opp_horse) %>%
    left_join(frame_data %>% select(horse_id, x, y, lat_position = distance_to_inside, perp_marker = distance_marker, stage, max_marker, run_up_marker), by = "horse_id") %>%
    left_join(frame_data %>% select(opp_horse = horse_id, finishing_place, x2 = x, y2 = y, lat_position2 = distance_to_inside, perp_marker2 = distance_marker, stage2 = stage), by = "opp_horse") %>%
    mutate(
      euclidean = sqrt((x - x2)^2 + (y - y2)^2),
      lat_dist = lat_position2 - lat_position,
      fwd_dist = case_when(
        grepl("run up", stage) & grepl("run up", stage2) ~ perp_marker2 - perp_marker,
        grepl("run up", stage) & !grepl("run up", stage2) ~ perp_marker2 + (perp_marker - run_up_marker),
        perp_marker >= max_marker - 10 & perp_marker2 <= 10 ~ perp_marker + (max_marker - perp_marker2),
        TRUE ~ perp_marker - perp_marker2
      ),
      is_drafting = case_when(
        fwd_dist > 0 ~ FALSE,
        TRUE ~ lat_position >= lat_position2 - draft(-fwd_dist) & lat_position <= lat_position2 + draft(-fwd_dist)
      )
    ) %>%
    select(horse_id, finishing_place, opp_horse, euclidean, lat_dist, fwd_dist, is_drafting) %>%
    pivot_wider(id_cols = "horse_id", names_from = "finishing_place", values_from = c("opp_horse", "euclidean", "lat_dist", "fwd_dist", "is_drafting"))
  
  if (nrow(distances) == 0) {
    distances = data.frame(horse_id = frame_data$horse_id)
  }
  
  final_data = left_join(frame_data, distances, by = "horse_id")
  
  return(final_data)
  
}



#####################################################
## STEP 10: TRIM FRAMES FOR COMPUTATIONAL PURPOSES ##
#####################################################



############################################
## STEP 11: ADD IN EXTRA MODEL COVARIATES ##
############################################

# Calculate additional model covariates
race_covariates = function(df, horse_close) {
  
  N = nrow(df)
  
  # Extract column names for distances and drafting
  lat_cols = which(grepl("lat_dist", names(df)))
  fwd_cols = which(grepl("fwd_dist", names(df)))
  euclid_cols = which(grepl("euclidean_", names(df)))
  draft_cols = which(grepl("is_drafting_", names(df)))
  
  # Calculate data frame of metrics based on metrics
  out = do.call(rbind, lapply(c(1:N), function(i) {
    
    curr_row = df[i,]
    
    lat_dist = curr_row[lat_cols]
    fwd_dist = curr_row[fwd_cols]
    euclid_dist = curr_row[euclid_cols]
    draft_dist = curr_row[draft_cols]
    
    within_x_metres = which(abs(fwd_dist) < horse_close)
    horses_inside = which(lat_dist < 0)
    horses_outside = which(lat_dist > 0)
    
    horses_inside_close = intersect(horses_inside, within_x_metres)
    horses_outside_close = intersect(horses_outside, within_x_metres)
    
    n_horses_inside = length(horses_inside)
    n_horses_outside = length(horses_outside)
    n_horses_close_inside = length(horses_inside_close)
    n_horses_outside_close = length(horses_outside_close)
    
    nearest_inside = ifelse(n_horses_inside > 0, -1 * min(-lat_dist[horses_inside]), 0)
    nearest_inside_euclid = ifelse(n_horses_inside > 0, min(euclid_dist[horses_inside]), 0)
    nearest_outside = ifelse(n_horses_outside > 0, min(lat_dist[horses_outside]), 0)
    nearest_outside_euclid = ifelse(n_horses_outside > 0, min(euclid_dist[horses_outside]), 0)
    
    cov_df <- data.frame(
      n_horses_inside = n_horses_inside,
      n_horses_outside = n_horses_outside,
      n_horses_close_inside = n_horses_close_inside,
      n_horses_close_outside = n_horses_outside_close,
      nearest_inside = nearest_inside,
      nearest_inside_euclid = nearest_inside_euclid,
      nearest_outside = nearest_outside,
      nearest_outside_euclid = nearest_outside_euclid
    )
    
  }))
  
  return(out)
  
}




##########################################
## STEP 12: REARRANGE AND SAVE THE DATA ##
##########################################


