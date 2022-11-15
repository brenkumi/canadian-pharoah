
## LOAD TRACK CONNECTOR DATA ##

track_connector = read_csv("Data/Prepped Input/track_connector.csv")



## FUNCTIONS TO CONVERT COORDINATES BETWEEN RACE COORDS, TRACK COORDS AND CARTESIAN COORDS ##

race2cart = function(track,course,fwd,lat) {
  
  locs = data.frame(track_id = track, course_type = course, race_loc = fwd) %>%
    left_join(track_connector, by = c("track_id", "course_type", "race_loc"))
  
  x_inner = locs$x_inner; x_outer = locs$x_outer
  y_inner = locs$y_inner; y_outer = locs$y_outer
  
  prop = lat / sqrt((x_inner - x_outer)^2 + (y_inner - y_outer)^2)
  
  x_out = x_inner + (x_outer - x_inner) * prop
  y_out = y_inner + (y_outer - y_inner) * prop
  
  return(data.frame(x = x_out, y = y_out))
  
}



track2cart = function(track,course,fwd,lat) {
  
  locs = data.frame(track_id = track, course_type = course, track_loc = fwd) %>%
    left_join(track_connector, by = c("track_id", "course_type", "track_loc"))
  
  x_inner = locs$x_inner; x_outer = locs$x_outer
  y_inner = locs$y_inner; y_outer = locs$y_outer
  
  prop = lat / sqrt((x_inner - x_outer)^2 + (y_inner - y_outer)^2)
  
  x_out = x_inner + (x_outer - x_inner) * prop
  y_out = y_inner + (y_outer - y_inner) * prop
  
  return(data.frame(x = x_out, y = y_out))
  
}



cart2track = function(track,course,x,y) {
  
  locs = data.frame(x = x, y = y) %>%
    mutate(tracks = map(x, ~track_connector)) %>%
    unnest() %>%
    filter(track_id == track & course_type == course) %>%
    mutate(euclid = sqrt((x_inner - x)^2 + (y_inner - y)^2)) %>%
    group_by(x, y) %>%
    slice(which.min(euclid)) %>%
    ungroup()
  
  fwd_out = locs$track_loc
  lat_out = locs$euclid
  
  return(data.frame(track_loc = fwd_out, lat_dist = lat_out))
  
}



cart2race = function(track,course,x,y,frame) {
  
  locs = data.frame(x = x, y = y) %>%
    mutate(tracks = map(x, ~track_connector)) %>%
    unnest() %>%
    filter(track_id == track & course_type == course) %>%
    mutate(euclid = sqrt((x_inner - x)^2 + (y_inner - y)^2)) %>%
    group_by(x, y) %>%
    filter(euclid == min(euclid)) %>%
    ungroup()
  
  if (frame > 100) {
    locs = locs %>%
      group_by(track_loc) %>%
      slice(which.max(race_loc)) %>%
      ungroup()
  } else {
    locs = locs %>%
      group_by(track_loc) %>%
      slice(which.min(race_loc)) %>%
      ungroup()
  }
  
  fwd_out = locs$race_loc
  lat_out = locs$euclid
  
  return(data.frame(race_loc = fwd_out, lat_dist = lat_out))
  
}



race2track = function(track,course,fwd) {
  
  locs = data.frame(track_id = track, course_type = course, race_loc = fwd) %>%
    left_join(track_connector, by = c("track_id", "course_type", "race_loc"))
  
  fwd_out = locs$track_loc
  
  return(fwd_out)
  
}



track2race = function(track,course,fwd,frame) {
  
  locs = data.frame(track_id = track, course_type = course, track_loc = fwd) %>%
    left_join(track_connector, by = c("track_id", "course_type", "track_loc"))
  
  if (frame > 100) {
    locs = locs %>%
      group_by(track_loc) %>%
      slice(which.max(race_loc)) %>%
      ungroup()
  } else {
    locs = locs %>%
      group_by(track_loc) %>%
      slice(which.min(race_loc)) %>%
      ungroup()
  }
  
  fwd_out = locs$race_loc
  
  return(fwd_out)
  
}



## SET STARTING POSITIONS ##

initialize_race = function(track, course, run_up, horses) {
  
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
  
  return(starting_positions)
  
}






