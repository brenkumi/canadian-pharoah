
// CREATE FUNCTIONS TO SIMULATE RACES

functions {
  
  // BUILD FUNCTIONS TO UPDATE COVARIATES
  
  // Update forward distance travelled over previous frame
  vector update_distance_prev(vector fwd_loc, vector fwd_loc_lag, int n_race_horses) {
    
    vector[n_race_horses] distance_prev;
    
    distance_prev = fwd_loc - fwd_loc_lag;
    
    return distance_prev;
    
  }
  
  
  // // Update lateral distance travelled over previous frame
  // vector update_y_lag(vector lat_loc, vector lat_loc_lag, int n_race_horses) {
  //   
  //   vector[n_race_horses] y_lag;
  //   
  //   y_lag = lat_loc - lat_loc_lag;
  //   
  //   return y_lag;
  //   
  // }
  
  
  // Update lateral distance travelled over previous frame
  vector update_inside_movement(vector lat_loc_lag, vector lat_loc_lag2, int n_race_horses) {
    
    vector[n_race_horses] inside_movement;
    
    inside_movement = lat_loc_lag - lat_loc_lag2;
    
    return inside_movement;
    
  }
  
  
  // Update the number of horses in front covariate for each horse
  vector update_n_horses_fwd(vector fwd_loc, int n_race_horses) {
    
    // Initialize variables for number of horses in front and whether a horse is in front
    vector[n_race_horses] n_horses_fwd;
    vector[n_race_horses] is_forward;
    
    // Double for loop to look at each subject horse (i) and all horses relative to him (j)
    for (i in 1:n_race_horses) {
      for (j in 1:n_race_horses) {
        
        // Determine if the horse is in front
        if (fwd_loc[j] > fwd_loc[i]) {
          is_forward[j] = 1.0;
        } else {
          is_forward[j] = 0.0;
        }
        
      }
      
      // Tally up horses in front
      n_horses_fwd[i] = sum(is_forward);
      
    }
    
    return n_horses_fwd;
    
  }
  
  
  // Update the number of horses behind covariate for each horse
  vector update_n_horses_bwd(vector fwd_loc, int n_race_horses) {
    
    vector[n_race_horses] n_horses_bwd;
    vector[n_race_horses] is_backward;
    
    for (i in 1:n_race_horses) {
      for (j in 1:n_race_horses) {
        
        if (fwd_loc[j] < fwd_loc[i]) {
          is_backward[j] = 1.0;
        } else {
          is_backward[j] = 0.0;
        }
        
      }
      
      n_horses_bwd[i] = sum(is_backward);
      
    }
    
    return n_horses_bwd;
    
  }
  
  
  // Update the number of horses inside covariate for each horse
  vector update_n_horses_inside(vector lat_loc, int n_race_horses) {
    
    vector[n_race_horses] n_horses_inside;
    vector[n_race_horses] is_inside;
    
    for (i in 1:n_race_horses) {
      for (j in 1:n_race_horses) {
        
        if (lat_loc[j] < lat_loc[i]) {
          is_inside[j] = 1.0;
        } else {
          is_inside[j] = 0.0;
        }
        
      }
      
      n_horses_inside[i] = sum(is_inside);
      
    }
    
    return n_horses_inside;
    
  }
  
  
  // Update the number of horses outside covariate for each horse
  vector update_n_horses_outside(vector lat_loc, int n_race_horses) {
    
    vector[n_race_horses] n_horses_outside;
    vector[n_race_horses] is_outside;
    
    for (i in 1:n_race_horses) {
      for (j in 1:n_race_horses) {
        
        if (lat_loc[j] > lat_loc[i]) {
          is_outside[j] = 1.0;
        } else {
          is_outside[j] = 0.0;
        }
        
      }
      
      n_horses_outside[i] = sum(is_outside);
      
    }
    
    return n_horses_outside;
    
  }
  
  
  // Update the lateral distance to nearest inside horse
  vector update_nearest_inside(vector lat_loc, int n_race_horses, vector n_horses_inside) {
    
    vector[n_race_horses] nearest_inside;
    vector[n_race_horses] lat_dist;
    
    for (i in 1:n_race_horses) {
      for (j in 1:n_race_horses) {
        
        lat_dist[j] = lat_loc[i] - lat_loc[j];
        
        if (lat_dist[j] <= 0) {
          lat_dist[j] = 10000;
        }
        
      }
      
      nearest_inside[i] = min(lat_dist);
      
      if (n_horses_inside[i] == 0) {
        nearest_inside[i] = 0;
      }
      
    }
    
    return nearest_inside;
    
  }
  
  
  // Update the lateral distance to nearest inside horse
  vector update_nearest_outside(vector lat_loc, int n_race_horses, vector n_horses_outside) {
    
    vector[n_race_horses] nearest_outside;
    vector[n_race_horses] lat_dist;
    
    for (i in 1:n_race_horses) {
      for (j in 1:n_race_horses) {
        
        lat_dist[j] = lat_loc[j] - lat_loc[i];
        
        if (lat_dist[j] <= 0) {
          lat_dist[j] = 10000;
        }
        
      }
      
      nearest_outside[i] = min(lat_dist);
      
      if (n_horses_outside[i] == 0) {
        nearest_outside[i] = 0;
      }
      
    }
    
    return nearest_outside;
    
  }
  
  
  // Update the forward distance to the nearest horse (forward or backwards)
  vector update_nearest_fwd(vector fwd_loc, int n_race_horses) {
    
    vector[n_race_horses] nearest_fwd;
    vector[n_race_horses] fwd_dist;
    
    for (i in 1:n_race_horses) {
      for (j in 1:n_race_horses) {
        
        if (i == j) {
          fwd_dist[j] = 10000;
        } else {
          fwd_dist[j] = fabs(fwd_loc[j] - fwd_loc[i]);
        }
        
      }
      nearest_fwd[i] = min(fwd_dist);
    }
    
    return nearest_fwd;
    
  }
  
  
  // Update the euclidean distance of the nearest inside horse
  vector update_nearest_inside_euclid(vector fwd_loc, vector lat_loc, int n_race_horses, vector n_horses_inside) {
    
    vector[n_race_horses] nearest_inside_euclid;
    vector[n_race_horses] lat_dist;
    vector[n_race_horses] euclid_dist;
    
    for (i in 1:n_race_horses) {
      for (j in 1:n_race_horses) {
        
        lat_dist[j] = lat_loc[i] - lat_loc[j];
        
        if (lat_dist[j] <= 0) {
          euclid_dist[j] = 10000;
        } else {
          euclid_dist[j] = sqrt((lat_loc[i] - lat_loc[j])^2 + (fwd_loc[i] - fwd_loc[j])^2);
        }
        
      }
      
      nearest_inside_euclid[i] = min(euclid_dist);
      
      if (n_horses_inside[i] == 0) {
        nearest_inside_euclid[i] = 0;
      }
      
    }
    
    return nearest_inside_euclid;
    
  }
  
  
  // Update the euclidean distance of the nearest outside horse
  vector update_nearest_outside_euclid(vector fwd_loc, vector lat_loc, int n_race_horses, vector n_horses_outside) {
    
    vector[n_race_horses] nearest_outside_euclid;
    vector[n_race_horses] lat_dist;
    vector[n_race_horses] euclid_dist;
    
    for (i in 1:n_race_horses) {
      for (j in 1:n_race_horses) {
        
        lat_dist[j] = lat_loc[j] - lat_loc[i];
        
        if (lat_dist[j] <= 0) {
          euclid_dist[j] = 10000;
        } else {
          euclid_dist[j] = sqrt((lat_loc[i] - lat_loc[j])^2 + (fwd_loc[i] - fwd_loc[j])^2);
        }
        
      }
      
      nearest_outside_euclid[i] = min(euclid_dist);
      
      if (n_horses_outside[i] == 0) {
        nearest_outside_euclid[i] = 0;
      }
      
    }
    
    return nearest_outside_euclid;
    
  }
  
  
  // Convert real-valued x to an integer
  int bin_search(real x, int min_val, int max_val) {
    
    // This assumes that min_val >= 0 is the minimum integer in range, max_val > min_val, and that x has
    // already been rounded. It should find the integer that is equivalent to x.
    
    int range = (max_val - min_val + 1) / 2; // We add 1 to make sure that truncation doesn't exclude a number
    int mid_pt = min_val + range;
    int out;
    real x_star = round(x);
    
    if (x_star > max_val * 1.0) {
      x_star = max_val;
    }
    
    if (x_star < min_val * 1.0) {
      x_star = min_val;
    }
    
    while (range > 0) {
      if (x_star == mid_pt) {
        out = mid_pt;
        range = 0;
      } else {
        // Figure out if range == 1
        range = (range + 1) / 2;
        mid_pt = x_star > mid_pt ? mid_pt + range: mid_pt - range;
      }
    
    }
    return out;
  }
  
  
  // Determine if a horse is on the home stretch or not
  int[] update_race_stage(vector fwd_loc, int n_race_horses, int[] stage, int n_markers, real run_up_distance, real granularity) {
    
    int fwd_loc_index[n_race_horses];
    int is_in_stage[n_race_horses];
    
    for (i in 1:n_race_horses) {
      fwd_loc_index[i] = bin_search((fwd_loc[i] + run_up_distance)/granularity + 1, 1, n_markers);
      is_in_stage[i] = stage[fwd_loc_index[i]];
    }
    
    return is_in_stage;
    
  }
  
  
  // Determine if a horse is drafting or not
  vector update_drafting_int(vector fwd_loc, vector lat_loc, int n_race_horses, real horse_width, real max_distance_behind) {
    
    vector[n_race_horses] is_drafting;
    vector[n_race_horses] dist_behind;
    vector[n_race_horses] dist_wide;
    real draft_width;
    
    for (i in 1:n_race_horses) {
      
      is_drafting[i] = 0;
      
      for (j in 1:n_race_horses) {
        
        if (i != j) {
          dist_behind[j] = fwd_loc[j] - fwd_loc[i];
          dist_wide[j] = lat_loc[j] - lat_loc[i];
          
          if ((dist_behind[j] > 0) && (dist_behind[j] < max_distance_behind)) {
            draft_width = (horse_width / 2) * log((max_distance_behind + 1) - dist_behind[j]) / log(max_distance_behind + 1);
          } else {
            draft_width = 0;
          }
          
          if (fabs(dist_wide[j]) <= draft_width) {
            is_drafting[i] = 1;
          }
        }
      }
    }
    
    return is_drafting;
    
  }
  
  
  // Determine the energy spent by a horse in the current frame
  vector update_cumulative_energy(vector fwd_loc, vector lat_loc, vector fwd_loc_lag, vector drafting_int, vector c_energy_lag, int n_race_horses) {
    
    vector[n_race_horses] drag;
    vector[n_race_horses] force;
    vector[n_race_horses] energy;
    vector[n_race_horses] c_energy;
    
    for (i in 1:n_race_horses) {
      
      if (drafting_int[i] > 0.5) {
        
        // Simplified for simulation
        drag[i] = 0.382;
        
      } else {
        
        drag[i] = 0.429;
        
      }
      
      force[i] = 0.5 * ((fwd_loc[i] - fwd_loc_lag[i]) * 4.0)^2 * 0.81956 * drag[i];
      
      energy[i] = force[i] * (fwd_loc[i] - fwd_loc_lag[i]);
      
      c_energy[i] = energy[i] + c_energy_lag[i];
      
    }
    
    return c_energy;
    
  }
  
  
  // Determine the energy spent by a horse in the current frame
  vector update_cumulative_max_energy(vector fwd_loc, vector lat_loc, vector fwd_loc_lag, vector drafting_int, vector c_energy_lag, int n_race_horses) {
    
    vector[n_race_horses] drag;
    vector[n_race_horses] force;
    vector[n_race_horses] energy;
    vector[n_race_horses] c_energy;
    
    for (i in 1:n_race_horses) {
      
      drag[i] = 0.429;
      
      force[i] = 0.5 * ((fwd_loc[i] - fwd_loc_lag[i]) * 4.0)^2 * 0.81956 * drag[i];
      
      energy[i] = force[i] * (fwd_loc[i] - fwd_loc_lag[i]);
      
      c_energy[i] = energy[i] + c_energy_lag[i];
      
    }
    
    return c_energy;
    
  }
  
  
  // Determine the proportion of energy saved by drafting, interacted with home_stretch
  vector update_prop_energy_saved_x_hs(vector c_energy, vector c_max_energy, int[] is_home_stretch, int n_race_horses) {
    
    vector[n_race_horses] prop_energy_saved;
    vector[n_race_horses] prop_energy_saved_x_hs;
    
    for (i in 1:n_race_horses) {
      
      prop_energy_saved[i] = (c_max_energy[i] - c_energy[i]) / c_max_energy[i];
      
      prop_energy_saved_x_hs[i] = prop_energy_saved[i] * is_home_stretch[i];
    }
    
    return prop_energy_saved_x_hs;
    
  }
  
  
  
  // BUILD FUNCTIONS TO STITCH TOGETHER ALL COVARIATES
  
  matrix update_covariates(vector fwd_loc, vector lat_loc, int n_race_horses, int n_lat_cov,
                           vector fwd_loc_lag, vector lat_loc_lag, vector lat_loc_lag2,
                           int[] home_stretch, int[] turn2hs, int[] bend,
                           int n_markers, real run_up_distance, real granularity,
                           vector c_energy_lag, vector c_max_energy_lag) {
    
    // Create matrix to store covariates
    matrix[n_race_horses, n_lat_cov + 2] covariates;
    
    // Calculate lag distance covariates
    vector[n_race_horses] distance_prev = update_distance_prev(fwd_loc, fwd_loc_lag, n_race_horses);
    //vector[n_race_horses] y_lag = update_y_lag(lat_loc, lat_loc_lag, n_race_horses);
    vector[n_race_horses] inside_movement = update_inside_movement(lat_loc_lag, lat_loc_lag2, n_race_horses);
    
    // Calculate number of horse covariates
    vector[n_race_horses] n_horses_fwd = update_n_horses_fwd(fwd_loc, n_race_horses);
    vector[n_race_horses] n_horses_bwd = update_n_horses_bwd(fwd_loc, n_race_horses);
    vector[n_race_horses] n_horses_inside = update_n_horses_inside(lat_loc, n_race_horses);
    vector[n_race_horses] n_horses_outside = update_n_horses_outside(lat_loc, n_race_horses);
    
    // Calculate nearest horse covariates
    vector[n_race_horses] nearest_fwd = update_nearest_fwd(fwd_loc, n_race_horses);
    vector[n_race_horses] nearest_inside = update_nearest_inside(lat_loc, n_race_horses, n_horses_inside);
    vector[n_race_horses] nearest_outside = update_nearest_outside(lat_loc, n_race_horses, n_horses_outside);
    vector[n_race_horses] nearest_inside_euclid = update_nearest_inside_euclid(fwd_loc, lat_loc, n_race_horses, n_horses_inside);
    vector[n_race_horses] nearest_outside_euclid = update_nearest_outside_euclid(fwd_loc, lat_loc, n_race_horses, n_horses_outside);
    
    // Calculate race stage covariates
    int is_home_stretch[n_race_horses] = update_race_stage(fwd_loc, n_race_horses, home_stretch, n_markers, run_up_distance, granularity);
    int is_turn2hs[n_race_horses] = update_race_stage(fwd_loc, n_race_horses, turn2hs, n_markers, run_up_distance, granularity);
    int is_bend[n_race_horses] = update_race_stage(fwd_loc, n_race_horses, bend, n_markers, run_up_distance, granularity);
    
    // Calculate drafting covariates
    vector[n_race_horses] drafting_int = update_drafting_int(fwd_loc, lat_loc, n_race_horses, 1.2, 7);
    vector[n_race_horses] cumulative_energy = update_cumulative_energy(fwd_loc, lat_loc, fwd_loc_lag, drafting_int, c_energy_lag, n_race_horses);
    vector[n_race_horses] cumulative_max_energy = update_cumulative_max_energy(fwd_loc, lat_loc, fwd_loc_lag, drafting_int, c_max_energy_lag, n_race_horses);
    vector[n_race_horses] prop_energy_saved_x_hs = update_prop_energy_saved_x_hs(cumulative_energy, cumulative_max_energy, is_home_stretch, n_race_horses);
    
    // Stitch together covariate matrix (+ energy variables)
    covariates[,1] = inside_movement;
    covariates[,2] = distance_prev;
    covariates[,3] = n_horses_inside;
    covariates[,4] = n_horses_outside;
    covariates[,5] = n_horses_fwd;
    covariates[,6] = n_horses_bwd;
    covariates[,7] = nearest_inside;
    covariates[,8] = nearest_outside;
    covariates[,9] = nearest_inside_euclid;
    covariates[,10] = nearest_outside_euclid;
    covariates[,11] = nearest_fwd;
    covariates[,12] = prop_energy_saved_x_hs;
    covariates[,13] = drafting_int;
    covariates[,14] = to_vector(is_bend);
    covariates[,15] = to_vector(is_home_stretch);
    covariates[,16] = to_vector(is_turn2hs);
    covariates[,17] = cumulative_energy;
    covariates[,18] = cumulative_max_energy;
    //covariates[,19] = y_lag;
    
    return covariates;
    
  }
  
  
  // BUILD FUNCTION TO EXTRACT AND PREP SPLINE BASIS FUNCTIONS FOR MODELLING
  
  matrix update_basis_functions(vector fwd_loc, int n_race_horses, matrix spline_matrix, int n_splines, int n_markers, real run_up_distance, real granularity) {
    
    int fwd_loc_index[n_race_horses];
    matrix[n_race_horses, n_splines] horse_splines;
    
    matrix[n_splines, n_markers] spline_transposed;
    spline_transposed = spline_matrix';
    
    for (i in 1:n_race_horses) {
      fwd_loc_index[i] = bin_search((fwd_loc[i] + run_up_distance)/granularity + 1, 1, n_markers);
      horse_splines[i,] = to_row_vector(spline_transposed[,fwd_loc_index[i]]);
    }
    
    return horse_splines;
    
  }
  
  
  // BUILD FUNCTION TO SIMULATE RACE
  
  matrix simulate_race_rng(matrix fwd_beta, vector fwd_gamma, vector fwd_theta_track, vector fwd_theta_jockey, real fwd_sigma,
                           real lat_beta_lag, vector lat_beta, vector lat_theta_track, vector lat_theta_jockey, real lat_sigma,
                           int[] horse_id, int[] jockey_id_fwd, int[] jockey_id_lat, int track_id,
                           int start_frame, real race_length, int n_race_horses, vector initial_fwd_loc, vector initial_lat_loc,
                           vector initial_fwd_loc_lag, vector initial_lat_loc_lag, vector initial_lat_loc_lag2,
                           vector initial_cum_energy, vector initial_cum_max_energy, matrix initial_lat_covariates,
                           matrix spline_matrix, real run_up_distance, real granularity,
                           int n_fwd_cov, int n_lat_cov, int n_splines, int n_markers,
                           int[] home_stretch, int[] turn2hs, int[] bend) {
    
    // This matrix is used to store the final frame-by-frame distances and energies that we output from the simulations
    matrix[450 - start_frame, n_race_horses + n_race_horses + n_race_horses + n_race_horses] combined_distances_and_energies;
    
    // These matrices store the (fowrad and lateral) distances and cumulative (actual and real) energies
    // Note: We cbind these four to get combined_distances_and_energies defined above
    matrix[450 - start_frame, n_race_horses] fwd_distances;
    matrix[450 - start_frame, n_race_horses] lat_distances;
    matrix[450 - start_frame, n_race_horses] cumulative_energies;
    matrix[450 - start_frame, n_race_horses] cumulative_max_energies;
    
    // These vectors will be used to temporarily store frame-by-frame information for simulations
    vector[n_race_horses] fwd_linear_predictor; // Store linear predictors for forward movement model at an individual frame
    vector[n_race_horses] lat_linear_predictor; // Store linear predictors for lateral movement model at an individual frame
    vector[n_race_horses] fwd_frame_movement; // Store simulated output for forward movement model at an individual frame
    vector[n_race_horses] fwd_frame_position; // Store simulated forward position at an individual frame
    vector[n_race_horses] lat_frame_position; // Store simulated lateral position at an individual frame
    vector[n_race_horses] cum_energy_frame; // Store simulated cumulative energies at current frame
    vector[n_race_horses] cum_max_energy_frame; // Store simulated cumulative max energies at current frame
    vector[n_race_horses] fwd_lag_position = initial_fwd_loc_lag; // Store lag forward position for horse
    vector[n_race_horses] lat_lag_position = initial_lat_loc_lag; // Store lag lateral position for horse
    vector[n_race_horses] lat_lag2_position = initial_lat_loc_lag2; // Store double lag lateral position for horse
    vector[n_race_horses] cum_energy_lag = initial_cum_energy; // Store lag cumulative energy used for horse
    vector[n_race_horses] cum_max_energy_lag = initial_cum_max_energy; // Store lag cumulative energy used for horse
    // Note: No need to store lat_frame_movement since the lateral movement model outputs location relative to inside of track
    
    // These matrices will be used to store model covariates and spline basis functions
    matrix[n_race_horses, n_fwd_cov] fwd_frame_covariates = initial_lat_covariates[,1:n_fwd_cov];
    matrix[n_race_horses, n_lat_cov] lat_frame_covariates = initial_lat_covariates;
    matrix[n_race_horses, n_splines] horse_splines = update_basis_functions(initial_fwd_loc, n_race_horses, spline_matrix, n_splines, n_markers, run_up_distance, granularity);
    matrix[n_race_horses, n_lat_cov + 2] covariates_and_energies;
    vector[n_race_horses] y_lag = initial_lat_loc - initial_lat_loc_lag;
    
    // Initialize current frame, distances and energies
    int curr_frame = start_frame;
    fwd_frame_position = initial_fwd_loc;
    lat_frame_position = initial_lat_loc;
    fwd_distances[1,] = to_row_vector(initial_fwd_loc);
    lat_distances[1,] = to_row_vector(initial_lat_loc);
    cum_energy_frame = initial_cum_energy;
    cum_max_energy_frame = initial_cum_max_energy;
    cumulative_energies[1,] = to_row_vector(initial_cum_energy);
    cumulative_max_energies[1,] = to_row_vector(initial_cum_max_energy);
    
    // Run while loop until max frames (450) reached or race is finished
    while ((min(fwd_frame_position) < race_length) && (curr_frame - start_frame + 1 < 450 - start_frame)) {
      
      // Update frame
      curr_frame = curr_frame + 1;
      
      // Store current movement
      lat_lag2_position = lat_lag_position;
      fwd_lag_position = fwd_frame_position;
      lat_lag_position = lat_frame_position;
      cum_energy_lag = cum_energy_frame;
      cum_max_energy_lag = cum_max_energy_frame;
      
      // Simulate forward and lateral movement
      for (i in 1:n_race_horses) {
        
        // Update linear predictors
        fwd_linear_predictor[i] = fwd_theta_track[track_id] + fwd_theta_jockey[jockey_id_fwd[i]] + dot_product(fwd_gamma, fwd_frame_covariates[i,]) + dot_product(to_vector(fwd_beta[,horse_id[i]]), to_row_vector(horse_splines[i,]));
        lat_linear_predictor[i] = lat_theta_track[track_id] + lat_theta_jockey[jockey_id_lat[i]] + lat_lag_position[i] * lat_beta_lag + dot_product(lat_beta, lat_frame_covariates[i,]);
        //fwd_linear_predictor[i] = 4.0;
        //lat_linear_predictor[i] = 15.0;
        
        // Simulate forward and lateral movement
        fwd_frame_movement[i] = normal_rng(fwd_linear_predictor[i], fwd_sigma);
        fwd_frame_position[i] = fwd_lag_position[i] + fwd_frame_movement[i];
        lat_frame_position[i] = normal_rng(lat_linear_predictor[i], lat_sigma);
        
        if (fwd_frame_position[i] > 1609.4) {
          lat_frame_position[i] = lat_lag_position[i];
          fwd_frame_position[i] = 1609.4;
        }
        
        if (lat_frame_position[i] <= 0) {
          lat_frame_position[i] = lat_lag_position[i] + uniform_rng(0, 0.5);
        }
        
      }
      
      // Update covariates and energies
      covariates_and_energies = update_covariates(fwd_frame_position, lat_frame_position, n_race_horses, n_lat_cov,
                                                  fwd_lag_position, lat_lag_position, lat_lag2_position,
                                                  home_stretch, turn2hs, bend, n_markers, run_up_distance, granularity,
                                                  cum_energy_lag, cum_max_energy_lag);
      
      fwd_frame_covariates = covariates_and_energies[,1:n_fwd_cov];
      lat_frame_covariates = covariates_and_energies[,1:n_lat_cov];
      cum_energy_frame = covariates_and_energies[,n_lat_cov+1];
      cum_max_energy_frame = covariates_and_energies[,n_lat_cov+2];
      
      // Update spline basis functions
      horse_splines = update_basis_functions(fwd_frame_position, n_race_horses, spline_matrix, n_splines, n_markers, run_up_distance, granularity);
      
      // Store distances and energies
      fwd_distances[curr_frame - start_frame + 1,] = to_row_vector(fwd_frame_position);
      lat_distances[curr_frame - start_frame + 1,] = to_row_vector(lat_frame_position);
      cumulative_energies[curr_frame - start_frame + 1,] = to_row_vector(cum_energy_frame);
      cumulative_max_energies[curr_frame - start_frame + 1,] = to_row_vector(cum_max_energy_frame);
      
    }
    
    combined_distances_and_energies = append_col(append_col(append_col(fwd_distances, lat_distances), cumulative_energies), cumulative_max_energies);
    
    return combined_distances_and_energies;
    
  }
  
}


// SET INPUT DATA FOR RACE SIMULATOR

data {
  
  // Basic simulation and data information
  int<lower=1> n_race_horses; // Number of horses in the simulated race
  int<lower=1> n_horses; // Number of horses in model
  int<lower=1> n_jockeys_fwd; // Number of jockeys in forward model
  int<lower=1> n_jockeys_lat; // Number of jockeys in forward model
  int<lower=1> n_tracks; // Number of track type/condition combinations in model
  int<lower=1> n_splines; // Number of basis functions in horse splines
  int<lower=1> n_markers; // Number of track markers from start of the race to finish (there is a track marker every 10cm on the track)
  int<lower=1> n_fwd_cov; // Number of covariates in forward model
  int<lower=1> n_lat_cov; // Number of covariates in lateral model
  int<lower=1> start_frame; // frame_id at start of simulation
  real<lower=0> race_length; // Number of metres in the race
  real run_up_distance; // The run-up distance for the race
  
  // Horse, jockey and track IDs
  int<lower=1> horse_id_fwd[n_race_horses]; // The horse IDs for the forward movement model
  // int<lower=1> horse_id_lat[n_race_horses]; // The horse IDs for the lateral movement model (don't need)
  
  int<lower=1> jockey_id_fwd[n_race_horses]; // The jockey IDs for the forward movement model
  int<lower=1> jockey_id_lat[n_race_horses]; // The jockey IDs for the lateral movement model
  
  int<lower=1> track_id; // The ID for the track type/condition combination
  
  // Starting locations
  vector[n_race_horses] initial_fwd_loc; // The starting forward locations for each horse
  vector[n_race_horses] initial_lat_loc; // The starting lateral locations for each horse
  vector[n_race_horses] initial_fwd_loc_lag; // The starting forward locations for each horse - lagged by 1 frame
  vector[n_race_horses] initial_lat_loc_lag; // The starting lateral locations for each horse - lagged by 1 frame
  vector[n_race_horses] initial_fwd_loc_lag2; // The starting forward locations for each horse - lagged by 2 frame
  vector[n_race_horses] initial_lat_loc_lag2; // The starting lateral locations for each horse - lagged by 2 frame
  
  // Starting covariates
  matrix[n_markers, n_splines] spline_matrix; // Spline matrix
  matrix[n_race_horses, n_fwd_cov] initial_fwd_cov; // Initial forward covariates
  matrix[n_race_horses, n_lat_cov] initial_lat_cov; // Initial forward covariates
  vector[n_race_horses] cum_energy; // Initial cumulative energy spent by horses
  vector[n_race_horses] cum_max_energy; // Initial max cumulative energy spent by horses (without drafting)
  int bend[n_markers]; // Indicator for bends/turns on track
  int turn2hs[n_markers]; // Indicator for the transition from turn to home stretch
  int home_stretch[n_markers]; // Indicator for home stretch
  real granularity;
  
}


// SET MODEL PARAMETERS

parameters {
  
  // Forward model parameters
  matrix[n_splines, n_horses] fwd_beta; // Spline parameters
  real<lower=0> fwd_sigma; // Variance parameter
  vector[n_fwd_cov] fwd_gamma; // Covariate parameters
  vector[n_tracks] fwd_theta_track; // Random effect for track surface + condition
  vector[n_jockeys_fwd] fwd_theta_jockey;
  
  // Lateral model parameters
  real lat_beta_lag; // Parameter for previous side movement
  vector[n_lat_cov] lat_beta; // Parameters for covariates
  vector[n_tracks] lat_theta_track; // Random effect for track surface + condition
  vector[n_jockeys_lat] lat_theta_jockey; // Random effect for jockey
  real<lower=0> lat_sigma; // Parameter for sigma
  
}


// USE POSTERIOR DRAWS TO SIMULATE RACES

generated quantities {
  
  // Initialize variable to store simulated draft results
  matrix[450 - start_frame, n_race_horses + n_race_horses + n_race_horses + n_race_horses] race_sim;

  race_sim = simulate_race_rng(fwd_beta, fwd_gamma, fwd_theta_track, fwd_theta_jockey, fwd_sigma,
                               lat_beta_lag, lat_beta, lat_theta_track, lat_theta_jockey, lat_sigma,
                               horse_id_fwd, jockey_id_fwd, jockey_id_lat, track_id,
                               start_frame, race_length, n_race_horses, initial_fwd_loc, initial_lat_loc,
                               initial_fwd_loc_lag, initial_lat_loc_lag, initial_lat_loc_lag2,
                               cum_energy, cum_max_energy, initial_lat_cov,
                               spline_matrix, run_up_distance, granularity,
                               n_fwd_cov, n_lat_cov, n_splines, n_markers, home_stretch, turn2hs, bend);
                               
}






