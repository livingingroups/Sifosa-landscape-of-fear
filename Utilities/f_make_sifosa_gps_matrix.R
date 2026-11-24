#' Function: f_make_sifosa_gps_matrix.R (ARI'S MATRIX)
#'
#' Organizes GPS tracking data into structured matrices (ARI'S MATRIX, AM)
#' 
#' @author Ariana Strandburg-Peshkin (original code from cocomo package)
#' @author Maevatiana N. Ratsimbazafindranahaka (code adaptation)
#' @author Tracy M. Montgomery (code reviewer)
#' @param data A data frame containing the following columns:
#'   - `individual_local_identifier`: Unique ID for each individual
#'   - `study_local_timestamp`: Timestamps of location data
#'   - `location_long`: Longitude
#'   - `location_lat`: Latitude
#'   - `utm_easting`: Easting coordinate in UTM
#'   - `utm_northing`: Northing coordinate in UTM
#'   - `height_above_ellipsoid`: Height above ellipsoid in m
#' @param time_zone Site timezone. Default UTM
#' @return A list containing:
#'   - `xs`: UTM easting coordinates matrix
#'   - `ys`: UTM northing coordinates matrix
#'   - `zs`: height (above ellipsoid) matrix
#'   - `lats`: Latitude coordinates matrix
#'   - `lons`: Longitude coordinates matrix
#'   - `timestamps`: Vector of unique timestamps
#'   - `ids`: Data frame mapping individual IDs (`id_code`) to indices (`idx`) and species
#'   - `dates`: Unique dates extracted from timestamps
#'   - `day_start_idxs`: Indices marking the start of each unique day

library(dplyr)
library(tibble)

f_make_sifosa_gps_matrix <- function(data, time_zone = "UTC") {
  
  # Extract unique combinations of individuals and species
  unique_combinations <- distinct(data, individual_local_identifier, species)
  
  # Create an index mapping for each unique individual
  ids <- data.frame(
    id_code = unique_combinations$individual_local_identifier,
    idx = seq_along(unique(data$individual_local_identifier)),
    species = unique_combinations$species
  )
  
  # Sort timestamps in chronological order
  timestamps <- sort(unique(data$study_local_timestamp))
  
  # Initialize matrices for spatial data (lat/lon and UTM coordinates)
  n <- nrow(ids)
  num_timestamps <- length(timestamps)
  lats <- lons <- xs <- ys <- zs <- matrix(NA, nrow = n, ncol = num_timestamps)
  
  # Iterate over each individual to fill matrices
  for (i in seq_len(n)) {
    
    curr_animal <- ids$id_code[i]  # Retrieve the individual's ID
    curr_dat <- filter(data, individual_local_identifier == curr_animal)  # Get of this individual
    
    # Identify missing timestamps for the individual (considering all possible timestamps of all individuals combined)
    missing_timestamps <- setdiff(timestamps, curr_dat$study_local_timestamp)
    missing_timestamps <- as.POSIXct(missing_timestamps, origin = "1970-01-01", tz = time_zone)
    
    # Create a placeholder data frame for missing timestamps
    extra_time <- tibble(
      study_local_timestamp = missing_timestamps,
      individual_local_identifier = curr_animal,
      location_long = NA_real_,
      location_lat = NA_real_,
      utm_easting = NA_real_,
      utm_northing = NA_real_,
      height_above_ellipsoid = NA_real_
    )
    
    # Merge original data for this individual with missing timestamps and sort by time
    curr_dat <- bind_rows(curr_dat, extra_time) %>%
      arrange(study_local_timestamp)
    
    # Store content in the matrices
    lats[i, ] <- curr_dat$location_lat
    lons[i, ] <- curr_dat$location_long
    xs[i, ] <- curr_dat$utm_easting
    ys[i, ] <- curr_dat$utm_northing
    zs[i, ] <- curr_dat$height_above_ellipsoid
  }
  
  # Compute unique dates and indices marking the start of each day
  days <- as.Date(timestamps)
  dates <- sort(unique(days))
  day_start_idxs <- sapply(dates, function(d) min(which(days == d)))
  
  # 7) Return structured data as a list
  return(list(xs = xs, ys = ys, zs = zs, lats = lats, lons = lons, timestamps = timestamps, ids = ids, dates = dates, day_start_idxs = day_start_idxs))
}
