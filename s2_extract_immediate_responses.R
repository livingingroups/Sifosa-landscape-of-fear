#'----------------------------------------------------------------------------------------------------
#' Script: s2_extract_immediate_responses.R
#'
#' Processes behavioral responses of sifaka based on sifosa encounter data from s1_extract_sifosa_encounters.R
#' and using GPS data from s0_make_ari_matrix.R
#' It computes behavioral metrics (Pitch and VeDBA) by sifosa distance intervals as fosa approaches/retires,
#  and generates control data from non-encounter days
#'
#' @author Maevatiana N. Ratsimbazafindranahaka (primary code author)
#' @author Meredith Lutz (code reviewer)
#' @export `sifosa_[year]_[response metric]_resp.RData` dataframe containing average response per distance interval to fossa per encounter
#'----------------------------------------------------------------------------------------------------------

rm(list = ls())

library(ggplot2)
library(dplyr)
library(lubridate)
library(readr)
library(tidyr)
library(suncalc)
library(stringr)

# List and source all useful R functions -------------------------------------------------------------------

files <- list.files(
  "C:/Users/maevatiana.ratsim/Documents/Projects/Dev/Sifosa-landscape-of-fear/Utilities/",
  pattern = "\\.R$",
  full.names = TRUE,
  recursive = TRUE
)
lapply(files, source)

# Parameters -----------------------------------------------------------------------------------------------------

# Set which data to analyze. Options include:
# - "vedba" (for VeDBA)
# - "pitch" (for Pitch)
# - "movement" for displacement
data_to_view = "vedba"

season = "dry"
# Encounter data full path. 1 file = one season/year. Contains all_sifosa_encounters, notes
enc_file       <- "C:/Users/maevatiana.ratsim/Documents/Projects/Sifaka/Sifosa-landscape-of-fear-datasets/s1/sifosa_2022_dry_encounters.Rdata"
# GPS data in AM format full path. 1 file = one season/year. Contains gps, ddd, group_list, and notes
gps_file       <- "C:/Users/maevatiana.ratsim/Documents/Projects/Sifaka/Sifosa-landscape-of-fear-datasets/s0/sifosa_2022_dry_AM.Rdata"

# Regularly sampled ACC data (VeDBA and pitch) location (1 csv = 1 individual 1 year 1 season)
ACC_dir       <- "C:/Users/maevatiana.ratsim/Documents/Projects/Sifaka/Sifosa-landscape-of-fear-datasets/ACC/"
# Output directory
output_dir    <- "C:/Users/maevatiana.ratsim/Documents/Projects/Sifaka/Sifosa-landscape-of-fear-datasets/s2/"

# Define the time range around each encounter to be considered for analysis
time_range    <- hours(2)

# Define the distance intervals to be used for analyzing behavioral responses to distance to fossa changes.
# e.g., with seq(350, 50, by = -100), the distances range from 350m to 50m in decrements of 100m.
# Subsequent step will add the additional range "≤ 50m".
search_values <- seq(350, 50, by = -100)

# Load data ------------------------------------------------------------------------------------------------------

load(enc_file) # Encounters
load(gps_file) # gps
head(gps$timestamps) # first timestamp should be “2022-06-24 04:00:00 EAT” in 2022 and “2023-06-25 03:00:00 EAT” in 2023

# Check ACC timestamps (just one individual)
curr.accdata <- read.csv(paste0(ACC_dir, "S03_2022_dry_regular.csv"))
curr.accdata$timestamps <- as.POSIXct(curr.accdata$DT_adj, tz = tz(gps$timestamps))
head(curr.accdata$timestamps) # first timestamp should be "2022-06-24 03:01:00 EAT"
curr.accdata <- read.csv(paste0(ACC_dir, "S03_2023_dry_regular.csv"))
curr.accdata$timestamps <- as.POSIXct(curr.accdata$DT_adj, tz = tz(gps$timestamps))
head(curr.accdata$timestamps) # first timestamp should be "2023-06-25 03:00:00 EAT"

# Quick inspection: Visualize the VeDBA and distance of one encounter ---------------------------------------------

# e.g., i = 11 for the encounter #11 in all_sifosa_encounters loaded from enc_path
f_view_vedba(i = 11,
             time_range = time_range+hours(5),
             all_sifosa_encounters,
             ACC_dir,
             gps, season)

# Test distance intervals and data extraction on one encounter -----------------------------------------------------------------

# Compute the response for a a given sifaka (indA) - fossa (indB) encounter (calculate the average of the selected metric within each distance interval)
enc_to_view = 2 # e.g., 1 for encounter #1

# During approach
values1 = f_get_response_by_dist(
  search_values,
  time_range,
  enc_to_view,
  all_sifosa_encounters,
  ACC_dir,
  gps,
  ddd,
  data_to_view,
  "approach", season
)
# During departure
values2 = f_get_response_by_dist(
  search_values,
  time_range,
  enc_to_view,
  all_sifosa_encounters,
  ACC_dir,
  gps,
  ddd,
  data_to_view,
  "departure", season
)
print(values1)
print(values2)


# Assemble approach and departure into one, removing duplicate "≤ 50" intervals (both computed for the approach and departure)
common_rows <- inner_join(values1, values2, by = names(values1))
has_common_row <- common_rows %>%
  filter(dist == "≤ 50") %>%
  nrow() == 1

# Combine, removing the duplicate if found
if (has_common_row) {
  values <- bind_rows(values1, values2) %>%
    distinct()  # Removes exact duplicates
} else {
  print("WARNING. ≤ 50 period doesn't match")
}
values # Quick check

# Plot the computed values
values$dist <- factor(values$dist,
                      levels = unique(values$dist),
                      ordered = TRUE)
par(mfrow = c(1, 1))
plot(
  as.numeric(values$dist),
  values$value,
  type = "b",
  pch = 16,
  xaxt = "n",
  xlab = "Distance",
  ylab = str_to_title(data_to_view),
  main = paste0(
    str_to_title(data_to_view),
    " vs. Distance for encounter #",
    enc_to_view
  )
)
axis(1, at = as.numeric(values$dist), labels = values$dist)

# Loop through all encounters and compute the responses per distance interval ------------------------------------------------------------------------
# Also generate control data (data corresponding to the exact same time windows but on days no encounters were registered for that individual) -----------------------------------------------

options(timeout = 600) # Increase the timeout limit to 600 seconds (10 minutes) to prevent interruptions during long operations.

all_values <- list() # Initialize an empty big list to store values from each encounter

for (enc_to_view in 1:nrow(all_sifosa_encounters)) {
  # For each encounter
  
  # Call the function f_get_response_by_dist to compute behavioral data for the current encounter
  values1 = f_get_response_by_dist(
    search_values,
    time_range,
    enc_to_view,
    all_sifosa_encounters,
    ACC_dir,
    gps,
    ddd,
    data_to_view,
    "approach", season
  )
  values2 = f_get_response_by_dist(
    search_values,
    time_range,
    enc_to_view,
    all_sifosa_encounters,
    ACC_dir,
    gps,
    ddd,
    data_to_view,
    "departure", season
  )
  
  # Assemble approach and departure into one, removing duplicate "≤ 50" intervals (both computed for the approach and departure)
  common_rows <- inner_join(values1, values2, by = names(values1))
  has_common_row <- common_rows %>%
    filter(dist == "≤ 50") %>%
    nrow() == 1
  if (has_common_row) {
    values <- bind_rows(values1, values2) %>%
      distinct()  # Removes exact duplicates
  } else {
    print("WARNING. ≤ 50 period doesn't match")
  }
  
  # Add metadata related to the encounter
  values$encounter <- enc_to_view  # Assign encounter identifier
  values$group_id <- group_list$animal_group_id[group_list$animal_id == values$indA[1]]  # Retrieve group ID
  
  # Generate control data (behavioral data during non-encounter days)
  
  # Load acceleration data for the current individual sifaka and ensure consistent timezone
  curr.ind <- all_sifosa_encounters$indA[enc_to_view]
  curr.accdata <- read.csv(paste0(
    ACC_dir,
    curr.ind,
    '_',
    names(which.max(table(
      year(all_sifosa_encounters$start)
    ))),
    '_',
    season,
    '_regular.csv'
  ))
  curr.accdata$timestamps <- as.POSIXct(curr.accdata$DT_adj, tz = tz(gps$timestamps))
  curr.ind.idx <- gps$ids$idx[which(gps$ids$id_code == curr.ind)]
  curr.xs <- gps$xs[curr.ind.idx, ]
  curr.ys <- gps$ys[curr.ind.idx, ]
  
  
  # Identify all encounters the current individual experienced
  all_curr_ind_encounters <- all_sifosa_encounters[all_sifosa_encounters$indA == curr.ind, ]
  
  # Identify non-encounter dates for control data selection
  dates <- as_date(gps$timestamps)  # Extract only date components of the timestamps
  encounter_dates <- unique(c(
    as_date(all_curr_ind_encounters$start),
    as_date(all_curr_ind_encounters$end)
  ))  # List all encounter dates
  gpsdata_control <- !dates %in% encounter_dates  # Logical vector: TRUE if the date is not an encounter day
  non_encounter_dates <- unique(dates[gpsdata_control])  # Extract dates without encounters
  
  # Extract the start and end times of each distance interval of the current encounter
  time_parts_start <- format(values$start, "%H:%M:%S", tz = tz(gps$timestamps))
  time_parts_end <- format(values$end, "%H:%M:%S", tz = tz(gps$timestamps))
  
  # Initialize columns for control data storage.
  # We use lists so that for one row (distance interval) in the column values,
  # there is one column containing a list of all corresponding control data
  values <- values %>%
    mutate(ctrl = vector("list", nrow(values)),
           non_encounter_dates = vector("list", nrow(values)))
  
  # Loop through each row/dist. interval in `values` for the current encounter
  # to compute control averages
  for (k in 1:nrow(values)) {
    # Initialize vector to store control averages corresponding to current dist. interval (k)
    avg <- rep(NA, length(non_encounter_dates))
    
    # Loop through each non-encounter date to compute the corresponding average
    if (data_to_view == "movement") {
      for (i in 1:length(non_encounter_dates)) {
        # Build the start and end in datetime format
        
        # Case 1: The time interval for the current dist. interval occurs within a single day
        if (values$end[k] != values$start[k] &
            as_date(values$end[k]) == as_date(values$start[k])) {
          ddd.range.time <- gps$timestamps >= as.POSIXct(
            paste(non_encounter_dates[i], time_parts_start[k]),
            format = "%Y-%m-%d %H:%M:%S",
            tz = tz(gps$timestamps)
          ) &
            gps$timestamps <= as.POSIXct( #included bc start interval i+1 relative to start interval i
              paste(non_encounter_dates[i], time_parts_end[k]),
              format = "%Y-%m-%d %H:%M:%S",
              tz = tz(gps$timestamps)
            )
          
          # Case 2: The time interval for the current dist. interval spans multiple days
          # e.g., starts at 23h59 and ends at 00h05
        } else if (values$end[k] != values$start[k] &
                   as_date(values$end[k]) > as_date(values$start[k])) {
          ddd.range.time <- gps$timestamps >= as.POSIXct(
            paste(non_encounter_dates[i], time_parts_start[k]),
            format = "%Y-%m-%d %H:%M:%S",
            tz = tz(gps$timestamps)
          ) &
            gps$timestamps <= as.POSIXct(
              paste(non_encounter_dates[i], time_parts_end[k]),
              format = "%Y-%m-%d %H:%M:%S",
              tz = tz(gps$timestamps)
            ) + days(1)
          
        } else {
          # Remaining case: The time interval for the current dist. interval is extremely short (less than the data resolution)
          ddd.range.time <- gps$timestamps >= as.POSIXct(
            paste(non_encounter_dates[i], time_parts_start[k]),
            format = "%Y-%m-%d %H:%M:%S",
            tz = tz(gps$timestamps)
          ) &
            gps$timestamps <= as.POSIXct(
              paste(non_encounter_dates[i], time_parts_start[k]),
              format = "%Y-%m-%d %H:%M:%S",
              tz = tz(gps$timestamps)
            ) + minutes(3)  # Add 3 min buffer. Assume GPS data every 3 minutes
        }
        
        # Calculate the displacement of the animal from the start of the interval to its end.
        
        xi <- curr.xs[ddd.range.time][1]
        yi <- curr.ys[ddd.range.time][1]
        xj <- tail(curr.xs[ddd.range.time], 1)
        yj <- tail(curr.ys[ddd.range.time], 1)
        
        
        # Get distances and store
        dists_ij <- sqrt((xi - xj)^2 + (yi - yj)^2)
        
        if (length(dists_ij) == 0) {# No data on that date 
          avg[i] = NA
        } else {
          avg[i] <- dists_ij
        } 
        
      }
    } else {
      for (i in 1:length(non_encounter_dates)) {
      # Build the start and end in datetime format
      
      # Case 1: The time interval for the current dist. interval occurs within a single day
      if (values$end[k] != values$start[k] &
          as_date(values$end[k]) == as_date(values$start[k])) {
        ddd.range.time <- curr.accdata$timestamps >= as.POSIXct(
          paste(non_encounter_dates[i], time_parts_start[k]),
          format = "%Y-%m-%d %H:%M:%S",
          tz = tz(gps$timestamps)
        ) &
          curr.accdata$timestamps < as.POSIXct(
            paste(non_encounter_dates[i], time_parts_end[k]),
            format = "%Y-%m-%d %H:%M:%S",
            tz = tz(gps$timestamps)
          )
        
        # Case 2: The time interval for the current dist. interval spans multiple days
        # e.g., starts at 23h59 and ends at 00h05
      } else if (values$end[k] != values$start[k] &
                 as_date(values$end[k]) > as_date(values$start[k])) {
        ddd.range.time <- curr.accdata$timestamps >= as.POSIXct(
          paste(non_encounter_dates[i], time_parts_start[k]),
          format = "%Y-%m-%d %H:%M:%S",
          tz = tz(gps$timestamps)
        ) &
          curr.accdata$timestamps < as.POSIXct(
            paste(non_encounter_dates[i], time_parts_end[k]),
            format = "%Y-%m-%d %H:%M:%S",
            tz = tz(gps$timestamps)
          ) + days(1)
        
      } else {
        # Remaining case: The time interval for the current dist. interval is extremely short (less than the data resolution)
        ddd.range.time <- curr.accdata$timestamps >= as.POSIXct(
          paste(non_encounter_dates[i], time_parts_start[k]),
          format = "%Y-%m-%d %H:%M:%S",
          tz = tz(gps$timestamps)
        ) &
          curr.accdata$timestamps < as.POSIXct(
            paste(non_encounter_dates[i], time_parts_start[k]),
            format = "%Y-%m-%d %H:%M:%S",
            tz = tz(gps$timestamps)
          ) + minutes(3)  # Add 3 min buffer. Assume GPS data every 3 minutes
      }
      
      # Calculate the mean of the specified data within the determined time interval.
      avg[i] <- mean(curr.accdata[[data_to_view]][ddd.range.time], na.rm = TRUE)
      }
    }
    
    ## Print progress for debugging purposes
    print(paste0("Encounter #", enc_to_view, ", dist #", k))
    #print(avg)
    
    # Store computed control values in the corresponding columns
    values$ctrl[[k]] <- avg  # Store list of control data from different days
    values$non_encounter_dates[[k]] <- non_encounter_dates  # Store corresponding control dates
  }
  
  # Store the processed data frame for this encounter in the big list
  all_values[[enc_to_view]] <- values
  print("Done")
}

# Combine all encounter data frames into one structured dataset
combined_values <- do.call(rbind, all_values)

# Convert 'dist' to an ordered factor to ensure correct ordering in analyses and plots.
combined_values$dist <- factor(
  combined_values$dist,
  levels = unique(combined_values$dist),
  ordered = TRUE
)

# Flag daytime and nighttime -----------------------------------------------------------------------------------------------------------------------

# Get sunrise and sunrise times for each of the distance interval per encounter
sun_times = f_get_suntimes(combined_values$start,
                         -20.783,
                         44.174,
                         tz(combined_values$start))
sun_times <- sun_times %>%
  mutate(sunrise = as.POSIXct(sunrise, tz = tz(combined_values$start)),
         sunset  = as.POSIXct(sunset, tz = tz(combined_values$start)))


# Initialize 'time_day' with default "" for all rows.
combined_values$time_day <- ""

# Determine whether all recorded distance in each encounter occurred during the day or night
# If all happen during the day -> flag as day
# If all happen during night -> flag as night
# Otherwise -> flag as Crepuscular

combined_values <- combined_values %>%
  mutate(date = as.Date(start)) %>%
  left_join(sun_times, by = "date") %>%
  group_by(encounter) %>%
  mutate(
    start_within_day = start >= sunrise & start <= sunset,
    end_within_day   = end   >= sunrise & end   <= sunset,
    
    time_day = case_when(
      all(start_within_day & end_within_day)   ~ "Day",
      all(!start_within_day & !end_within_day) ~ "Night",
      TRUE                                      ~ "Crep"
    ),
    
    time_rel = as.factor((start - first(start)) / 60)
  ) %>%
  ungroup() %>%
  select(-start_within_day, -end_within_day)

# Quick inspection --------------------------------------------------------------------------------

# Final data set for the current year/season
print(combined_values)

# Summarize the sample

# Remove duplicate encounters to ensure each fosa pair is counted only once
combined_values_filtered = combined_values[combined_values$duplicate != T, ]

# temporal characteristics of the distance intervals
print(
  combined_values_filtered %>% group_by(time_day, dist) %>%
    summarise(
      avg_dur_minutes = as.numeric(median(end - start)) / 60,
      min_dur_minutes = as.numeric(min(end - start)) / 60,
      max_dur_minutes = as.numeric(max(end - start)) / 60,
      n = n()
    ),
  n = 100
)

# counting unique individuals and groups, and calculating the average number of individuals per group
print(
  combined_values_filtered %>%
    mutate(year = year(start)) %>%
    group_by(year) %>%
    summarize(
      # Count unique individuals
      total_unique_individuals = n_distinct(indA),
      # Count unique groups
      total_unique_groups = n_distinct(group_id),
      # Average unique groups
      avg_individuals_per_group = total_unique_individuals / total_unique_groups
    ),
  n = 100
)
# year      total_unique_individuals total_unique_groups avg_individuals_per_group
# 2022 dry                       13                   7                      1.86
# 2023 dry                       22                  10                      2.2

# Count the number of unique encounters
print(length(unique(combined_values_filtered$encounter)))
# 2022 dry: 17, 2023 dry: 44

# Show non encounter dates
sort(unique(as.Date(
  unlist(combined_values$non_encounter_dates)
)))
# 2022 dry: 2022-06-24 to 2022-07-09
# 2023 dry: 2023-06-25 to 2023-07-23

# Visualization: Box plot and scatter plot of response values across distances
var_name = "dist"
ggplot(
  combined_values_filtered,
  aes(
    x = !!sym(var_name),
    y = value,
    group = encounter,
    colour = fosa_type
  )
) +
  # Boxplot per distance
  geom_boxplot(
    aes(group = !!sym(var_name)),
    alpha = 0.3,
    outlier.shape = NA,
    colour = "black",
    fill = "gray"
  ) +
  # Individual data points with transparency
  geom_point(aes(shape = fosa_type), size = 2, alpha = 0.5) +
  # Connect paired data with jittered lines
  geom_line(aes(group = encounter),
            position = position_jitter(width = 0.1),
            alpha = 0.5) +
  # Define shape aesthetics for different fosa types
  scale_shape_manual(values = c(20, 8)) +
  # Create separate panels by time of day and sex
  facet_wrap( ~ time_day, ncol = 1) +
  labs(
    x = str_to_title(var_name),
    y = str_to_title(data_to_view),
    colour = "Fosa group type",
    shape = "Fosa group type"
  ) +
  theme_bw()+theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank())

# Save --------------------------------------------------------------------------------------------------------------------------------------
# Define file path: year as indicated in curr_year, data name as indicated in data_to_view
file_file <- paste0(output_dir,
                    "sifosa_",
                    curr_year,
                    '_',
                    season,
                    '_',
                    data_to_view,
                    '_resp.RData')
notes <- paste0("These data were generated using the script s3_extract_immediate_responses.R")
save(combined_values, notes, file = file_file)
print(paste("Data saved to:", file_file))

