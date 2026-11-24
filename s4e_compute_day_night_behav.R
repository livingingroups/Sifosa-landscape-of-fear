#'----------------------------------------------------------------------------------------------------
#' Script: s4e_compute_day_night_behav.R
#'
#' Compute daily and nightly average behavioral metrics: DDD, pitch, VeDBA, and % head-up
#'
#' @author Maevatiana N. Ratsimbazafindranahaka (primary code author)
#' @author Meredith C. Lutz (code reviewer)
#' @export `sifaka_day_night_behav_[YEAR]_dry.csv`
#-----------------------------------------------------------------------------------------------------

rm(list = ls())

library(dplyr)
library(readxl)
library(ggplot2)
library(tidyr)
library(lubridate)
library(hms)
library(data.table)

# List and source all useful R functions -------------------------------------------------------------------

files <- list.files("C:/Users/maevatiana.ratsim/Documents/Projects/Dev/Sifosa-landscape-of-fear/Utilities/", pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
lapply(files, source)

# Parameters -----------------------------------------------------------------------------------------

season = 'dry'

# Output directory
output_dir <- "C:/Users/maevatiana.ratsim/Documents/Projects/Sifaka/Sifosa-landscape-of-fear-datasets/s4e/"

# Pre-processed GPS data from 's0_make_ari_matrix.R' (one dataset per season/year)
# Contains gps, ddd, group_list, sun_times_filtered, and notes
gps_file <- "C:/Users/maevatiana.ratsim/Documents/Projects/Sifaka/Sifosa-landscape-of-fear-datasets/s0/sifosa_2023_dry_AM.Rdata"

# Path to sifosa encounter data from 's2_extract_sifosa_encounters.R' (one dataset per season/year)
# Contains all_sifosa_encounters and notes
encouters_file <- "C:/Users/maevatiana.ratsim/Documents/Projects/Sifaka/Sifosa-landscape-of-fear-datasets/s1/sifosa_2023_dry_encounters.Rdata"


# Directory containing regularly sampled VeDBA and pitch data derived from ACC data
acc_dir <- "C:/Users/maevatiana.ratsim/Documents/Projects/Sifaka/Sifosa-landscape-of-fear-datasets/ACC/"

# CSV table containing all deployment, group and individual info
depinfo_file <- "C:/Users/maevatiana.ratsim/Documents/Projects/Sifaka/Sifosa-landscape-of-fear-datasets/GPS/reference.csv"

# Read data --------------------------------------------------------------------------------

# Load GPS data
load(gps_file)
head(gps$timestamps) # first timestamp should be “2022-06-24 04:00:00 EAT” in 2022 and “2023-06-25 03:00:00 EAT” in 2023

# Load encounter data
load(encouters_file)

# Load the table containing sifaka group and individual information
depinfo <- fread(depinfo_file)

# Get sunrise and sunset times -------------------------------------------------------------------------------

sun_times = f_get_suntimes(gps$timestamps, -20.783, 44.174, tz(gps$timestamps))

sun_times <- sun_times %>%
  mutate(sunrise = as.POSIXct(sunrise, tz = tz(gps$timestamps)),
         sunset  = as.POSIXct(sunset,  tz = tz(gps$timestamps)))

# Add group ID for each individual in the encounter data -----------------------------------------------------
all_sifosa_encounters <- all_sifosa_encounters %>%
  left_join(group_list %>% select(animal_id, animal_group_id), 
            by = c("indA" = "animal_id")) %>%
  rename(group_id = animal_group_id)


# Get the exhaustive list of dyads for each group ------------------------------------------------------------
dyads_per_group_all <- group_list %>%
  group_by(animal_group_id) %>%
  group_split() %>%
  lapply(f_get_dyads_per_group) %>%
  bind_rows()

# Create the list of valid sifaka IDs
valid_ids <- gps$ids[gps$ids$species == "sifaka",]
rownames(valid_ids) <- NULL


# List dyads from groups containing the selected individuals -------------------------------------------------
dyads_per_group <- dyads_per_group_all %>%
  filter(
    indA %in% valid_ids$id_code & indB %in% valid_ids$id_code
  )

dyads_per_group

# Arrange dyads by group ID
dyads_per_group <- dyads_per_group %>% arrange(group_id)

# View the result
head(dyads_per_group)


# Test the workflow on one individual/group ------------------------------------------------------------------
dev.new()

# Select the focal individual and retrieve its ID, GPS index, and group ID
curr.ind.id = 2;
curr.ind.code = valid_ids$id_code[curr.ind.id]
curr.ind.idx <- gps$ids$idx[gps$ids$id_code == curr.ind.code]
curr.ind.code
curr.group.id = valid_ids$group_id[curr.ind.id]
curr.group.id
curr_year

# Retrieve all dyads belonging to the focal group
curr_group = dyads_per_group[dyads_per_group$group_id == curr.group.id,]
curr_group

# Compute dyadic distances for the focal group
curr_ddd <- f_compute_group_ddd(gps, curr_group, ddd) # Dyadic distances

# Plot the distance time series for one dyad
k = 1
plot(gps$timestamps,
     curr_ddd$group_ddd[,k],
     type = "l", ylab = "Distance", xlab = "Time", main = curr_ddd$dyad_list[k])

# Compute the row-wise average dyadic distance within the group
avg_ddd <- rowMeans(curr_ddd$group_ddd, na.rm = TRUE) # Average group-mean dyadic distance


# List individuals belonging to the focal group
inds <- valid_ids$id_code[valid_ids$group_id == curr.group.id]
inds

# Load the corresponding ACC data for the focal individual
curr.accdata <- read.csv(paste0(acc_dir, str_to_title(curr.ind.code), '_', curr_year, '_', season, '_regular.csv'))

# Convert the timestamp column to POSIXct format
curr.accdata$timestamps <- as.POSIXct(curr.accdata$DT_adj, format = "%Y-%m-%d %H:%M:%S", tz = tz(gps$timestamps))


# Check hourly variation in the behavioral metrics
f_generate_hourly_boxplot(curr.accdata$timestamps, curr.accdata$pitch, sun_times, 
                          title_string = paste0("group #", curr.group.id,
                                                ": ", toupper(curr.ind.code)), data_name = "Average Pitch")

f_generate_hourly_boxplot(curr.accdata$timestamps, curr.accdata$vedba, sun_times, 
                          title_string = paste0("group #", curr.group.id,
                                                ": ", toupper(curr.ind.code)), data_name = "Average VeDBA")

f_generate_hourly_boxplot(gps$timestamps, avg_ddd, sun_times, 
                          title_string = paste0("group #", curr.group.id), data_name = "Average dyadic dist.")


# Calculate and visualize day/night average group cohesion based on average dyadic distance
day_night_avg <- f_compute_day_night_avg(gps$timestamps, avg_ddd, sun_times) 

# Calculate and visualize day/night average pitch
day_night_avg <- f_compute_day_night_avg(curr.accdata$timestamps, curr.accdata$pitch, sun_times) 

# Calculate and visualize day/night average VeDBA
day_night_avg <- f_compute_day_night_avg(curr.accdata$timestamps, curr.accdata$vedba, sun_times) 

# Calculate the proportion of time with pitch above the overall mean + SD of the targeted period
day_night_avg <- f_compute_day_night_abv_ths(curr.accdata$timestamps, curr.accdata$pitch, sun_times, 
                                             th_day = NA, th_night = NA)

# Compute metrics for all individuals ------------------------------------------------------------------------
all_day_night_data <- list() # Initialize an empty list to store the results

for (i in 1:nrow(valid_ids)) { #1:nrow(valid_ids)
  
  # Print processing progress
  cat("Processing:", i, "out of", nrow(valid_ids), "individuals\n")
  
  # Retrieve the current individual's ID, group ID, and GPS index
  curr.ind.code = valid_ids$id_code[i]
  curr.group.id = valid_ids$group_id[i]
  curr.ind.idx <- gps$ids$idx[gps$ids$id_code == curr.ind.code]
  
  # Load ACC data for the current individual
  curr.accdata <- read.csv(paste0(acc_dir, str_to_title(curr.ind.code), '_', curr_year, '_', season, '_regular.csv'))
  
  # Convert the timestamp column to POSIXct format
  curr.accdata$timestamps <- as.POSIXct(curr.accdata$DT_adj, format = "%Y-%m-%d %H:%M:%S", tz = tz(gps$timestamps))
  
  # Filter dyads belonging to the current group
  curr_group <- dyads_per_group[dyads_per_group$group_id == curr.group.id,]
  
  curr_group_gps <- curr_group
  
  # Compute group-level DDD metrics when dyads are available
  if (nrow(curr_group_gps) > 0) {
    
    # Compute dyadic distances
    curr_ddd <- f_compute_group_ddd(gps, curr_group, ddd)
    
    # Count individuals contributing to DDD
    n_inds_ddd <- length(unique(c(curr_group_gps$indB,curr_group_gps$indA)))
    
    # Compute the row-wise mean of dyadic distances
    avg_ddd <- rowMeans(curr_ddd$group_ddd, na.rm = TRUE)
    
    # If no dyads are available, return an NA time series
  } else {
    
    n_inds_ddd <- 0;
    
    avg_ddd <- rep(NA, length(gps$timestamps))
  }
  
  
  # Generate an hourly boxplot of average dyadic distance
  f_generate_hourly_boxplot(gps$timestamps, avg_ddd, sun_times, 
                            title_string = paste0("group #", curr.group.id), 
                            data_name = "Average dyadic dist.")
  
  
  # Calculate day/night average dyadic distance, rename the metric, and add identifiers
  day_night_avg <- f_compute_day_night_avg(gps$timestamps, avg_ddd, sun_times) 
  day_night_data <- day_night_avg %>%
    rename(ddd = metric) %>% mutate(group_id = curr.group.id, animal_id = curr.ind.code)
  
  # Calculate day/night average pitch, rename the metric, and add it to the main table
  day_night_avg <- f_compute_day_night_avg(curr.accdata$timestamps, curr.accdata$pitch, sun_times) 
  day_night_avg <- day_night_avg %>%
    rename(pitch = metric) %>% mutate(group_id = curr.group.id, animal_id = curr.ind.code)
  day_night_data <- day_night_data %>%
    left_join(day_night_avg, by = c("date","day_night", "group_id", "animal_id"))
  
  # Calculate day/night average VeDBA, rename the metric, and add it to the main table
  day_night_avg <- f_compute_day_night_avg(curr.accdata$timestamps, curr.accdata$vedba, sun_times) 
  day_night_avg <- day_night_avg %>%
    rename(vedba = metric) %>% mutate(group_id = curr.group.id, animal_id = curr.ind.code)
  day_night_data <- day_night_data %>%
    left_join(day_night_avg, by = c("date","day_night", "group_id", "animal_id"))
  
  # Compute the proportion of time with pitch above mean + SD, rename the metric, and add it to the main table
  day_night_avg <- f_compute_day_night_abv_ths(curr.accdata$timestamps, curr.accdata$pitch, sun_times, 
                                               th_day = NA, th_night = NA) 
  day_night_avg <- day_night_avg %>%
    rename(headup = metric) %>% mutate(group_id = curr.group.id, animal_id = curr.ind.code)
  day_night_data <- day_night_data %>%
    left_join(day_night_avg, by = c("date","day_night", "group_id", "animal_id"))
  
  
  # Order rows by date and add the numbers of contributing individuals
  day_night_data <- day_night_data[order(day_night_data$date), ]
  day_night_data$n_inds_ddd = n_inds_ddd
  
  
  
  # Store the result in the list
  all_day_night_data[[as.character(i)]] <- day_night_data
}

# Combine all individual data frames into one table
final_day_night_data <- bind_rows(all_day_night_data)
#View(final_day_night_data)


# Incorporate confirmed fosa encounter events ---------------------------------------------------------------

# Step 1: Classify fosa encounters as day or night and adjust the date for pre-sunrise encounters
fosa_encounters_classified <- all_sifosa_encounters %>% 
  mutate(date = as_date(start)) %>%
  left_join(sun_times, by = "date") %>%
  mutate(
    encounter_period = case_when(
      start > sunset ~ "night",                 # After sunset: night associated with the same date
      start < sunrise ~ "night",                # Before sunrise: night associated with the previous date
      TRUE ~ "day"                              # Between sunrise and sunset: day
    ),
    adjusted_date = case_when(
      encounter_period == "night" & start < sunrise ~ date - days(1),  # Assign pre-sunrise encounters to the previous date
      TRUE ~ date
    )
  )

# Step 2: Count encounters by group, date, and period, then reshape to wide format
fosa_encounter_summary <- fosa_encounters_classified %>%
  group_by(adjusted_date, group_id, encounter_period) %>%
  summarise(n_encounters = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = encounter_period,
    values_from = n_encounters,
    names_prefix = "fosa_encounter_",
    values_fill = 0
  ) %>%
  rename(date = adjusted_date)

# Reshape encounter counts to match the day/night structure of the behavioral dataset
fosa_encounter_summary_long <- fosa_encounter_summary %>%
  pivot_longer(cols = c("fosa_encounter_day", "fosa_encounter_night"), 
               names_to = "day_night", 
               values_to = "fosa_encounter") %>%
  mutate(day_night = recode(day_night,
                            fosa_encounter_day = "day",
                            fosa_encounter_night = "night")
  )

# Step 3: Join encounters to the final behavioral dataset and convert counts to yes/no flags
final_day_night_data <- final_day_night_data %>%
  left_join(fosa_encounter_summary_long, by = c("date", "group_id", "day_night")) %>%
  mutate(
    fosa_encounter = if_else(coalesce(fosa_encounter, 0) > 0, "yes", "no")
  )


# Print the final table
print(final_day_night_data)

# Visualize all individuals at once
f_plot_day_night_metric(data = final_day_night_data,
                        metric_name = "vedba",period = "day",
                        y_limits = c(0, 0.2)
)

# Save the final table ---------------------------------------------------------------------------------------
write.csv(final_day_night_data, file = paste0(output_dir, "sifaka_day_night_behav_", curr_year, "_", season, ".csv"), row.names = FALSE)

