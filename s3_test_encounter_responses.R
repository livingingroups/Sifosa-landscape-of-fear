#'----------------------------------------------------------------------------------------------------
#' Script: s3_test_encounter_responses.R
#'
#' This script test if there is any significant behavioral changes of sifakas
#' to fossa encounters, using response data from s3_extract_immediate_responses.R and a permutation approach
#'
#' @author Maevatiana N. Ratsimbazafindranahaka (primary code author)
#' @author Damien Caillaud (code reviewer)
#'----------------------------------------------------------------------------------------------------------

rm(list = ls())

if (!require(devtools)) {
  install.packages('devtools')
}
devtools::install_github('erocoar/gghalves')


library(ggplot2)
library(gghalves)
library(dplyr)
library(readxl)
library(lubridate)
library(readr)
library(tidyr)
library(lme4)
library(lmerTest)
library(patchwork)
library(data.table)

# List and source all useful R functions -------------------------------------------------------------------

files <- list.files(
  "C:/Users/maevatiana.ratsim/Documents/Projects/Dev/Sifosa-landscape-of-fear/Utilities/",
  pattern = "\\.R$",
  full.names = TRUE,
  recursive = TRUE
)
lapply(files, source)

# Parameters -----------------------------------------------------------------------------------------------------

# Define which response data to analyze: "pitch" or "vedba" or "movement" (acceleration-based behavioral metric)
data_to_view = "pitch"

# Response data location [containing 2022 and 2023 dry data]
resp_dir <- "C:/Users/maevatiana.ratsim/Documents/Projects/Sifaka/Sifosa-landscape-of-fear-datasets/s2/"
# Location of GPS data in AM format [containing 2022 and 2023 dry data]
gps_dir  <- "C:/Users/maevatiana.ratsim/Documents/Projects/Sifaka/Sifosa-landscape-of-fear-datasets/s0/"
# Regularly sampled ACC data (VeDBA and pitch) location (1 csv = 1 individual 1 year 1 season)
ACC_dir   <- "C:/Users/maevatiana.ratsim/Documents/Projects/Sifaka/Sifosa-landscape-of-fear-datasets/ACC/"

# Table containing all deployment, group and individual info
depinfo_file <- "C:/Users/maevatiana.ratsim/Documents/Projects/Sifaka/Sifosa-landscape-of-fear-datasets/GPS/reference.csv"

# Output figure directory
fig_dir <- "C:/Users/maevatiana.ratsim/Documents/Projects/Sifaka/Sifosa-landscape-of-fear-figures/"
# Output file directory
out_dir <- "C:/Users/maevatiana.ratsim/Documents/Projects/Sifaka/Sifosa-landscape-of-fear-datasets/s3/"

# Load pre-processed data from dry season 2022 and 2023 ----------------------------------------------------------------------------------------------

# 2022
load(paste0(resp_dir, "sifosa_2022_dry_", data_to_view, "_resp.RData")) # Response data
data2022 <- combined_values  # Store the 2022 response data under a distinct name
load(paste0(gps_dir, "sifosa_2022_dry_AM.Rdata")) # gps data
gps_ids2022 <- gps$ids
gps_ids2022$year = "2022" # Store the 2022 individual mapping data under a distinct name
group_list2022 <- group_list # Store the 2022 group list under a distinct name

#2023
load(paste0(resp_dir, "sifosa_2023_dry_", data_to_view, "_resp.RData")) # Response data
data2023 <- combined_values  # Store the 2023 response data under a distinct name
load(paste0(gps_dir, "sifosa_2023_dry_AM.Rdata")) # gps data
gps_ids2023 <- gps$ids
gps_ids2023$year = "2023" # Store the 2023 individual mapping data under a distinct name
group_list2023 <- group_list # Store the 2023 group list under a distinct name

# Load deployment info
depinfo <- fread(depinfo_file)

# Combine the individual mapping data and group list for a full list of individual to work with
gps_ids <- rbind(gps_ids2022, gps_ids2023)
group_list <- rbind(group_list2022, group_list2023)
gps_ids %>% arrange(id_code)

# Get the list of dyads per group
dyads_per_group_all <- group_list %>%
  distinct(animal_id, animal_group_id, .keep_all = TRUE) %>%   # keep unique combos
  group_by(animal_group_id) %>%
  group_split() %>%
  lapply(f_get_dyads_per_group) %>%
  bind_rows()

# Get a list of the sifaka to work with
valid_ids <- unique(gps_ids$id_code[gps_ids$species == "sifaka"])
dyads_per_group <- dyads_per_group_all %>%
  filter(indA %in% valid_ids & (is.na(indB) | indB %in% valid_ids))
dyads_per_group

# Ensure unique encounter IDs in 2023 and combine with 2022 responses data
data2023$encounter <- data2023$encounter + max(data2022$encounter)  # Offset encounter IDs to avoid duplication
combined_values <- rbind(data2022, data2023)  # Merge both years' datasets into a single dataframe

# Remove duplicate encounters to ensure encounter with fosa pair is counted only once
combined_values_filtered <- combined_values[combined_values$duplicate != TRUE, ]  # Exclude rows where "duplicate" is TRUE

# Calculate encounter duration in minutes
combined_values_filtered <- combined_values_filtered %>%
  group_by(encounter) %>%
  mutate(
    # Total encounter duration (from first start to last end) in minutes
    transit_duration_min = as.numeric(difftime(max(end), min(start), units = "mins")),
    
    # Duration of the "< 50" distance category (from first start to last end within "< 50")
    encounter_duration_min = ifelse(any(dist == "< 50"), as.numeric(difftime(
      max(end[dist == "< 50"]), min(start[dist == "< 50"]), units = "mins"
    )), NA),
    # Assign NA if no "< 50" distance category exists
  ) %>%
  ungroup()

# Display the updated dataframe
combined_values_filtered

# Display all dates with encounter
sort(unique(combined_values_filtered$date))
# [1] "2022-06-28" "2022-06-29" "2022-06-30" "2022-07-01" "2022-07-02" "2022-07-03" "2022-07-06" "2022-07-07" "2022-07-08"
# [10] "2023-06-27" "2023-06-28" "2023-06-29" "2023-06-30" "2023-07-02" "2023-07-03" "2023-07-07" "2023-07-08" "2023-07-10"
# [19] "2023-07-11" "2023-07-12" "2023-07-13" "2023-07-14" "2023-07-18" "2023-07-20" "2023-07-21

# Display all collared fosa involved
unique(combined_values_filtered$indB) # [1] "F01" "F06" "F03" "F04" "F05"
length(unique(combined_values_filtered$indB)) # 5
# Encounters with a pair
length(unique(combined_values_filtered$encounter[
  combined_values_filtered$time_day != "Crep" & combined_values_filtered$fosa_type == "pair"])) #11

# Independent approach events
enc_list <- combined_values_filtered %>%
  filter(dist == "≤ 50")
indep_list <- enc_list %>%
  group_by(date, indB, group_id, time_day) %>%
  summarise(
    n = n(),
    start = paste(start, collapse = ", "),
    end = paste(end, collapse = ", "),
    inds = paste(indA, collapse = ", "),
    .groups = "drop"
  )
#View(indep_list) #Check first that all approaches grouped under in the same row really occurs at the more or less at the same time
nrow(indep_list[indep_list$time_day != "Crep",]) #25
length(unique(indep_list$group_id[indep_list$time_day != "Crep"])) #11

# Restrict the data to only one encounter by one individual per group per day  -------------------------------------------------
# This is optional (mostly for robustness check). Ignored by default.
# Set robustness_check to TRUE if you want to run this

robustness_check = F
keep_same_unique_encounters_set = F # After running the first metric/period, set to true to keep the randomly selected set consistent

if (robustness_check) {
  if (keep_same_unique_encounters_set) {
    combined_values_filtered <- combined_values_filtered %>% filter(encounter %in% unique_ind_random$encounter)
    unique_ind_random$encounter
    # [1]  6  8  5  7 12  2  1 13 16 10 30 53 22 41 23 57 46 51 56 55 61 32 19 25 58 21 50 42
    # [29] 33 48 39 60
  } else {
      # List the unique encounters
      unique_encounters = combined_values_filtered %>% filter(dist == unique(combined_values_filtered$dist)[4])
      
      # pick one random row per group per day
      set.seed(1)
      unique_ind_random <- unique_encounters %>%
        group_by(date, group_id) %>%
        slice_sample(n = 1) %>%
        ungroup()
      
      # Keep only these picked row
      combined_values_filtered <- combined_values_filtered %>% filter(encounter %in% unique_ind_random$encounter)
  }
}

# Inspect sample size ------------------------------------------------------

# N individuals and group per day and night
summary_stats <- combined_values_filtered %>%
  filter(time_day != "Crep") %>%
  #mutate(year = year(start)) %>%  # Extract the year
  #group_by(year) %>% # For each year
  summarize(
    # Count the number of unique individuals that encountered a fossa
    total_unique_individuals = n_distinct(indA),
    # Count the number of unique groups that encountered a fossa
    total_unique_groups = n_distinct(group_id),
    # Compute average individuals per group that encountered a fossa
    avg_individuals_per_group = total_unique_individuals / total_unique_groups
  )
summary_stats # Shows result
# total_unique_individuals total_unique_groups avg_individuals_per_group
#                      24                  11                      2.18

# N encounter per individual (Crep excluded)
summary_stats <- combined_values_filtered %>%
  filter(time_day != "Crep") %>%
  distinct(encounter, .keep_all = TRUE) %>%
  group_by(indA) %>% # For each year
  summarize(
    n = n()
  )
sum(summary_stats$n) # 45
length(unique(combined_values_filtered$encounter[
  combined_values_filtered$time_day == "Night"])) #30
length(unique(combined_values_filtered$encounter[
  combined_values_filtered$time_day == "Day"])) #15
mean(summary_stats$n) # 1.875
sd(summary_stats$n) # 0.8998792

# Print the total number of unique encounters (Night Day Crep)
length(unique(combined_values_filtered$encounter)) #61

# Show proportion of day, night and crep encounters
u <- unique(combined_values_filtered[, c("encounter", "time_day")])
tbl <- table(u$time_day)
result <- data.frame(
  time_day = names(tbl),
  count = as.vector(tbl),
  proportion = as.vector(prop.table(tbl) * 100)
)        # counts
result # proportions
# time_day count proportion
#      Crep    16   26.22951
#       Day    15   24.59016
#     Night    30   49.18033

# Quick data visualization -------------------------------------------------------
dev.set(4)
var_name  <- "dist"

# Boxplot and line plot of responses by distance by time of day by sex
ggplot(
  combined_values_filtered,
  aes(
    x = !!sym(var_name),
    y = value,
    group = encounter,
    colour = fosa_type
  )
) +
  geom_boxplot(
    aes(group = !!sym(var_name)),
    alpha = 0.3,
    outlier.shape = NA,
    colour = "black",
    fill = "gray"
  ) +
  geom_point(aes(shape = fosa_type), size = 2, alpha = 0.5) +  # Jittered points to show individual data
  geom_line(aes(group = encounter),
            position = position_jitter(width = 0.1),
            alpha = 0.5) +  # Connect paired data
  scale_shape_manual(values = c(20, 8)) +  # Define shapes for different fosa types
  facet_wrap( ~ time_day, ncol = 1) +  # Facet by time of day and sex
  labs(
    x = toupper(var_name),
    y = toupper(data_to_view),
    colour = "Fosa group type",
    shape = "Fosa group type"
  ) +
  theme_bw()

# Compute null data by permutation an test --------------------------------------------------------------------------------

# Choose whether you want to analyze night or daytime data
period_to_view <- "Night" # "Night" or "Day"

# Filter the data set to include only encounters in the specified period of the day
curr.period_data <- combined_values_filtered %>%
  filter(time_day == period_to_view)

# Calculate the duration of each dist interval
curr.period_data$int_dur  <- as.numeric(curr.period_data$end - curr.period_data$start)

# Quick inspection of the duration of each distance interval
ggplot(curr.period_data, aes(x = dist, y = int_dur)) +
  geom_boxplot(alpha = 0.3) + ylab("Duration (s)") + xlab("Distance intervals") +
  theme_bw()


# Inspection of the distribution of the encounters over the day

# Creates decimal time (no date) for start, end and midpoint
curr.period_data <- curr.period_data %>%
  mutate(
    hour_decimal_start = hour(start) + minute(start) / 60 + second(start) / 3600,
    hour_decimal_end   = hour(end)   + minute(end) / 60   + second(end) / 3600,
    # Compute duration, handling midnight crossing
    duration = if_else(
      hour_decimal_end < hour_decimal_start,
      (24 - hour_decimal_start) + hour_decimal_end,
      # cross midnight
      hour_decimal_end - hour_decimal_start           # same day
    ),
    # Midpoint in hours
    hour_decimal_mid = (hour_decimal_start + duration / 2) %% 24
  ) %>% select(-duration)

# Histogram of the duration of each encounter (when fosa is within 350 m)
ggplot(curr.period_data, aes(x = transit_duration_min / 60)) +
  geom_histogram(
    binwidth = 1,
    alpha = 0.3,
    fill = "steelblue",
    color = "black"
  ) +
  theme_minimal() +
  labs(x = "Duration (h)", y = "Count")

# Histogram of when each distance interval occurred during the day
ggplot(curr.period_data, aes(x = hour_decimal_mid)) +
  annotate(
    "rect",
    xmin = 17,
    xmax = 25,
    ymin = -Inf,
    ymax = Inf,
    fill = "grey70",
    alpha = 0.3
  ) +
  annotate(
    "rect",
    xmin = -1,
    xmax = 7,
    ymin = -Inf,
    ymax = Inf,
    fill = "grey70",
    alpha = 0.3
  ) +
  geom_histogram(
    binwidth = 2,
    alpha = 0.3,
    fill = "steelblue",
    color = "black"
  ) +
  theme_minimal() +
  facet_wrap( ~ dist, ncol = 1) +
  labs(x = "Hour", y = "Count")

# Compute average and standard deviation of observed response (accross encounters) for each distance category
averages_table <- curr.period_data %>%
  group_by(dist) %>%
  summarise(n = sum(!is.na(value)),
            avg_value = mean(value, na.rm = TRUE),
            std_value = sd(value, na.rm = TRUE))

# Initialize an empty list column to store control values for each distance
averages_table <- averages_table %>%
  mutate(ctrl = vector("list", nrow(averages_table)),
         ctrl_std = vector("list", nrow(averages_table)))

# Build a table listing unique encounters
k <- 1  # Use the first distance category
unique_encounters <- curr.period_data %>%
  filter(dist == levels(dist)[k]) %>%
  distinct()

# Get list of all study dates
all_sampled_dates = sort(unique(c(
  as.Date(unlist(combined_values$non_encounter_dates)), as.Date(unique_encounters$start)
)))

# Build a look-up table for the dates and for storing original day order
date_lookup <- data.frame(
  idx = 1:length(all_sampled_dates),
  date = all_sampled_dates,
  year = year(all_sampled_dates)
)

# Get a list of the unique distance categories
int_list <- unique(curr.period_data$dist)

# Perform day reshuffling to generate null distributions
set.seed(1) # Set seed for reproducibility in subsequent random sampling
# Perform 1000 iterations
for (i in 1:1000) {
  print(i)
  
  # Initialize a list to store sampled values for each distance category
  samples = replicate(length(int_list), rep(NA, nrow(unique_encounters)), simplify = FALSE)
  
  # Create reshuffled day index in date_lookup
  date_lookup <- date_lookup %>% group_by(year) %>%
    mutate(idx_shuffled = sample(idx))
  
  # Iterate through each individual encounter as listed in unique_encounters
  for (j in 1:nrow(unique_encounters)) {
    # Get the control date sampled (i.e., idx in the reshuffled) for this encounter
    new_date_idx = date_lookup$idx_shuffled[date_lookup$date == as.Date(unique_encounters$start[j])]
    new_date = date_lookup$date[date_lookup$idx == new_date_idx]
    new_date
    
    # Get the index of the non-encounter samples that correspond to that sampled day
    sampled_idx <- which(unique_encounters$non_encounter_dates[[j]] == new_date)
    # if this does not returns anything, that means the sampled date in not a non-encounter day for that individual
    # (likely cause the same day was resampled)
    
    # Iterate through each distance category and assign sampled control values (corresponding to sampled_idx)
    # Ensures that the control data for different distance categories,
    # selected in the current iteration for the current encounter,
    # come from the same day by using sampled_idx
    for (jj in 1:length(int_list)) {
      if (length(sampled_idx) != 0) {
        samples[[jj]][j] = curr.period_data[curr.period_data$dist == int_list[jj] &
                                              curr.period_data$encounter == unique_encounters$encounter[j], ]$ctrl[[1]][sampled_idx]
      } else {
        samples[[jj]][j] = NA
      }
    }
  }
  
  # Compute mean and standard deviation of control samples for each distance category
  for (jj in 1:length(int_list)) {
    averages_table$ctrl[[jj]][i] = mean(unlist(samples[[jj]]), na.rm = TRUE)  # Compute mean of sampled control values
    averages_table$ctrl_std[[jj]][i] = sd(unlist(samples[[jj]]), na.rm = TRUE)  # Compute standard deviation
  }
}

# Compute summary statistics for null distributions
summary_stats_control <- data.frame(
  Intervals = averages_table$dist,
  Control_Mean = sapply(averages_table$ctrl, mean, na.rm = TRUE),
  Control_SD = sapply(averages_table$ctrl, sd, na.rm = TRUE),
  Control_CI_lower = sapply(averages_table$ctrl, quantile, probs = 0.025, na.rm = TRUE),
  Control_CI_upper = sapply(averages_table$ctrl, quantile, probs = 0.975, na.rm = TRUE)
)
summary_stats_control %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

# Plot the observed response vs. null expectation --------------------------------------------------------------------------------------

# Define y-axis limits based on data_to_view and period_to_view
ylimits_lookup <- list(
  "pitch_Night" = c(-60, 60),
  "pitch_Day" = c(-60, 60),
  "vedba_Night" = c(0, 0.3),
  "vedba_Day" = c(0, 0.3),
  "movement_Night" = c(0, 75),
  "movement_Day" = c(0, 75)
)
key <- paste(data_to_view, period_to_view, sep = "_") # Construct the key dynamically
ylimits <- ylimits_lookup[[key]] %||% c(NA, NA)  # Default to NA if the key is not found

# Create a data frame for plotting, which includes:
# - 'Distance': Repeated values from 'averages_table$dist' to match the length of control values.
# - 'VEDBA': Unlisted values from 'averages_table$ctrl' (control VEDBA values at each distance).

# Create the long-format data frame containing the null
plot_data <- data.frame(
  Intervals = rep(averages_table$dist, sapply(averages_table$ctrl, length)),
  Resp = unlist(averages_table$ctrl)
)

# Create a data frame for the observed responses per distance to overlay as points
avg_points <- data.frame(Period = period_to_view, Intervals = averages_table$dist, n = averages_table$n, 
                         Resp = averages_table$avg_value)

# Compute P
P_lower <- sapply(1:length(averages_table$dist), function(k)
  mean(averages_table$ctrl[[k]] <= averages_table$avg_value[k]))
P_higher <- sapply(1:length(averages_table$dist), function(k)
  mean(averages_table$ctrl[[k]] >= averages_table$avg_value[k]))
P_two_tailed <- 2 * pmin(P_lower, P_higher)
P_two_tailed <- pmin(P_two_tailed, 1) # Cap p-values at 1 (since 2*p can exceed 1)
comparison_results <- data.frame(
  Distance = averages_table$dist,
  P_Lower = P_lower,
  P_Higher = P_higher,
  P_two_Tailed = P_two_tailed
)
comparison_results  # Display formatted proportion comparisons

# Add P to the observed responses
avg_points <- avg_points %>%
  left_join(comparison_results, by = c("Intervals" = "Distance"))
avg_points <- avg_points %>% mutate(
  sig_higher = case_when(
    P_higher < 0.001 ~ "***",
    P_higher < 0.01  ~ "**",
    P_higher < 0.05  ~ "*",
    TRUE ~ ""
  ),
  sig_lower = case_when(
    P_lower < 0.001 ~ "***",
    P_lower < 0.01  ~ "**",
    P_lower < 0.05  ~ "*",
    TRUE ~ ""
  ),
  sig_two_tailed = case_when(
    P_two_tailed < 0.001 ~ "***",
    P_two_tailed < 0.01  ~ "**",
    P_two_tailed < 0.05  ~ "*",
    TRUE ~ ""
  )
)

# Plot

{
  # Pick if one tailed or two tailed (depending on the prediction)
  P_to_use <- dplyr::case_when(
    period_to_view == "Day"   &
      data_to_view == "vedba" ~ "P_two_tailed",
    period_to_view == "Day"   &
      data_to_view == "pitch" ~ "P_higher",
    period_to_view == "Day"   &
      data_to_view == "movement" ~ "P_two_tailed",
    period_to_view == "Night" &
      data_to_view == "vedba" ~ "P_higher",
    period_to_view == "Night" &
      data_to_view == "pitch" ~ "P_higher",
    period_to_view == "Night" &
      data_to_view == "movement" ~ "P_two_tailed",
    TRUE ~ NA_character_
  )
  sig_col <- sub("^P", "sig", P_to_use)
  
  y_name <- dplyr::case_when(
    data_to_view == "vedba" ~ "VeDBA (g)",
    data_to_view == "pitch" ~ "Pitch (°)",
    data_to_view == "movement" ~ "Displacement (m)",
    TRUE ~ NA_character_
  )
  
  panel_vertical_adjustment <- dplyr::case_when(period_to_view == "Day" ~ 1,
                                                period_to_view == "Night" ~ 0.9,
                                                TRUE ~ NA)
  
  panel_horizontal_adjustment <- dplyr::case_when(data_to_view == "pitch" ~ 0.5,
                                                  data_to_view == "vedba" ~ 0.595,
                                                  data_to_view == "movement" ~ 0.6,
                                                  TRUE ~ NA)
  
  p <- ggplot(plot_data, aes(x = factor(Intervals), y = Resp)) +
    # Null distribution
    geom_half_violin(
      side = "r",
      # 'r' for right side of the violin
      trim = FALSE,
      alpha = 0.4,
      color = NA,
      fill = "black"
    ) +
    # Observed responses
    geom_segment(
      data = avg_points,
      aes(
        x = as.numeric(factor(Intervals)),
        # Start at center of the violin
        xend = as.numeric(factor(Intervals)) + 0.4,
        # Extend slightly to the right
        y = Resp,
        yend = Resp
      ),
      color = "red",
      linewidth = .2
    ) + ylim(ylimits) +
    labs(x = "Distance (m)", y = y_name) +
    theme_bw() +
    # Showing significance symbols
    geom_text(
      data = avg_points,
      aes(
        x = as.numeric(factor(Intervals)),
        y = ylimits[1] + 0.80 * (ylimits[2] - ylimits[1]),
        label = get(sig_col)
      ),
      vjust = -0.5,
      size = 5
    ) + theme(legend.position = "none",
              axis.text.x = element_text(size = 5))
  print(p)
}

avg_points <- avg_points %>%
  left_join(summary_stats_control, by = c("Intervals" = "Intervals"))
avg_points %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

name <- paste0("fig_data_",
  tolower(period_to_view),
  "_",
  data_to_view
)
assign(name, list(null = plot_data, obs = avg_points)); 
save(list = name, file = paste0(out_dir, name, ".RData"))


### Plot (only after running all periods and metrics) #########

load(paste0(out_dir, "fig_data_night_pitch.RData"))
load(paste0(out_dir, "fig_data_day_pitch.RData"))
load(paste0(out_dir, "fig_data_night_vedba.RData"))
load(paste0(out_dir, "fig_data_day_vedba.RData"))
load(paste0(out_dir, "fig_data_night_movement.RData"))
load(paste0(out_dir, "fig_data_day_movement.RData"))

# Make a table containing the test results
obs_combined <- bind_rows(
  fig_data_day_pitch$obs %>% mutate(metric = "Pitch"),
  fig_data_night_pitch$obs %>% mutate(metric = "Pitch"),
  fig_data_day_vedba$obs %>% mutate(metric = "VeDBA"),
  fig_data_night_vedba$obs %>% mutate(metric = "VeDBA"),
  fig_data_day_movement$obs %>% mutate(metric = "Movement"),
  fig_data_night_movement$obs %>% mutate(metric = "Movement")
) %>%
  select(metric, everything()) %>%
  select(-any_of(c(
    "sig_higher",
    "sig_lower",
    "sig_two_tailed",
    "Control_SD"
  ))) 
obs_combined
write.csv(obs_combined, paste0(out_dir, "obs_combined.csv"), row.names = FALSE)

obs_width = 0.4
cols <- viridisLite::plasma(5)

ylimits <- c(-60,60)

p1 <- ggplot(fig_data_day_pitch$null, aes(x = factor(Intervals), y = Resp)) +
  # Null distribution
  geom_half_violin(aes(fill = Intervals), side = "r", trim = FALSE, 
                   alpha = 0.7,
                    color = NA
  ) + scale_fill_manual(
    values = c("350 - 250" = cols[2], "250 - 150" = cols[3],
      "150 - 50"  = cols[4],"≤ 50"     = cols[5],"50 - 150"  = cols[4],
      "150 - 250" = cols[3],"250 - 350" = cols[2]
    )
  )+
  # Observed responses
  geom_segment(data = fig_data_day_pitch$obs,
               aes(x = as.numeric(factor(Intervals)),
                   xend = as.numeric(factor(Intervals)) + 0.4,
                   y = Resp,
                   yend = Resp
               ),color = "red",linewidth = obs_width) + 
  ylim(ylimits) + labs(x = "Distance (m)", y = "Pitch (°)") +
  theme_bw() +
  geom_text(
    data = fig_data_day_pitch$obs,
    aes(
      x = as.numeric(factor(Intervals)),
      y = 30,
      label = get("sig_higher")
    ),
    vjust = -0.5,
    size = 5
  ) + theme(legend.position = "none",
            axis.text.x = element_text(size = 4))
p1

pp1 <- ggplot(fig_data_night_pitch$null, aes(x = factor(Intervals), y = Resp)) +
  # Null distribution
  geom_half_violin(aes(fill = Intervals), side = "r", trim = FALSE, 
                   alpha = 0.7,
                   color = NA
  ) + scale_fill_manual(
    values = c("350 - 250" = cols[2], "250 - 150" = cols[3],
               "150 - 50"  = cols[4],"≤ 50"     = cols[5],"50 - 150"  = cols[4],
               "150 - 250" = cols[3],"250 - 350" = cols[2]
    )
  )+
  # Observed responses
  geom_segment(data = fig_data_night_pitch$obs,
    aes(x = as.numeric(factor(Intervals)),
      xend = as.numeric(factor(Intervals)) + 0.4,
      y = Resp,
      yend = Resp
    ),color = "red",linewidth = obs_width) + 
  ylim(ylimits) + labs(x = "Distance (m)", y = "Pitch (°)") +
  theme_bw() +
  geom_text(
    data = fig_data_night_pitch$obs,
    aes(
      x = as.numeric(factor(Intervals)),
      y = 30,
      label = get("sig_higher")
    ),
    vjust = -0.5,
    size = 5
  ) + theme(legend.position = "none",
            axis.text.x = element_text(size = 4))
pp1

ylimits <- c(-0.01, 0.35)

p2 <- ggplot(fig_data_day_vedba$null, aes(x = factor(Intervals), y = Resp)) +
  # Null distribution
  geom_half_violin(aes(fill = Intervals), side = "r", trim = FALSE, 
                   alpha = 0.7,
                   color = NA
  ) + scale_fill_manual(
    values = c("350 - 250" = cols[2], "250 - 150" = cols[3],
               "150 - 50"  = cols[4],"≤ 50"     = cols[5],"50 - 150"  = cols[4],
               "150 - 250" = cols[3],"250 - 350" = cols[2]
    )
  )+
  # Observed responses
  geom_segment(data = fig_data_day_vedba$obs,
               aes(x = as.numeric(factor(Intervals)),
                   xend = as.numeric(factor(Intervals)) + 0.4,
                   y = Resp,
                   yend = Resp
               ),color = "red",linewidth = obs_width) + 
  ylim(ylimits) + labs(x = "Distance (m)", y = "VeDBA (g)") +
  theme_bw() +
  geom_text(
    data = fig_data_day_vedba$obs,
    aes(
      x = as.numeric(factor(Intervals)),
      y = 0.2,
      label = get("sig_two_tailed")
    ),
    vjust = -0.5,
    size = 5
  ) + theme(legend.position = "none",
            axis.text.x = element_text(size = 4))
p2

pp2 <- ggplot(fig_data_night_vedba$null, aes(x = factor(Intervals), y = Resp)) +
  # Null distribution
  geom_half_violin(aes(fill = Intervals), side = "r", trim = FALSE, 
                   alpha = 0.7,
                   color = NA
  ) + scale_fill_manual(
    values = c("350 - 250" = cols[2], "250 - 150" = cols[3],
               "150 - 50"  = cols[4],"≤ 50"     = cols[5],"50 - 150"  = cols[4],
               "150 - 250" = cols[3],"250 - 350" = cols[2]
    )
  )+
  # Observed responses
  geom_segment(data = fig_data_night_vedba$obs,
               aes(x = as.numeric(factor(Intervals)),
                   xend = as.numeric(factor(Intervals)) + 0.4,
                   y = Resp,
                   yend = Resp
               ),color = "red",linewidth = obs_width) + 
  ylim(ylimits) + labs(x = "Distance (m)", y = "VeDBA (g)") +
  theme_bw() +
  geom_text(
    data = fig_data_night_vedba$obs,
    aes(
      x = as.numeric(factor(Intervals)),
      y = 0.2,
      label = get("sig_higher")
    ),
    vjust = -0.5,
    size = 5
  ) + theme(legend.position = "none",
            axis.text.x = element_text(size = 4))

ylimits <- c(-10,100)

p3 <- ggplot(fig_data_day_movement$null, aes(x = factor(Intervals), y = Resp)) +
  # Null distribution
  geom_half_violin(aes(fill = Intervals), side = "r", trim = FALSE, 
                   alpha = 0.7,
                   color = NA
  ) + scale_fill_manual(
    values = c("350 - 250" = cols[2], "250 - 150" = cols[3],
               "150 - 50"  = cols[4],"≤ 50"     = cols[5],"50 - 150"  = cols[4],
               "150 - 250" = cols[3],"250 - 350" = cols[2]
    )
  )+
  # Observed responses
  geom_segment(data = fig_data_day_movement$obs,
               aes(x = as.numeric(factor(Intervals)),
                   xend = as.numeric(factor(Intervals)) + 0.4,
                   y = Resp,
                   yend = Resp
               ),color = "red",linewidth = obs_width) + 
  ylim(ylimits) + labs(x = "Distance (m)", y = "Mov. (m)") +
  theme_bw() +
  geom_text(
    data = fig_data_day_movement$obs,
    aes(
      x = as.numeric(factor(Intervals)),
      y = 60,
      label = get("sig_two_tailed")
    ),
    vjust = -0.5,
    size = 5
  ) + theme(legend.position = "none",
            axis.text.x = element_text(size = 4))
p3

pp3 <- ggplot(fig_data_night_movement$null, aes(x = factor(Intervals), y = Resp)) +
  # Null distribution
  geom_half_violin(aes(fill = Intervals), side = "r", trim = FALSE, 
                   alpha = 0.7,
                   color = NA
  ) + scale_fill_manual(
    values = c("350 - 250" = cols[2], "250 - 150" = cols[3],
               "150 - 50"  = cols[4],"≤ 50"     = cols[5],"50 - 150"  = cols[4],
               "150 - 250" = cols[3],"250 - 350" = cols[2]
    )
  )+
  # Observed responses
  geom_segment(data = fig_data_night_movement$obs,
               aes(x = as.numeric(factor(Intervals)),
                   xend = as.numeric(factor(Intervals)) + 0.4,
                   y = Resp,
                   yend = Resp
               ),color = "red",linewidth = obs_width) + 
  ylim(ylimits) + labs(x = "Distance (m)", y = "Mov. (m)") +
  theme_bw() +
  geom_text(
    data = fig_data_night_movement$obs,
    aes(
      x = as.numeric(factor(Intervals)),
      y = 60,
      label = get("sig_two_tailed")
    ),
    vjust = -0.5,
    size = 5
  ) + theme(legend.position = "none",
            axis.text.x = element_text(size = 4))
pp3

p1 <- p1 +
  theme(
    axis.text.x  = element_blank(),
    axis.title.x = element_blank(),
  )
pp1 <- pp1 +
  theme(
    axis.text.x  = element_blank(),
    axis.title.x = element_blank(),
  )
pp1 <- pp1 +
  theme(
    axis.text.y  = element_blank(),
    axis.title.y = element_blank(),
  )
p2 <- p2 +
  theme(
    axis.text.x  = element_blank(),
    axis.title.x = element_blank()
  )
pp2 <- pp2 +
  theme(
    axis.text.y  = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x  = element_blank(),
    axis.title.x = element_blank()
  )
pp3 <- pp3 +
  theme(
    axis.text.y  = element_blank(),
    axis.title.y = element_blank(),
  )



P<- (p1 | pp1) /
  (p2 | pp2) / (p3 | pp3) +
  plot_layout(
    widths  = c(1, 1,1),
    heights = c(1, 1,1)
  )
P

# Save if good
{
  ggsave(
    filename = paste0(
      fig_dir,
      "figure_Responses.pdf"
    ),
    plot     = P,
    device   = pdf,
    width    = 140,
    # in mm
    height   = 90,
    # in mm
    units    = "mm"
  )
  print(
    paste0(
      "Saved as ",
      fig_dir,
      fig_dir,
      "figure_Responses.pdf"
    )
  )
}
#### Visualize individual encounters ---------------------------------------------------------------------------

# Select the index of the encounter to visualize
i <- 33 # 33 -> S10 with F06

{
  set.seed(2)
  enc_to_see <- combined_values_filtered[combined_values_filtered$encounter == i, ]
  
  # Retrieve the individual ID (indA) and the year of the encounter
  curr.ind <- enc_to_see$indA[1]
  print(curr.ind)
  print(enc_to_see$indB[1])
  curr.year <- year(enc_to_see$start[1])
  
  # Load gps data (assume dry season analysis)
  load(paste0(gps_dir, "sifosa_", curr.year, "_dry_", "AM.Rdata"))
  
  # Find the corresponding index in the AM GPS data set for both individuals (sifaka and fosa) in the encounter
  indA <- gps$ids$idx[which(gps$ids$id_code == enc_to_see$indA[1])]
  indB <- gps$ids$idx[which(gps$ids$id_code == enc_to_see$indB[1])]
  
  # Load the corresponding ACC data for the individual and year (assume dry season analysis)
  curr.accdata <- read.csv(paste0(ACC_dir, curr.ind, '_', curr.year, '_dry_regular.csv'))
  
  # Convert timestamp column to POSIXct format to enable time-based analysis
  curr.accdata$timestamps <- as.POSIXct(curr.accdata$DT_adj,
                                        format = "%Y-%m-%d %H:%M:%S",
                                        tz = tz(gps$timestamps))
  
  # GPS time range of the encounter, including buffer time (time_range)
  time_range = minutes(9)
  gpsdata.range <- gps$timestamps >= enc_to_see$start[1] - time_range &
    gps$timestamps <= enc_to_see$end[nrow(enc_to_see)] + time_range
  
  # ACC data time range of the encounter, including buffer time (time_range)
  accdata_range <- curr.accdata$timestamps >= enc_to_see$start[1] - time_range &
    curr.accdata$timestamps <=  enc_to_see$end[nrow(enc_to_see)] + time_range
  
  
  # Select one non-encounter day
  non_enc_to_see_date <- sample(enc_to_see$non_encounter_dates[[1]], 1)
  
  
  # Build start and end corresponding to each distance intervals on the encounter day for the non encounter day
  
  tz <- attr(enc_to_see$start, "tzone") # Time zone
  
  # seconds since midnight
  sec <- function(x) {
    tt <- strptime(format(x, "%H:%M:%S"), "%H:%M:%S")
    tt$hour * 3600 + tt$min * 60 + tt$sec
  }
  s_sec <- sec(enc_to_see$start)
  e_sec <- sec(enc_to_see$end)
  
  d_start <- s_sec < s_sec[1]
  d_end   <- d_start + (e_sec < s_sec)
  
  enc_to_see$non_enc_start <- as.POSIXct(paste(
    non_enc_to_see_date + d_start,
    format(enc_to_see$start, "%H:%M:%S")
  ), tz = tz)
  enc_to_see$non_enc_end <- as.POSIXct(paste(
    non_enc_to_see_date + d_end,
    format(enc_to_see$end, "%H:%M:%S")
  ), tz = tz)
  
  # GPS time range of the non-encounter, including buffer time (time_range)
  non_enc_gpsdata.range <- gps$timestamps >= enc_to_see$non_enc_start[1] -
    time_range &
    gps$timestamps <= enc_to_see$non_enc_end[nrow(enc_to_see)] + time_range
  
  # ACC data time range of the non-encounter, including buffer time (time_range)
  non_enc_accdata_range <- curr.accdata$timestamps >= enc_to_see$non_enc_start[1] -
    time_range &
    curr.accdata$timestamps <=  enc_to_see$non_enc_end[nrow(enc_to_see)] +
    time_range
  
  # Set common x-axis limits
  common_xlim <- range(c(gps$timestamps[gpsdata.range], curr.accdata$timestamps[accdata_range]))
  
  
  # Get behavioral average distance interval + midpoint
  pitch_df <- data.frame(time  = curr.accdata$timestamps[accdata_range], pitch = curr.accdata$pitch[accdata_range])
  non_enc_pitch_df <- data.frame(time  = curr.accdata$timestamps[non_enc_accdata_range],
                                 non_enc_pitch = curr.accdata$pitch[non_enc_accdata_range])
  vedba_df <- data.frame(time  = curr.accdata$timestamps[accdata_range], vedba = curr.accdata$vedba[accdata_range])
  non_enc_vedba_df <- data.frame(time  = curr.accdata$timestamps[non_enc_accdata_range],
                                 non_enc_vedba = curr.accdata$vedba[non_enc_accdata_range])
  
  interval_pitch <- transform(
    enc_to_see,
    mid_time   = start + (end - start) / 2,
    mean_pitch = vapply(seq_len(nrow(enc_to_see)), function(i) {
      idx <- pitch_df$time >= enc_to_see$start[i] &
        pitch_df$time <= enc_to_see$end[i]
      mean(pitch_df$pitch[idx], na.rm = TRUE)
    }, numeric(1)),
    non_enc_mean_pitch = vapply(seq_len(nrow(enc_to_see)), function(i) {
      idx <- non_enc_pitch_df$time >= enc_to_see$non_enc_start[i] &
        non_enc_pitch_df$time <= enc_to_see$non_enc_end[i]
      mean(non_enc_pitch_df$non_enc_pitch[idx], na.rm = TRUE)
    }, numeric(1))
  )
  
  interval_vedba <- transform(
    enc_to_see,
    mid_time   = start + (end - start) / 2,
    mean_vedba = vapply(seq_len(nrow(enc_to_see)), function(i) {
      idx <- vedba_df$time >= enc_to_see$start[i] &
        vedba_df$time <= enc_to_see$end[i]
      mean(vedba_df$vedba[idx], na.rm = TRUE)
    }, numeric(1)),
    non_enc_mean_vedba = vapply(seq_len(nrow(enc_to_see)), function(i) {
      idx <- non_enc_vedba_df$time >= enc_to_see$non_enc_start[i] &
        non_enc_vedba_df$time <= enc_to_see$non_enc_end[i]
      mean(non_enc_vedba_df$non_enc_vedba[idx], na.rm = TRUE)
    }, numeric(1))
  )
  
  distance_df <- data.frame(time = gps$timestamps[gpsdata.range], distance = ddd[indA, indB, gpsdata.range])
  
  gpsdata.range_extended <- gpsdata.range
  idx <- which(gpsdata.range == 1)
  gpsdata.range_extended[min(idx) - 1] <- T
  gpsdata.range_extended[max(idx) + 1] <- T
  movement_df <- data.frame(time = gps$timestamps[gpsdata.range_extended], xs = gps$xs[indA,gpsdata.range_extended], 
                            ys = gps$ys[indA,gpsdata.range_extended])
  movement_df <- movement_df |>
    dplyr::mutate(
      movement = sqrt((xs - lag(xs))^2 + (ys - lag(ys))^2)
    )
  
  # non-encounter movement
  gpsdata.range_extended <- non_enc_gpsdata.range
  idx <- which(non_enc_gpsdata.range == 1)
  gpsdata.range_extended[min(idx) - 1] <- T
  gpsdata.range_extended[max(idx) + 1] <- T
  non_enc_movement_df <- data.frame(time = gps$timestamps[gpsdata.range_extended], xs = gps$xs[indA,gpsdata.range_extended], 
                            ys = gps$ys[indA,gpsdata.range_extended])
  non_enc_movement_df <- non_enc_movement_df |>
    dplyr::mutate(
      movement = sqrt((xs - lag(xs))^2 + (ys - lag(ys))^2)
    )
  year(non_enc_movement_df$time)  <- year(movement_df$time[1])
  month(non_enc_movement_df$time) <- month(movement_df$time[1])
  mday(non_enc_movement_df$time)  <- mday(movement_df$time[1])
  
  cross_times <- function(df, thresholds = c(350, 250, 150, 50)) {
    # keep finite values
    ok <- !is.na(df$time) & !is.na(df$distance)
    df <- df[ok, ]
    if (nrow(df) < 2)
      return(data.frame())
    
    t_num <- as.numeric(df$time)  # seconds since epoch
    y <- df$distance
    
    out <- lapply(thresholds, function(th) {
      y1 <- y[-length(y)]
      y2 <- y[-1]
      t1 <- t_num[-length(t_num)]
      t2 <- t_num[-1]
      
      # crossing occurs if threshold is between y1 and y2 (inclusive), excluding flat segments
      crosses <- (y1 - th) * (y2 - th) <= 0 & (y2 != y1)
      
      if (!any(crosses))
        return(NULL)
      
      # linear interpolation fraction
      frac <- (th - y1[crosses]) / (y2[crosses] - y1[crosses])
      t_cross <- t1[crosses] + frac * (t2[crosses] - t1[crosses])
      
      direction <- ifelse(y2[crosses] > y1[crosses], "up", "down")
      
      data.frame(
        threshold = th,
        time = as.POSIXct(
          t_cross,
          origin = "1970-01-01",
          tz = attr(df$time, "tzone")
        ),
        direction = direction,
        stringsAsFactors = FALSE
      )
    })
    
    res <- do.call(rbind, out)
    if (is.null(res) || nrow(res) == 0)
      return(data.frame())
    res[order(res$time, res$threshold), ]
  }
  
  crossings <- cross_times(distance_df, c(350, 250, 150, 50))
  crossings <- crossings %>%
    group_by(threshold, direction) %>%
    filter(
      (direction == "down" & row_number() == 1) |
        (direction == "up" & row_number() == n())
    ) %>%
    ungroup()
  
  # Plot pitch over time
  p1 <- ggplot() +
    geom_line(
      data = data.frame(
        time  = curr.accdata$timestamps[non_enc_accdata_range] - (
          as.Date(curr.accdata$timestamps[non_enc_accdata_range][1]) - as.Date(curr.accdata$timestamps[accdata_range][1])
        ),
        pitch = curr.accdata$pitch[non_enc_accdata_range]
      ),
      aes(x = time, y = pitch),
      color = "black", linewidth = .5, alpha = .4, linetype = "dashed"
    ) + geom_line(
      data = data.frame(time  = curr.accdata$timestamps[accdata_range], pitch = curr.accdata$pitch[accdata_range]),
      aes(x = time, y = pitch),
      color = "red", linewidth = .5, alpha = .5
    ) +
    # geom_segment(
    #   data = interval_pitch,
    #   aes(
    #     x = start,
    #     xend = end,
    #     y = non_enc_mean_pitch,
    #     yend = non_enc_mean_pitch
    #   ),
    #   color = "gray70",
    #   linewidth = 1,
    #   alpha = .5
    # ) + geom_segment(
    #   data = interval_pitch,
    #   aes(
    #     x = start,
    #     xend = end,
    #     y = mean_pitch,
    #     yend = mean_pitch
    #   ),
    #   color = "black",
    #   linewidth = 1,
    #   alpha = .5
    # ) +
    # geom_vline(
    #   xintercept = c(enc_to_see$start, tail(enc_to_see$end, 1)),
    #   linetype = "dashed",
    #   color = "gray"
    # )+
    geom_vline(
      xintercept = crossings$time,
      linetype = "dashed",
      linewidth = .2,
      color = "black", alpha = 1
    ) +
    coord_cartesian(xlim = common_xlim,
                    ylim = c(-80, 80),
                    expand = FALSE) +
    labs(x = "Time", y = "Pitch (°)") +
    theme_bw() + theme(axis.title.x = element_blank(), axis.text.x  = element_blank())
  
  # Plot vedba over time
  p2 <- ggplot() +
    geom_line(
      data = data.frame(
        time  = curr.accdata$timestamps[non_enc_accdata_range] - (
          as.Date(curr.accdata$timestamps[non_enc_accdata_range][1]) - as.Date(curr.accdata$timestamps[accdata_range][1])
        ),
        vedba = curr.accdata$vedba[non_enc_accdata_range]
      ),
      aes(x = time, y = vedba),
      color = "black", linewidth = .5, alpha = .4,linetype = "dashed"
    ) + geom_line(
      data = data.frame(time  = curr.accdata$timestamps[accdata_range], vedba = curr.accdata$vedba[accdata_range]),
      aes(x = time, y = vedba),
      color = "red", linewidth = .5, alpha = .5
    ) +
    # geom_segment(
    #   data = interval_vedba,
    #   aes(
    #     x = start,
    #     xend = end,
    #     y = non_enc_mean_vedba,
    #     yend = non_enc_mean_vedba
    #   ),
    #   color = "gray70",
    #   linewidth = 1,
    #   alpha = .5
    # ) + geom_segment(
    #   data = interval_vedba,
    #   aes(
    #     x = start,
    #     xend = end,
    #     y = mean_vedba,
    #     yend = mean_vedba
    #   ),
    #   color = "black",
    #   linewidth = 1,
    #   alpha = .5
    # ) +
    # geom_vline(
    #   xintercept = c(enc_to_see$start, tail(enc_to_see$end, 1)),
    #   linetype = "dashed",
    #   color = "gray"
    # )+
    geom_vline(
      xintercept = crossings$time,
      linetype = "dashed",
      linewidth = .2,
      color = "black", alpha = 1
    ) +
    coord_cartesian(xlim = common_xlim,
                    ylim = c(-0.02, 0.4),
                    expand = FALSE) +
    labs(x = "Time", y = "VeDBA (g)") +
    theme_bw() + theme(axis.title.x = element_blank(), axis.text.x  = element_blank())
  #plot movement over time
  p3 <- ggplot() +
    geom_line(
      data = non_enc_movement_df, aes(x = time, y = movement),
      color = "black", linewidth = .5, alpha = .4,linetype = "dashed"
    )+
    geom_line(data = movement_df, aes(x = time, y = movement), 
              color = "red", linewidth = .5, alpha = .5) +
    geom_vline(
      xintercept = crossings$time,
      linetype = "dashed",
      linewidth = .2,
      color = "black", alpha = 1
    ) +
    labs(x = "Time", y = "Mov. (m)") + 
    coord_cartesian(xlim = common_xlim,
                    ylim = c(-1, 25),
                    expand = FALSE) +
    theme_bw()+theme(axis.text.x = element_text(size = 4))
  p3
  # Plot distance over time
  
  cols <- viridisLite::plasma(5)
  
  plot_df <- dplyr::bind_rows(
    distance_df,
    crossings |>
      dplyr::transmute(
        time = time,
        distance = threshold
      )
  ) |>
    dplyr::arrange(time)

  
  interval_cols <- c(
    ">350"    = cols[1],
    "350-250" = cols[2],
    "250-150" = cols[3],
    "150-50"  = cols[4],
    "≤50"     = cols[5],
    "50-150"  = cols[4],
    "150-250" = cols[3],
    "250-350" = cols[2],
    ">350"    = cols[1]
  )
  
  
  thresholds <- crossings$threshold
  down <- crossings$threshold[crossings$direction == "down"]
  up   <- crossings$threshold[crossings$direction == "up"]
  labels <- c(
    if (350 %in% down) ">350",
    if (250 %in% down) "350-250",
    if (150 %in% down)  "250-150",
    if (50 %in% down)  "150-50",
    if (50 %in% up)    "≤50",
    if (150 %in% up)   "50-150",
    if (250 %in% up)   "150-250",
    if (350 %in% up)   "250-350",
    if (350 %in% up)   ">350"
  )
  
  breaks <- crossings$time
  
  plot_df$interval <- labels[
    findInterval(plot_df$time, crossings$time) + 1
  ]
  
  plot_df <- plot_df |>
    dplyr::mutate(
      time_next = dplyr::lead(time),
      distance_next = dplyr::lead(distance)
    )
  
  p4 <- ggplot(plot_df) +
    geom_segment(
      aes(
        x = time,
        y = distance,
        xend = time_next,
        yend = distance_next,
        color = interval
      ),
      linewidth = 1,
      alpha = .5
    ) +
    geom_vline(
      xintercept = crossings$time,
      linewidth = .2,
      color = "black",
      alpha = 1, linetype = "dashed"
    ) +
    coord_cartesian(
      xlim = common_xlim,
      ylim = c(0, 500),
      expand = FALSE
    ) +
    labs(x = "Time", y = "Distance (m)") +
    theme_bw() +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),legend.position = "none"
    )+scale_color_manual(
      values = c(
        ">350"   = cols[1],
        "350-250" = cols[2],
        "250-150" = cols[3],
        "150-50"  = cols[4],
        "≤50"     = cols[5],
        "50-150"  = cols[4],
        "150-250" = cols[3],
        "250-350" = cols[2]
      ))
  p4
  # Combine plots
  PP <- p4 / p1 / p2 / p3 + plot_layout(heights = c(1, 1, 1,1))
  print(PP)
  print(non_enc_to_see_date)
}

PPP<- (PP | P) +
  plot_layout(widths = c(1, 2))
PPP

# Save if good
ggsave(
  filename = paste0(
    fig_dir,"immediate_responses.pdf"
  ),
  plot     = PPP,
  device   = pdf,
  width    = 240,
  # in mm
  height   = 90,
  # in mm
  units    = "mm"
)

