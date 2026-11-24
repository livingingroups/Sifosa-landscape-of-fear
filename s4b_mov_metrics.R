#'----------------------------------------------------------------------------------------------------
#' Script: s4b_mov_metrics.R
#'
#' Quantify daily and nightly movements
#'
#' @author Tracy Montgomery (primary code author)
#' @author Maevatiana Ratsimbazafindranahaka (code reviewer)
#-----------------------------------------------------------------------------------------------------

rm(list = ls())

# Load libraries
library(tidyverse)

# List and source all useful R functions -------------------------------------------------------------------
files <- list.files("C:/Users/maevatiana.ratsim/Documents/Projects/Dev/Sifosa-landscape-of-fear/Utilities/", pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
lapply(files, source)


##### Calculate daily/nightly path length #####

##### 1: Set up workspace #####

# Set options
options(stringsAsFactors = F)
options(digits.secs = 3)

time_zone = "Indian/Antananarivo"

# Set directories
wdir <- "C:/Users/maevatiana.ratsim/Documents/Projects/Sifaka/Sifosa-landscape-of-fear-datasets/"
setwd(wdir)
gps.data.dir <- "GPS/"
processed.data.dir <- "s4b/"
plots.dir <- "s4b/plots/"

season <- "dry"

# Load functions
dist_btw_2pts <- function(X1, Y1, X2, Y2, digits = 2) {
  return( round(
    sqrt(
      ( (X1 - X2) ^ 2) +
        ( (Y1 - Y2) ^ 2)
    ) , digits = digits)
  )
}

# Load data
sifaka.22.3min <- fread(paste0(gps.data.dir, "sifaka.22.3min.csv"))
sifaka.23.3min <- fread(paste0(gps.data.dir, "sifaka.23.3min.csv"))

# Fix timezone
message(sprintf(
  "The current local time (e.g., first row = %s) was loaded assuming that it is %s. It will now be forced to be %s without changing the values.",
  sifaka.22.3min$study_local_timestamp[1], tz(sifaka.22.3min$study_local_timestamp[1]), time_zone))
sifaka.22.3min$study_local_timestamp <- force_tz(sifaka.22.3min$study_local_timestamp, tzone = time_zone)
sifaka.23.3min$study_local_timestamp <- force_tz(sifaka.23.3min$study_local_timestamp, tzone = time_zone)
head(sifaka.22.3min$study_local_timestamp) # first timestamp should be “2022-06-24 04:00:00 EAT” 
head(sifaka.23.3min$study_local_timestamp) # first timestamp should be “2023-06-25 03:00:00 EAT”

sifaka.22.3min$date <- as.Date(sifaka.22.3min$study_local_timestamp)
sifaka.23.3min$date <- as.Date(sifaka.23.3min$study_local_timestamp)

sifaka_all_dates <- c(min(sifaka.22.3min$date)-1,sifaka.22.3min$date, min(sifaka.23.3min$date)-1,sifaka.23.3min$date)

sun_times <- f_get_suntimes(
  sifaka_all_dates,
  lat = -20.783,
  lon = 44.174,
  tz = "Indian/Antananarivo")

sun_times <- sun_times %>%
  mutate(sunrise = as.POSIXct(sunrise, tz = "Indian/Antananarivo"),
         sunset  = as.POSIXct(sunset, tz = "Indian/Antananarivo"))


##### 2: Create day/night dataframes #####

# Create base day/night dataframe
day_night_df <- sun_times %>%
  mutate(next_sunrise = lead(sunrise)) %>%
  transmute(
    date,
    day_start = sunrise,
    day_end = sunset,
    night_start = sunset,
    night_end = next_sunrise
  ) %>%
  pivot_longer(
    cols = -date,
    names_to = c("day_night", ".value"),
    names_pattern = "(day|night)_(start|end)"
  ) %>%
  rename(start_time = start, end_time = end) %>%
  filter(!is.na(end_time))

# Add in season
day_night_df <- day_night_df %>%
  mutate(
    season = case_when(
      format(date, "%Y-%m") %in% c("2022-06","2022-07") ~ "sifaka.22",
      format(date, "%Y-%m") %in% c("2023-06","2023-07") ~ "sifaka.23"
    )
  )

# Calculate number of points per period
day_night_df$num_pts_total <- NA
for (i in 1:nrow(day_night_df)) {
  start.i <- day_night_df$start_time[i]
  end.i <- day_night_df$end_time[i]
  if (grepl("2022-06", day_night_df$date[i]) | grepl("2022-07", day_night_df$date[i])) {
    day_night_df$num_pts_total[i] <-
      nrow(filter(sifaka.22.3min, study_local_timestamp > start.i & study_local_timestamp < end.i))
  }
  if (grepl("2023-06", day_night_df$date[i]) | grepl("2023-07", day_night_df$date[i])) {
    day_night_df$num_pts_total[i] <-
      nrow(filter(sifaka.23.3min, study_local_timestamp > start.i & study_local_timestamp < end.i))
  }
  rm(start.i, end.i)
}

# Subset to periods with points
day_night_df <- filter(day_night_df, num_pts_total > 0)
day_night_df$num_pts_total <- NULL # remove column

# Separate by season and add in animals
add_animals <- function(intervals, animals){
  tidyr::crossing(intervals, animal_id = animals)
}

daynight.22 <- add_animals(filter(day_night_df, season == "sifaka.22"),
                           unique(sifaka.22.3min$individual_local_identifier))

daynight.23 <- add_animals(filter(day_night_df, season == "sifaka.23"),
                           unique(sifaka.23.3min$individual_local_identifier))

##### 3: Calculate step lengths and turning angles #####

calc_path_metrics <- function(df,
                              id_col = "individual_local_identifier",
                              time_col = "study_local_timestamp") {

  df %>%
    # Arrange by id and time
    arrange(.data[[id_col]], .data[[time_col]]) %>%

    # Group by id
    group_by(.data[[id_col]]) %>%
    mutate(

      # Calculate step length for smoothed coordinates
      step_length = sqrt(
        (utm_easting_smooth - lag(utm_easting_smooth))^2 +
          (utm_northing_smooth - lag(utm_northing_smooth))^2
      ),

      # Calculate turning angle for smoothed coordinates
      turning_angle = atan2(
        (utm_easting_smooth - lag(utm_easting_smooth)) * (lead(utm_northing_smooth) - utm_northing_smooth) -
          (utm_northing_smooth - lag(utm_northing_smooth)) * (lead(utm_easting_smooth) - utm_easting_smooth),

        (utm_easting_smooth - lag(utm_easting_smooth)) * (lead(utm_easting_smooth) - utm_easting_smooth) +
          (utm_northing_smooth - lag(utm_northing_smooth)) * (lead(utm_northing_smooth) - utm_northing_smooth)
      ),

    ) %>%
    ungroup()
}

sifaka.22.3min <- calc_path_metrics(sifaka.22.3min, id_col = "individual_local_identifier",
                                    time_col = "study_local_timestamp")
quantile(sifaka.22.3min$step_length, probs = seq(0.9,1, 0.01), na.rm = T)
mean(sifaka.22.3min$turning_angle, na.rm = T)
#[1] 0.05562648

sifaka.23.3min <- calc_path_metrics(sifaka.23.3min, id_col = "individual_local_identifier",
                                    time_col = "study_local_timestamp")


##### 4: Calculate daily/nightly path length using raw and smoothed data #####

# R ≈ 1 → straight, directed movement that day
# R ≈ 0 → highly tortuous / random movement

calc_daily_metric <- function(interval_df, movement_df) {

  interval_df %>%
    rowwise() %>%
    mutate(
      path_length = sum(
        movement_df$step_length[
          movement_df$individual_local_identifier == animal_id &
            movement_df$study_local_timestamp > start_time &
            movement_df$study_local_timestamp <= end_time
        ],
        na.rm = TRUE
      ),

      straightness = mean(cos(movement_df$turning_angle[
        movement_df$individual_local_identifier == animal_id &
          movement_df$study_local_timestamp > start_time &
          movement_df$study_local_timestamp <= end_time
      ]), na.rm = TRUE
      ),

      metric_pts = sum(
        !is.na(
          movement_df$step_length[
            movement_df$individual_local_identifier == animal_id &
              movement_df$study_local_timestamp > start_time &
              movement_df$study_local_timestamp <= end_time
          ]
        )
      ),

    ) %>%
    ungroup()
}

daynight.22 <- calc_daily_metric(daynight.22, sifaka.22.3min)
daynight.23 <- calc_daily_metric(daynight.23, sifaka.23.3min)

# Look at maximum number of points (20 pts/hr) --> require 200 points
max(daynight.22[daynight.22$day_night == "day",]$metric_pts) # 219
max(daynight.23[daynight.23$day_night == "day",]$metric_pts) # 221

# Require 180 points (9 hrs) (20 pts/hour)
nrow(daynight.22)
daynight.22 <- filter(daynight.22, metric_pts >= 180) # remove 109
nrow(daynight.22)
nrow(daynight.23)
daynight.23 <- filter(daynight.23, metric_pts >= 180) # remove 188
nrow(daynight.23)


##### 5: Save data #####

save(list = c("daynight.22", "daynight.23"),
    file = paste0(processed.data.dir, "sifaka_day_night_mov.RData"))

write.csv(daynight.22, file = paste0(processed.data.dir, "sifaka_day_night_mov_2022_dry.csv"), row.names = FALSE)
write.csv(daynight.23, file = paste0(processed.data.dir, "sifaka_day_night_mov_2023_dry.csv"), row.names = FALSE)
