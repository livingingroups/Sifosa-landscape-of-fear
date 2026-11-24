#' Function: f_compute_day_night_abv_ths.R
#' 
#' calculate the proportion of datapoints exceeding a given threshold
#' per day and per night of a given metric (vector).
#' The default threshold is mean+sd for both day and night
#' 
#' @author Maevatiana N. Ratsimbazafindranahaka (primary code author)
#' @author Meredith C. Lutz (code reviewer)

library(dplyr)
library(ggplot2)

f_compute_day_night_abv_ths <- function(timestamps, metric, sun_times, th_day = NA, th_night = NA) {
  # Create a dataframe linking timestamps & metric
  data <- data.frame(
    date = as.Date(timestamps),  # Extract date
    timestamps = timestamps, 
    metric = metric
  )
  
  required_samples = as.numeric(hours(9), units = "seconds")/as.numeric(mean(diff(timestamps), na.rm = TRUE), units = "secs") # minimum 9 hours 
  
  
  # --- Daytime Data ---
  
  # # Ensure sun_times has Date and time as POSIXct
  # sun_times <- sun_times %>%
  #   mutate(sunrise = as.POSIXct(sunrise, tz = tz(timestamps)),
  #          sunset  = as.POSIXct(sunset,  tz = tz(timestamps)))
  
  data <- data %>%
    left_join(sun_times, by = "date")
  
  # Join by date and filter by time of day
  data_filtered_day <- data %>%
    filter(timestamps >= sunrise & timestamps <= sunset)
  
  if (is.na(th_day)) {th_day = mean(data_filtered_day$metric)+sd(data_filtered_day$metric)}
  
  day_table <- data_filtered_day %>%
    group_by(date) %>%
    summarise(
      metric_daytime = mean(metric > th_day, na.rm = TRUE), n = n(),
      n_non_na = sum(!is.na(metric))
    )
  day_table$metric_daytime[day_table$n_non_na < required_samples] = NA
  day_table <- day_table %>%
    select(-n, -n_non_na)
  
  # --- Nighttime Data ---
  data$night_date <- data$date
  night_data <- data %>%
    mutate(night_date = as.Date(ifelse(timestamps >= sunset, 
                                       date,  # Group under same date if after sunset
                                       date - 1)))  # Group under the previous day if before sunrise
  
  # Join by date and filter by time of day
  data_filtered_night <- night_data %>%
    filter(format(timestamps, "%H:%M:%S") >= format(sunset, "%H:%M:%S") | 
             format(timestamps, "%H:%M:%S") <= format(sunrise, "%H:%M:%S"))
  
  
  if (is.na(th_night)) {th_night = mean(data_filtered_night$metric)+sd(data_filtered_night$metric)}

  night_table <- data_filtered_night %>%
    group_by(night_date) %>%
    summarise(
      metric_nighttime = mean(metric > th_night, na.rm = TRUE), n = n(),
                              n_non_na = sum(!is.na(metric))
      ) %>%
    rename(date = night_date) # Rename to "date" for merging
    night_table$metric_nighttime[night_table$n_non_na < required_samples] = NA
    night_table <- night_table %>%
      select(-n, -n_non_na)
  
  # --- Combine Day & Night Data ---
  combined_table <- full_join(day_table, night_table, by = c("date"))
  
  # --- Reshape Data for ggplot (long format) ---
  plot_data <- combined_table %>%
    pivot_longer(cols = c("metric_daytime", "metric_nighttime"), 
                 names_to = "day_night", 
                 values_to = "metric") %>%
    mutate(day_night = recode(day_night,
                              metric_daytime = "day",
                              metric_nighttime = "night")
    )
  
  # --- Plot with ggplot2 ---
  p <- ggplot(plot_data, aes(x = date, y = metric, color = day_night, group = day_night)) +
    geom_line(size = 1.2, alpha = 0.8) +  # Transparent lines
    geom_point(size = 2, alpha = 0.8) +  # Transparent lines
    labs(title = "Day & Night", x = "Date", y = "Metric") +
    scale_color_manual(values = c("day" = "blue", "night" = "red"),
                       labels = c("Daytime", "Nighttime")) +
    theme_bw() +
    theme(legend.title = element_blank())
  print(p)
  
  return(plot_data)
}

