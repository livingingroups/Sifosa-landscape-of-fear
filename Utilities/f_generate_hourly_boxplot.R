#' Function: f_generate_hourly_boxplot.R
#' 
#' Generate and display hourly summary of a metric  
#' 
#' @author Maevatiana N. Ratsimbazafindranahaka (primary code author)
#' @author Meredith C. Lutz (code reviewer)

library(ggplot2)

f_generate_hourly_boxplot <- function(timestamps, data, sun_times, title_string = NULL, data_name = NULL) {
  # Create dataframe with date, hour, and distance
  if (mean(is.na(data)) == 1) {
    message("empty data provided")
    return(NULL)  
  }
  
  df <- data.frame(
    date = as.Date(timestamps),  # Extract date
    hour = format(timestamps, "%H"),  # Extract hour (00-23)
    data = data
  )
  df$hour <- as.numeric(df$hour)  # Convert hour to numeric
  
  # Generate sunrise and sunset times in hours (decimal)
  sunrise_times <- sun_times$sunrise_dec
  sunset_times <- sun_times$sunset_dec
  
  if (length(sunrise_times) > 1) {
    sunrise_time = mean(head(sunrise_times,1),tail(sunrise_times,1)) 
    sunset_time = mean(head(sunset_times,1),tail(sunset_times,1)) 
  }
  
  # Calculate **daily** hourly averages
  hourly_avg <- aggregate(data ~ date + hour, data = df, FUN = mean)
  
  # Generate the boxplot
  p <- ggplot(hourly_avg, aes(x = hour + 0.5, y = data, group = hour)) +  # Shift boxplots slightly
    geom_boxplot(fill = "lightblue", color = "darkblue", width = 0.9) +  
    labs(x = "Hour of Day", y = data_name) +
    theme_bw() +
    # Add shaded night-time areas
    ggplot2::annotate("rect", xmin = -Inf, xmax = sunrise_time, ymin = -Inf, ymax = Inf, 
             fill = "black", alpha = 0.2) +  # Before sunrise
    ggplot2::annotate("rect", xmin = sunset_time, xmax = Inf, ymin = -Inf, ymax = Inf, 
             fill = "black", alpha = 0.2) +  # After sunset
    scale_x_continuous(breaks = seq(min(hourly_avg$hour), max(hourly_avg$hour), by = 1))
  
  # Add title only if provided
  if (!is.null(title_string)) {
    p <- p + labs(title = title_string)
  }
  
  # Print the plot
  print(p)
  
  # Return the computed hourly averages
  return(hourly_avg)
}
