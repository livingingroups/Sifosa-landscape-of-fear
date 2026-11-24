#' Function: f_compute_day_night_avg.R
#' 
#' Plot day/night metric per group
#' 
#' 
#' @author Maevatiana N. Ratsimbazafindranahaka (primary code author)
#' @author Meredith C. Lutz (code reviewer)
#'
#' @param data A data frame containing date, group_id, and metric columns
#' @param metric_name Name of the metric (e.g., "pitch", etc.)
#' @param period Period of day (e.g., "day" or "night")
#' @param y_limits Optional: y-axis limits (e.g., c(0, 50))
#'
#' @return A ggplot object


library(ggplot2)
library(dplyr)

f_plot_day_night_metric <- function(data, metric_name, period, y_limits = NULL) {
  # Column name for the y-axis metric (e.g., "pitch_daytime")
  data <- data %>%
    filter(day_night == period)
  y_col = metric_name

  
  # Prepare base plot
  p <- ggplot(data, aes(x = date, y = .data[[y_col]])) +
    stat_summary(
      fun = mean,
      geom = "line",
      color = "black",
      size = 1,
      alpha = 0.5,
      aes(group = 1)
    ) +
    stat_summary(
      fun.data = mean_sdl,
      fun.args = list(mult = 1),
      geom = "ribbon",
      alpha = 0.5,
      fill = "gray",
      inherit.aes = FALSE,
      mapping = aes(x = date, y = .data[[y_col]], group = 1)
    ) +
    geom_line(aes(group = animal_id, color = as.factor(group_id)))
  
  # Add labels, theme, etc.
  p <- p + labs(
    title = paste0(toupper(substring(metric_name, 1, 1)), substring(metric_name, 2), " ", period, " per Group"),
    x = "Date",
    y = paste(toupper(metric_name), period),
    color = "Group ID",
    fill = "Standard Deviation"
  ) + theme_bw()
  
  # Optional y-axis limits
  if (!is.null(y_limits)) {
    p <- p + coord_cartesian(ylim = y_limits)
  }
  
  return(p)
}



