#' Function: f_get_suntimes.R
#' 
##' Compute Sunrise and Sunset Times for a Set of Timestamps
#'
#' Compute sunrise and sunset times for each unique date represented in a
#' vector of POSIXct timestamps. The function uses geographic coordinates and a
#' specified time zone to retrieve solar times via the suncalc package.
#'
#' @author Maevatiana N. Ratsimbazafindranahaka (primary code author)
#' @author Damien Caillaud (code reviewer)
#'
#' @param timestamps A vector of POSIXct timestamps (for example from GPS data).
#' @param lat Numeric. Latitude of the location in decimal degrees (for example -20.783).
#' @param lon Numeric. Longitude of the location in decimal degrees (for example 44.174).
#' @param tz Character. Time zone in Olson format (for example "Indian/Antananarivo").
#'
#' @return A data frame with columns `date`, `sunrise`, `sunset`,
#' `sunrise_decimal`, and `sunset_decimal`. The `sunrise` and `sunset` values are
#' character strings and the decimal columns represent the corresponding times
#' expressed in hours.
#'
#' @examples
#' sun_times <- f_get_suntimes(
#'   gps$timestamps,
#'   lat = -20.783,
#'   lon = 44.174,
#'   tz = "Indian/Antananarivo"
#' )

f_get_suntimes <- function(timestamps, lat, lon, tz) {
  
  # Ensure required packages are loaded
  require(tidyverse)
  require(lubridate)
  require(suncalc)
  
  # Prepare Unique Dates
  # Convert timestamps to Date format and extract unique dates
  unique_dates <- unique(as.Date(timestamps))
  unique_dates <- sort(unique_dates)
  unique_dates <- c(unique_dates[1]-1, unique_dates, tail(unique_dates,1)+1)
  
  # Initialize Data Frame
  # Create an empty data frame to store sunrise and sunset times
  sun.times <- data.frame(
    date = unique_dates,
    sunrise = NA_character_,  # Placeholder for sunrise times
    sunset  = NA_character_,   # Placeholder for sunset times
    sunrise_decimal = NA,  # Placeholder for sunrise times in decimals
    sunset_decimal  = NA   # Placeholder for sunset times in decimals
  )
  
  # Compute Sunrise and Sunset
  # Use suncalc::getSunlightTimes() to compute solar times
  sunlight <- getSunlightTimes(
    date = sun.times$date,
    lat = lat,
    lon = lon,
    tz = tz,
    keep = c("sunrise", "sunset")  # Keep only relevant columns
  )
  
  # Assign solar times to the data frame, convert to character for consistency
  sun.times$sunrise <- as.character(sunlight$sunrise)
  sun.times$sunset  <- as.character(sunlight$sunset)
  
  # Generate sunrise and sunset times in hours (decimal)
  sun.times$sunrise_decimal <- as.numeric(as.numeric(format(strptime(sun.times$sunrise, "%Y-%m-%d %H:%M:%S"), "%H"))) +
    as.numeric(as.numeric(format(strptime(sun.times$sunrise, "%Y-%m-%d %H:%M:%S"), "%M"))) / 60
  sun.times$sunset_decimal  <- as.numeric(as.numeric(format(strptime(sun.times$sunset, "%Y-%m-%d %H:%M:%S"), "%H"))) +
    as.numeric(as.numeric(format(strptime(sun.times$sunset, "%Y-%m-%d %H:%M:%S"), "%M"))) / 60
  
  
  # Return Result
  return(sun.times)
}

