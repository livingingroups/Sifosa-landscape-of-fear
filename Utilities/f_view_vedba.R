#' Function: f_view_vedba.R
#' 
#' Visualize Vedba and Distance Data for a given Encounter
#' 
#' @author Maevatiana N. Ratsimbazafindranahaka (Primary code author)
#' @author NOT YET CODE REVIEWED
#'
#' @param i Integer index specifying the encounter to visualize in the `all_sifosa_encounters` data frame.
#'          This index corresponds to the row of the encounter that will be analyzed.
#' @param time_range A duration (e.g., `hours(2)` or `minutes(30)`) specifying the time window before and after
#'                   the encounter start and end times to consider for visualization.
#' @param all_sifosa_encounters Data frame containing details of all Sifaka-Fosa encounters, including 
#'                              encounter start and end times.
#' @param gps GPS in AM format                             
#' @param acc_loc File path to the location where the regularly sampled accelerometer data (Vedba) is stored.
#' @return None. This function generates plots: one for Vedba data and one for distance data.

library(dplyr)
library(lubridate)
library(ggplot2)
library(xts)

f_view_vedba <- function(i, time_range, all_sifosa_encounters, acc_loc, gps, season) {
  
  # Load current Vedba data for the current individual involved in the encounter and ensure consistent timezone 
  curr.ind <- all_sifosa_encounters$indA[i]
  curr.vedbadata <- read.csv(paste0(acc_loc, curr.ind, '_', names(which.max(table(year(all_sifosa_encounters$start)))),'_', season , '_regular.csv')) 
  curr.vedbadata$timestamps <- as.POSIXct(curr.vedbadata$DT_adj, tz = tz(gps$timestamps))
  
  # Find the indices for individuals A and B in the GPS data
  indA <- gps$ids$idx[which(gps$ids$id_code == all_sifosa_encounters$indA[i])]
  indB <- gps$ids$idx[which(gps$ids$id_code == all_sifosa_encounters$indB[i])]
  
  # Define the time range for the GPS and Vedba data
  gpsdata.range <- gps$timestamps >= all_sifosa_encounters$start[i] - time_range & 
    gps$timestamps <= all_sifosa_encounters$end[i] + time_range
  vedbadata.range <- curr.vedbadata$timestamps >= all_sifosa_encounters$start[i] - time_range & 
    curr.vedbadata$timestamps <= all_sifosa_encounters$end[i] + time_range
  
  # Set up a 2x1 grid for the plots
  par(mfrow = c(2, 1))
  
  # Define common x-axis limits based on the range of both GPS and Vedba timestamps
  common_xlim <- range(c(gps$timestamps[gpsdata.range], curr.vedbadata$timestamps[vedbadata.range]))
  
  # VeDBA plot (on the top panel)
  plot(curr.vedbadata$timestamps[vedbadata.range],
       curr.vedbadata$vedba[vedbadata.range],
       type = "l",
       main = paste0(""),
       ylab = "VeDBA",
       xlab = "Time",
       xlim = common_xlim)

  # Distance plot (on the bottom panel)
  plot(gps$timestamps[gpsdata.range],
       ddd[indA, indB, gpsdata.range],
       type = "l",
       main = paste0(""),
       ylab = "Distance (m)",
       xlab = "Time",
       xlim = common_xlim)
  # Add horizontal lines reference
  abline(h = 50, col = "red", lty = 2)  
  text(x = par("usr")[1]+2*60*60, y = 50, labels = "(50 m)", pos = 3, col = "red")
  abline(h = 350, col = "black", lty = 2)  # Add a horizontal line at 300 for reference
  text(x = par("usr")[1]+2*60*60, y = 350, labels = "(350 m)", pos = 3, col = "black")
}
