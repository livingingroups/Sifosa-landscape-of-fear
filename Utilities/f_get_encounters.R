#' Function: f_get_encounters.R
#'
#' Detects and visualizes encounters between two individuals over time 
#' based on predefined distance thresholds. It identifies when the individuals are within the 
#' encounter radius and a broader outer thresholde.
#' 
#' @author Maevatiana N. Ratsimbazafindranahaka (primary code author)
#' @author Tracy M. Montgomery (code reviewer)
#'
#' @param indA Integer index of the first individual (as defined in the AM formatted gps).
#' @param indB Integer index of the second individual (as defined in the AM formatted gps).
#' @param ids Data frame mapping individual indices ('idx') to their unique identifiers ('id_code').
#' @param ddd 3D array (individual x individual x time) containing pairwise distances over time.
#' @param timestamps Vector of timestamps corresponding to the third dimension of `ddd`.
#' @param encounter_radius Numeric value defining the maximum distance (in meters) for an encounter to be considered.
#' @param encounter_outer_limit Numeric value defining a broader proximity threshold (in meters).
#' 
#' ### Encounter Grouping Rule:
#' If two individuals are within `encounter_radius` (e.g., 50 m), an encounter starts.  
#' If they temporarily move apart beyond `encounter_radius` but remain within `encounter_outer_limit` (e.g., 300 m), 
#' and then return within `encounter_radius`, both close-contact periods (and the in-between period) 
#' are treated as a **single** encounter instead of separate ones.
#' 
#' @param encounter_to_plot Integer specifying which detected encounter event to highlight in the plot (e.g., if 1, then the first detected encounter is plotted).
#' 
#' @returns A data frame containing details of all detected encounters, including:
#' - `timestamps`: The time of each recorded interaction.
#' - `indA` and `indB`: The identifiers of the two individuals.
#' - `status`: Whether there is an encounter at a given time.
#' - `id`: Unique encounter event identifiers.



# Load required packages
library(lubridate) # For handling timestamps

f_get_encounters <- function(indA, indB, ids, ddd, timestamps, encounter_radius, encounter_outer_limit, encounter_to_plot) {
  
  
  # Initialize a data frame to store encounter statuses
  encounter <- data.frame(
    status = rep(FALSE, length(timestamps)),  # Indicates if individuals are within encounter radius
    outer = rep(FALSE, length(timestamps))    # Indicates if individuals are within outer limit
  )
  
  # Flag encounters based on distance thresholds
  encounter$status[ddd[indA, indB, ] <= encounter_radius] <- TRUE
  encounter$outer[ddd[indA, indB, ] <= encounter_outer_limit] <- TRUE
  
  # Assign an unique ID for each continuous period within outer limit
  encounter$outer_id <- cumsum(encounter$outer & c(TRUE, diff(encounter$outer) != 0)) 
  encounter$outer_id[!encounter$outer] <- 0 # set to NA if not an encounter
  
  # Plot flagged encounter status over time
  u <- data.frame(
    outer_id   = encounter$outer_id[!duplicated(encounter$outer_id)],
    outer   = encounter$outer[!duplicated(encounter$outer_id)],
    timestamps = timestamps[!duplicated(encounter$outer_id)]
  )
  u <- u[u$outer == T, ]
  par(mar = c(5, 15, 4, 2))  # bottom, left, top, right
  plot(timestamps, encounter$outer, type = "l", main = "Encounter Status Over Time", xlab = "Time", 
       ylab = "", yaxt = "n", ylim = c(0,1.2))
  lines(timestamps, encounter$status*0.5, col = "red")
  axis(2, at = c(0, 0.5, 1),
       labels = c("No encounter", "Within encounter radius", "Within outer limit"), las = 1)
  if (nrow(u) > 0) {
    text(u$timestamps, 1.1, u$outer_id, cex = 0.7) 
    }
  
  # Identify encounters within outer threshold where the encounter_radius was also reached
  relevant_outer_id <- unique(encounter$outer_id[encounter$status])
  if (length(relevant_outer_id) != 0) {
    
    # For each encounters within outer threshold where the encounter_radius was also reached
    for (i in seq_along(relevant_outer_id)) {
      
      # Get the indices of all encounter within encounter_radius within it
      indices <- which(encounter$status & encounter$outer_id == relevant_outer_id[i])
      
      # From the first to the last index, make the encounter within encounter_radius flag true
      # this result in the combination of successive encounter within encounter_radius into one if they are still within the same within-encounter_outer_limit
      if (length(indices) != 0) {
        first_index <- indices[1]
        last_index <- indices[length(indices)]
        encounter$status[first_index:last_index] <- TRUE
      }
    }
  }
  
  # Assign unique encounter IDs
  encounter$id <- cumsum(encounter$status & c(TRUE, diff(encounter$status) != 0))
  encounter$id[!encounter$status] <- 0 # set to zero if not an encounter
  
  # Plot final encounter identification
  u <- data.frame(
    id   = encounter$id[!duplicated(encounter$id)],
    outer   = encounter$outer[!duplicated(encounter$id)],
    timestamps = timestamps[!duplicated(encounter$id)]
  )
  u <- u[u$outer == T, ]
  par(mar = c(5, 15, 4, 2))  # bottom, left, top, right
  plot(timestamps, encounter$outer, type = "l", main = "Final encounter Status Over Time", xlab = "Time", 
       ylab = "", yaxt = "n", ylim = c(0,1.2))
  lines(timestamps, encounter$status*0.5, col = "red")
  axis(2, at = c(0, 0.5, 1),
       labels = c("No encounter", "Within encounter radius", "Within outer limit"), las = 1)
  if (nrow(u) > 0) {
    text(u$timestamps+6*60*60, 0.5, u$id, cex = 0.7)
  }
  
  # Add encounter metadata
  encounter$name_id <- paste(ids$id_code[ids$idx == indA], ids$id_code[ids$idx == indB], encounter$id, sep = "_") # Unique name for each encounter
  encounter$indA <- ids$id_code[ids$idx == indA] # First individual
  encounter$indB <- ids$id_code[ids$idx == indB] # Second individual
  encounter$timestamps <- timestamps
  
  # Plot the distance between individuals over time, highlighting the selected encounter, and return the whole detection result 
  
  # Adjust encounter_to_plot if it exceeds the number of available encounters
  if (encounter_to_plot > max(unique(encounter$id), na.rm = TRUE)) {
    encounter_to_plot <- max(unique(encounter$id), na.rm = TRUE)
  }
  
    if (nrow(encounter[encounter$status, ]) != 0) {
    par(mar = c(5, 15, 4, 2))  # bottom, left, top, right
    plot(
      timestamps, ddd[indA, indB, ],
      type = "l",
      xlim = c(min(encounter$timestamps[encounter$id == encounter_to_plot]) - hours(1),
               max(encounter$timestamps[encounter$id == encounter_to_plot]) + hours(1)),
      ylim = c(0, 100),
      main = paste0("Encounter #", encounter_to_plot, " Between ", 
                    ids$id_code[ids$idx == indA], " and ", 
                    ids$id_code[ids$idx == indB]),
      ylab = "Distance", xlab = "Time"
    )
    # Highlight the periods corresponding to the chosen encounter
    lines(timestamps[encounter$id == encounter_to_plot], 
          ddd[indA, indB, encounter$id == encounter_to_plot], col = "red", type = "o")
    
    return(encounter)
    
  } else {
    
    print("No encounters detected.")
    return(NULL)
    
  }
}
