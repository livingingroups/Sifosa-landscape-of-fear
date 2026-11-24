#' Function: f_process_encounters.R
#'
#' Detect encounters between all listed pairs, and aggregates the data into a final table.
#' 
#' @author Maevatiana N. Ratsimbazafindranahaka (primary code author)
#' @author Meredith C. Lutz (code reviewer)
#'
#' @param paired_combinations Data frame containing individual ID pairs.
#' @param gps List containing ID mappings (`gps$ids`) and timestamps (`gps$timestamps`).
#' @param ddd dyadic distances.
#' @param id_col1 Name of the column containing the first individual in each pair.
#' @param id_col2 Name of the column containing the second individual in each pair.
#' @param encounter_radius Numeric value defining the maximum distance (in meters) for an encounter to be considered.
#' @param encounter_outer_limit Numeric value defining a broader proximity threshold (in meters).
#' See f_get_encounters.R for more details on the detection process
#' 
#' @return Data frame with encounter details, including start and end timestamps.

library(dplyr)
library(lubridate)

f_process_encounters <- function(paired_combinations, gps, ddd, id_col1, id_col2, 
                               encounter_radius, encounter_outer_limit) {
  
  all_encounters <- data.frame()  # Initialize final table
  
  # Loop through each pair of individuals
  for (combination_number in 1:nrow(paired_combinations)) {
    
    # Retrieve individual indices
    indA <- gps$ids$idx[which(gps$ids$id_code == paired_combinations[[id_col1]][combination_number])]
    indB <- gps$ids$idx[which(gps$ids$id_code == paired_combinations[[id_col2]][combination_number])]
    
    # Extract encounters using specified distance thresholds
    encounter <- f_get_encounters(indA, indB, gps$ids, ddd, gps$timestamps, 
                                encounter_radius, encounter_outer_limit, 2)
    
    # Process valid encounters
    if (!is.null(encounter)) {
      
      filtered_encounter <- encounter[encounter$id != 0, ]  # Remove non-encounters
      
      # Aggregate encounters (start/end timestamps per encounter ID)
      result <- filtered_encounter %>%
        group_by(id, indA, indB, name_id) %>%
        summarise(
          start = min(timestamps),
          end = max(timestamps),
          .groups = "drop"
        )
      
      # Append results to the final table
      all_encounters <- rbind(all_encounters, result)
    }
  }
  
  return(all_encounters)
}
