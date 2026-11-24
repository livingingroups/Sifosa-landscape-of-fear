#'----------------------------------------------------------------------------------------------------
#' Script: s1_extract_sifosa_encounters.R
#' 
#' Identify encounters between Sifaka and Fosa using GPS tracking data from s0_make_ari_matrix.R
#'
#' @author Maevatiana N. Ratsimbazafindranahaka (primary code author)
#' @author Meredith C. Lutz (code reviewer)
#' @export `sifosa_[year]_encounters.Rdata` Encounter data: all_sifosa_encounters (data frame, 1 row = one encounter), notes (char)
#-----------------------------------------------------------------------------------------------------

rm(list = ls())

library(dplyr)         # Data manipulation
library(lubridate)     # Date-time handling

# List and source all useful R functions -------------------------------------------------------------------

files <- list.files("C:/Users/maevatiana.ratsim/Documents/Projects/Dev/Sifosa-landscape-of-fear/Utilities/", pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
lapply(files, source)

# Parameters -----------------------------------------------------------------------------------------

encounter_radius <- 50        # Inner distance threshold for defining an encounter
encounter_outer_limit <- 300  # Outer distance threshold beyond which any following move to 50 m is considered as a different encounter

# GPS data in AM fromat full path (from s1a_make_ari_matrix.R). Contains gps, ddd, group_list, and notes
gps_file <- "C:/Users/maevatiana.ratsim/Documents/Projects/Sifaka/Sifosa-landscape-of-fear-datasets/s0/sifosa_2023_dry_AM.Rdata"
season = 'dry'

# Output directory
output_dir <- "C:/Users/maevatiana.ratsim/Documents/Projects/Sifaka/Sifosa-landscape-of-fear-datasets/s1/"

# Load the pre-processed data ---------------------------------------------------------------------------------
load(gps_file)
head(gps$timestamps) # first timestamp should be “2022-06-24 04:00:00 EAT” in 2022 and “2023-06-25 03:00:00 EAT” in 2023

# Create a data frame of all possible combinations for Sifaka-Fosa and Fosa-Fosa encounters
# Paired Sifaka-Fosa combinations
paired_sifosa_combinations <- expand.grid(sifaka = gps$ids$id_code[gps$ids$species == "sifaka"],
                                          fosa = gps$ids$id_code[gps$ids$species == "fosa"])

# Paired Fosa-Fosa combinations, excluding self-encounters
paired_fosafosa_combinations <- expand.grid(fosaA = gps$ids$id_code[gps$ids$species == "fosa"],
                                            fosaB = gps$ids$id_code[gps$ids$species == "fosa"])
paired_fosafosa_combinations <- paired_fosafosa_combinations[paired_fosafosa_combinations$fosaA != 
                                                               paired_fosafosa_combinations$fosaB,] # Remove self encounters

# View the possible encounter combinations
print(paired_sifosa_combinations)
print(paired_fosafosa_combinations)

# Detect encounters for each Sifaka-Fosa and Fosa-Fosa pairs -----------------------------------------------------------------------------------

# Bw sifaka and fossa
all_sifosa_encounters <- f_process_encounters(paired_sifosa_combinations, gps, ddd, "sifaka", "fosa", 
                                            encounter_radius, encounter_outer_limit)
nrow(all_sifosa_encounters)
#2022 dry: 17, 2023 dry: 52 

# Bw fossa and fossa
all_fosafosa_encounters <- f_process_encounters(paired_fosafosa_combinations, gps, ddd, "fosaA", "fosaB", 
                                              encounter_radius, encounter_outer_limit)
nrow(all_fosafosa_encounters)
#2022 dry: 0, 2023 dry: 50

# Identify fosa "traveling" together based on fosa-fosa encounter data and flag them on sifaka-fossa encounters -------------------------------------------------------------------------

# First, adjust start and end times for fossa-fossa encounters where there's only one point within 50 meters (resulting in start == end)
# For these cases, add 3 minutes to the start and end time
if (nrow(all_fosafosa_encounters) > 0) { # If there are any Fosa-Fosa encounters
  all_fosafosa_encounters$adjustedstartend = F # Column to store if adjustment was made or not 
  all_fosafosa_encounters$adjustedstartend[all_fosafosa_encounters$start == all_fosafosa_encounters$end] = T # The cases that will be adjusted
  all_fosafosa_encounters$start[all_fosafosa_encounters$adjustedstartend == T] = all_fosafosa_encounters$start[all_fosafosa_encounters$adjustedstartend == T] - minutes(3) # Start - 3 min
  all_fosafosa_encounters$end[all_fosafosa_encounters$adjustedstartend == T] = all_fosafosa_encounters$end[all_fosafosa_encounters$adjustedstartend == T] + minutes(3) # End + 3 min
}

# Check for overlapping Sifaka-Fosa and Fosa-Fosa encounters (i.e, cases of Fosa accompanied by another Fosa) and flag them
all_sifosa_encounters$fosa_companion = "None"  # Initialize with "None"
all_sifosa_encounters$fosa_type = "lone"       # Default to "lone"
for (i in 1:nrow(all_sifosa_encounters)){ # For each sifosa encounter
  
  # Retrieve all encounters with another fosa by the fosa (indB) encountering the current sifaka (indA in all_sifosa_encounters)
  fosafosa_encounters = all_fosafosa_encounters[all_fosafosa_encounters$indA == all_sifosa_encounters$indB[i],]
  
  # Check if any Fosa-Fosa encounter for the current Fosa overlaps with its encounter with the sifaka
  if_accompanied_fosa = (all_sifosa_encounters$start[i] >= fosafosa_encounters$start & all_sifosa_encounters$start[i] <= fosafosa_encounters$end) |
    (all_sifosa_encounters$end[i] >= fosafosa_encounters$start & all_sifosa_encounters$end[i] <= fosafosa_encounters$end)
  
  # If an overlap is found, assign the Fosa companion name and classify as a "pair"
  if (sum(if_accompanied_fosa) > 0){
    all_sifosa_encounters$fosa_companion[i] = fosafosa_encounters$indB[if_accompanied_fosa]
    all_sifosa_encounters$fosa_type[i] = "pair"
  } 
}

# View the results of Sifaka-Fosa encounters with assigned companion information
print(all_sifosa_encounters)

# Arrange the encounter data by start time and individual names
all_sifosa_encounters <- all_sifosa_encounters %>%
  arrange(start) %>% arrange(indA)

# Inspection: Print the Sifaka-Fosa encounters where the Fosa was accompanied (fosa_type == "pair")
print(all_sifosa_encounters[all_sifosa_encounters$fosa_type == "pair",], n = 100)
nrow(all_sifosa_encounters[all_sifosa_encounters$fosa_type == "pair",])
#2022 dry: 0, 2023 dry: 19 

# For each encounter between a Sifaka and a Fosa, check if the Fosa is accompanied by another Fosa.
# If the accompanying Fosa's encounter with the Sifaka happens within 10 minutes of the first Fosa's encounter,
# mark the accompanying Fosa's encounter as a duplicate so that they can be excluded in further analysis (i.e., only analyzing the first encounter).
all_sifosa_encounters$duplicate = F # Initialize a 'duplicate' column
for (i in 2:nrow(all_sifosa_encounters)) {
  # Check if the current sifaka-fosa encounter involves the same fosa pair as the previous encounter
  # and if the encounters are within 10 minutes of each other
  if (all_sifosa_encounters$indA[i] == all_sifosa_encounters$indA[i-1] & 
      all_sifosa_encounters$indB[i] == all_sifosa_encounters$fosa_companion[i-1] &
      abs(all_sifosa_encounters$start[i] - all_sifosa_encounters$start[i-1]) < minutes(10)) {
    all_sifosa_encounters$duplicate[i] = T # Mark the current encounter as a duplicate
  }
}

# View the final table of Sifaka-Fosa encounters, including duplicates marked
print(all_sifosa_encounters)
sum(all_sifosa_encounters$duplicate == T)
# 2022 dry: 0, 2023 dry: 8

# Save everything ------------------------------------------------------------------------------------------------------------------------

file_file <- paste0(output_dir, "sifosa_", curr_year,"_", season, "_encounters.Rdata")
notes <- paste0("These data were generated using the script s2_extract_sifosa_encounters.R\n",
                "Encounters were defined using encounter_radius = ", encounter_radius, " and encounter_outer_limit = ", encounter_outer_limit)
save(all_sifosa_encounters, notes, file = file_file); print(paste("Data saved to:", file_file))


