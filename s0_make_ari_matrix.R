#'----------------------------------------------------------------------------------------------------
#' Script: s0_make_ari_matrix.R
#' 
#' Structures GPS data into Ari's matrix (AM) format for spatial and dyadic interactions analysis and
#' computes dyadic distances and identifies encounters
#'
#' @author Maevatiana N. Ratsimbazafindranahaka (primary code author), adapted from cocomo package
#' @author Tracy M. Montgomery (code reviewer)
#' @export `sifosa_[YEAR]_AM.Rdata` GPS data set in Ari's matrix format: gps (list), ddd (matrix), group_list (data frame), notes (chr), curr_year (num)
#-----------------------------------------------------------------------------------------------------

rm(list = ls())

library(readr)
library(readxl)
library(dplyr)
library(lubridate)
library(data.table)

# List and source all useful R functions -------------------------------------------------------------------

files <- list.files("C:/Users/maevatiana.ratsim/Documents/Projects/Dev/Sifosa-landscape-of-fear/Utilities/", pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
lapply(files, source)

# Parameters -----------------------------------------------------------------------------------------

time_zone = "Indian/Antananarivo"
curr_year = 2023
season_marker = "dry" # Season. Used to label the output according to the season

dir <- "C:/Users/maevatiana.ratsim/Documents/Projects/Sifaka/Sifosa-landscape-of-fear-datasets/"
setwd(dir)

# Output directory
output_dir <- "s0/"

# Clean GPS data path
gps_dir = "GPS/"

# CSV table containing all deployment, group and individual info
depinfo_file <- "GPS/reference.csv"

# Read clean GPS data --------------------------------------------------------------------------------

sifaka <- fread(paste0(gps_dir, "sifaka.", substr(curr_year, 3, 4), ".3min.csv"))         # Sifakas
fosa <- tryCatch({fread(paste0(gps_dir, "fosa.", substr(curr_year, 3, 4), ".3min.csv"))},  # Fossa
                 error = function(e) {message("Could not read file: ", e$message)
                   tibble()}
)

sifaka$species  <- "sifaka"   # Flag the species
fosa$species    <- "fosa"     # Flag the species

# Combine sifaka and fossa data
if (nrow(fosa) != 0) {
  combined_data <- bind_rows(sifaka, fosa) 
} else {
  combined_data <- sifaka
}

# Fix timezone
message(sprintf(
    "The current local time (e.g., first row = %s) was loaded assuming that it is %s. It will now be forced to be %s without changing the values.",
    combined_data$study_local_timestamp[1], tz(combined_data$study_local_timestamp[1]), time_zone))
combined_data$study_local_timestamp <- force_tz(combined_data$study_local_timestamp, tzone = time_zone)

head(combined_data$study_local_timestamp) # first timestamp should be “2022-06-24 04:00:00 EAT” in 2022 and “2023-06-25 03:00:00 EAT” in 2023

message(paste0("Note: Currently working on data from ", curr_year))

# Load group and individual info --------------------------------------------------------------------------------

# Read
depinfo <- fread(depinfo_file)

# Make a table with list of individual sifakas and fosa of the current working year and season, 
# their corresponding group ID (NA for fossas) and sex
group_list <- depinfo %>%
  select(animal_id, animal_group_id, 
         year) %>%
  distinct() %>%
  arrange(animal_group_id) %>% filter(year == curr_year)

# Put the GPS data into Ari's Matrix (AM) format stored in one big list ------------------------------------------

combined_data$utm_easting <- combined_data$utm_easting_smooth
combined_data$utm_northing <- combined_data$utm_northing_smooth
gps <- f_make_sifosa_gps_matrix(combined_data, time_zone)

# Add group id
# Create a unique mapping from animal_id to animal_group_id
unique_map <- depinfo %>%
  select(animal_id, animal_group_id) %>%
  distinct(animal_id, .keep_all = TRUE)
# Join the unique mapping to gps$ids
gps$ids <- gps$ids %>%
  left_join(unique_map, by = c("id_code" = "animal_id")) %>%
  rename(group_id = animal_group_id)

# Quick inspection: Visualize GPS data of one specific group, along with a given list of fosa
# f_plot_group_and_fosa_tracks("G12", group_list, c("F01"), gps) # 2022
# f_plot_group_and_fosa_tracks("G12", group_list, c("F04","F06"), gps) # 2023

# Calculate dyadic distance (all possible pair combination) ----------------------------------------------------------------

ddd <- f_get_dyadic_distances(gps$xs, gps$ys) # ddd[i,j,t]` gives the distance between individuals `i` and `j` at time `t`

# Inspection: Visualize one encounter
# get_encounters(#indA, #indB, ids, ddd, timestamps, encounter_radius,encounter_outer_limit, #encounter_to_plot)
# e.g. ind 18 and 33 in 2023, ind 2 and 22 in 2022
# This is only for visualization.  See s2_extract_sifosa_encounters for the actual encounters extractions
# f_get_encounters(12, 21, gps$ids, ddd, gps$timestamps, 50,300, 3) #2022
# f_get_encounters(18, 33, gps$ids, ddd, gps$timestamps, 50,300, 3) #2023

# Save everything  ---------------------------------------------------------------------------------------------------------
file_file <- paste0(output_dir, "sifosa_", curr_year,'_', season_marker, "_AM.Rdata")
notes <- paste0("These data were generated using the script s1a_make_ari_matrix.R on ",
                format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))
save(gps, ddd, group_list, notes,curr_year, file = file_file); print(paste("Data saved to:", file_file))

