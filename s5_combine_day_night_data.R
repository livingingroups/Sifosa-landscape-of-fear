#'----------------------------------------------------------------------------------------------------
#' Script: s5_combine_day_night_data.R
#'
#' Assemble all daily and nightly average behavioral metrics: DDD, VeDBA, % head-up
#' sleep site arrival and departure time, path length, straigtness, sleep efficiency
#'
#' @author Maevatiana N. Ratsimbazafindranahaka (primary code author)
#' @author Meredith C. Lutz (code reviewer)
#' @export `sifaka_day_night_behav_[YEAR]_dry.csv`
#-----------------------------------------------------------------------------------------------------

rm(list = ls())

library(stringr)
library(dplyr)
library(readxl)
library(lubridate)
library(data.table)

# CSV table containing all deployment, group and individual info
depinfo_file <- "C:/Users/maevatiana.ratsim/Documents/Projects/Sifaka/Sifosa-landscape-of-fear-datasets/GPS/reference.csv"

# Load the table containing sifaka group and individual information
depinfo <- fread(depinfo_file)

# --- Load ddd, vedba and %head up data  from s5e_compute_day_night_behav.R ---
beh_data <- list(
  "2022" = read.csv("C:/Users/maevatiana.ratsim/Documents/Projects/Sifaka/Sifosa-landscape-of-fear-datasets/s4e/sifaka_day_night_behav_2022_dry.csv"),
  "2023" = read.csv("C:/Users/maevatiana.ratsim/Documents/Projects/Sifaka/Sifosa-landscape-of-fear-datasets/s4e/sifaka_day_night_behav_2023_dry.csv")
)

# --- Load path length and tortuosity from s5b_mov_metrics.R ---
mov_data <- list(
  "2022" = read.csv("C:/Users/maevatiana.ratsim/Documents/Projects/Sifaka/Sifosa-landscape-of-fear-datasets/s4b/sifaka_day_night_mov_2022_dry.csv"),
  "2023" = read.csv("C:/Users/maevatiana.ratsim/Documents/Projects/Sifaka/Sifosa-landscape-of-fear-datasets/s4b/sifaka_day_night_mov_2023_dry.csv")
)

# ----- Load sleep site data containing arrival time and departure time

sleep_sites <- list(
  "2022" = read.csv("C:/Users/maevatiana.ratsim/Documents/Projects/Sifaka/Sifosa-landscape-of-fear-datasets/s4a/sleep_sites_2022_dry.csv"),
  "2023" = read.csv("C:/Users/maevatiana.ratsim/Documents/Projects/Sifaka/Sifosa-landscape-of-fear-datasets/s4a/sleep_sites_2023_dry.csv")
)

# --- Load summarized sleep data ---
load("C:/Users/maevatiana.ratsim/Documents/Projects/Sifaka/Sifosa-landscape-of-fear-datasets/s4c/sleepPeriodSummarizedData.RData")
sleep_per_noNA <- sleep_per_noNA %>%
  mutate(animal_id = animal)

# ----- Load fossa data
fossa <- read.csv("C:/Users/maevatiana.ratsim/Documents/Projects/Sifaka/Sifosa-landscape-of-fear-datasets/s4d/fosa_detection_pairs.csv")

# --- Combine years ---
joined <- bind_rows(beh_data) %>%
  mutate(join_year = year(date), date = as.Date(date))%>%
  select(date, day_night, join_year, animal_id, vedba, headup, ddd, n_inds_ddd, group_id)

# TST
joined <- joined %>%
  left_join(
    sleep_per_noNA %>% 
      select(animal_id, date, day_night, TST_LA),
    by = c("animal_id", "date", "day_night")
  )

#Mov
joined <- joined %>%
  left_join(
    bind_rows(mov_data) %>% mutate(date = as.Date(date)) %>%
      select(animal_id, date,day_night, path_length, straightness),
    by = c("animal_id", "date", "day_night")
  )

#sleep sites
joined <- joined %>%
  left_join(
    bind_rows(sleep_sites) %>% mutate(date = as.Date(date)) %>%
      select(animal_id, date,arrive_minutes, leave_minutes),
    by = c("animal_id", "date")
  )

#fosa
joined <- joined %>%
  left_join(
    fossa %>% mutate(date = as.Date(date)) %>%
      select(date,day_night, fosa_detect_rate, pair_presence),
    by = c("date", "day_night")
  )

View(joined)

write.csv(
  joined,
  file = "C:/Users/maevatiana.ratsim/Documents/Projects/Sifaka/Sifosa-landscape-of-fear-datasets/s5/combined_metrics_all.csv",
  row.names = FALSE
)

