#'----------------------------------------------------------------------------------------------------
#' Script: s4d_fossa_presence.R
#'
#' Quantify daily and nightly fosa presence
#'
#' @author Dario Walser (primary code author)
#' @author Maevatiana Ratsimbazafindranahaka (code reviewer)
#' @export `fosa_detection_pairs.csv`

#-----------------------------------------------------------------------------------------------------

rm(list = ls())

library(tidyverse)
library(suncalc)
library(lubridate)

# List and source all useful R functions -------------------------------------------------------------------

files <- list.files("C:/Users/maevatiana.ratsim/Documents/Projects/Dev/Sifosa-landscape-of-fear/Utilities/", pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
lapply(files, source)

# Parameters -----------------------------------------------------------------------------------------

# Regularly sampled ACC data (VeDBA and pitch) location (1 csv = 1 individual 1 year 1 season)
fossa_dir       <- "C:/Users/maevatiana.ratsim/Documents/Projects/Sifaka/Sifosa-landscape-of-fear-datasets/CAM/"

# Output directory
output_dir    <- "C:/Users/maevatiana.ratsim/Documents/Projects/Sifaka/Sifosa-landscape-of-fear-datasets/s4d/"

###############################################

fosa_all <- fread(paste0(fossa_dir, "detection_table.csv")) #combined data of 2022 and 2023
fosa_all <- fosa_all %>% #turn time and dates into POSIXct format
  mutate(pair_presence=ifelse(group_size==1,0,1),
         timestamp = force_tz(timestamp, tz = "Indian/Antananarivo"),
         date = date(timestamp), #add separate column for date
         year = year(timestamp),
         ncams = ifelse(year == 2022, 36, 54)) #number of cameras in grid per year
head(fosa_all$timestamp) # First time should be 2022-06-01 00:48:00 EAT

sun_times <- f_get_suntimes(
    fosa_all$date,
    lat = -20.783,
    lon = 44.174,
    tz = "Indian/Antananarivo")

sun_times <- sun_times %>%
  mutate(sunrise = as.POSIXct(sunrise, tz = "Indian/Antananarivo"),
         sunset  = as.POSIXct(sunset, tz = "Indian/Antananarivo"))

fosa_all <- fosa_all %>%
  left_join(sun_times, by = "date")


#### Calculate Fosa Day/Night ####
fosa_all <- fosa_all %>% #assign "day" or "night" relative to sunrise and sunset
  mutate(day_night=ifelse(timestamp>sunset,"night", ifelse(timestamp>sunrise, "day", "night")))

#shift the data to match a sifaka sunrise to sunrise day, meaning the assigned "night" before and after midnight will both belong to the same day (date before midnight)
fosa_all <- fosa_all %>%
  mutate(sameday = ifelse(timestamp < sunrise, "previous", "same")) #if entry is before sunrise, then this should be shifted to the previous day

fosa_all$sifaka_date <- fosa_all$date  #new column to create dates relevant for sifaka analysis

#shift "previous" columns to previous date
fosa_all$sifaka_date <- if_else(
  fosa_all$sameday == "same",
  fosa_all$sifaka_date,
  fosa_all$sifaka_date - 1
)

n_distinct(filter(fosa_all, year(date) == 2022)$placename) #in 2022 there were 31 cameras that detected fosa  (36 cameras total)
n_distinct(filter(fosa_all, year(date) == 2023)$placename) #in 2023 there were 51 cameras that detected fosa (54 cameras total)

#Calculate Detection Rate
fosa_rate <- fosa_all %>% 
  summarize(.by= c(year, sifaka_date, day_night, ncams), pair_presence=ifelse(sum(pair_presence)==0,0,1), ncams_fosa = n()) #count of total fosa photos per day or night

fosa_rate <- fosa_rate %>% 
  mutate(fosa_detect_rate = round((ncams_fosa / ncams), 2)) #calculate detect rate as number of photos divided by number of available cams

#fill in all missing days/nights
days <- c(seq(ymd("2022-05-31"),ymd("2022-07-31"), by="days"),
          seq(ymd("2023-05-31"),ymd("2023-07-31"), by="days")) %>% 
  rep(each=2) %>% 
  {cbind(sifaka_date=., day_night=rep(c("day","night"), length=length(.)))} %>% 
  as.data.frame() %>% 
  mutate(sifaka_date=as.numeric(sifaka_date),
         sifaka_date=as_date(sifaka_date))

fosa_rate <- merge(fosa_rate, days, by=c("sifaka_date", "day_night"), all.y=T) %>% 
  mutate(year=year(sifaka_date), ncams=ifelse(year==2022,36, 54)) %>% #fill in fixed values for each year
  replace(is.na(.), 0) #all remaining empty values mean there was no detections --> 0 values

fosa_rate <- fosa_rate %>%
  rename(date = sifaka_date)

#save data to file
write_csv(fosa_rate, file=paste0(output_dir,"fosa_detection_pairs.csv")) #all fosa data together in long format

