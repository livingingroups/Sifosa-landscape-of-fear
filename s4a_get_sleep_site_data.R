#'----------------------------------------------------------------------------------------------------
#' Script: s4a_get_sleep_site_data.R
#'
#' #Script for identifying sleep sites and visits
#' Summary
#'  This script:
#'    - Identifies sleep site locations
#'    - Determines which site tree was used by each individual on each night
#'    - Determines arrival (in the afternoon/eve) and departure (in the morning) times from sleep sites
#'      and using GPS data from s0a_clean_gps.R
#' 
#' How it works
#'  To identify sleep trees we:
#'    1. Get one point every hour for each individual during the period of 23:00 
#'      to 03:00 each night. This is considered to be the night (conservative definition).
#'    2. Run DBSCAN across all night time points, across all individuals and nights 
#'      combined. This chains together points within a given radius of one another into
#'      clusters. Here we set the radius to 25 m.
#'    3. For individuals that occupied more than one cluster on a given night, count
#'      them as being in the cluster associated with the majority of their points.
#'    4. Optionally (if make_plots is T), make some plots of these, and also output
#'      png files to string together into a visualization of sleep tree usage across nights
#'    5. Output two tables:
#'      a) The sleep site occupied by each individual on each night (sleep_site_visits data frame)
#'      b) Info about all sleep sites (sleep_sites data frame)
#'  The user has to specify several parameters, under the Parameters heading below
#'  These are described below.
#'  The important parameters are:
#'    night_start_hour: numeric value, representing the start of the "night" for the 
#'      purposes of identifying sleep trees. This should be set to a time when we 
#'      are pretty sure the sifakas are in their sleep tree by that time 
#'      Currently, it is set to 23 (11pm)
#'    night_end_hour: numeric value, representing the end of the "night" for the 
#'      purposes of identifying sleep trees. This should be set to a time when we 
#'      are pretty sure the sifakas are still in their sleep tree at that time 
#'      Currently, it is set to 3 (3am)
#'    radius: numeric value, radius to use in DBSCAN when identifying sleep sites - 
#'      see description below. Currently set to 25 m
#'    year: which year of data to run the script on (2022 or 2023)
#'    arrival_departure_radius: radius to use for determining arrival and departure 
#'      times. Currently set to 80 m
#'
#' @author Ari Strandburg-Peshkin (primary code author)
#' @author Maevatiana N. Ratsimbazafindranahaka (code reviewer)
#' @export `sleep_site_data_[year].RData` containing two data frames:
#'      1) sleep_sites - information about sleep site locations
#'            sleep_sites$sleep_site_id: an arbitrary id number for each sleep site
#'            sleep_sites$centr_northing: northing of the centroid of each sleep site
#'            sleep_sites$centr_easting: easting of the centroid of each sleep site
#'      2) sleep_site_visits - information about which sleep tree was visited each night by each individual
#'            sleep_site_visits$animal_id: animal id (from reference table)
#'            sleep_site_visits$night_date: date associated with a night 
#'              (we use the date from the night before, rather than the subsequent day,
#'              so for instance 2023-07-01 would refer to the night running from the eve
#'              of 2023-07-01 to the morning of 2023-07-02)
#'            sleep_site_visits$sleep_site_id: id number of the sleep site used by that 
#'              individual on that night
#'            sleep_site_visits$centr_easting: easting of sleep site centroid (same as in 
#'              other table)
#'            sleep_site_visits$centr_northing: northing of sleep site centroid (same as in 
#'              other table)
#'            sleep_site_visits$animal_group_id: animal group id of the individual
#'            sleep_site_visits$prev_sleep_site: id of the sleep site from the night before
#'            sleep_site_visits$next_sleep_site: id of the sleep site from the night after
#'            sleep_site_visits$dist_next_sleep_site: distance from the current to the next sleep site
#'            sleep_site_visits$ind_centr_easting: easting of the individual's position in 
#'              the sleep site during the night (from night_start_hour to night_end_hour)
#'            sleep_site_visits$ind_centr_northing: northing of the individual's position in 
#'              the sleep site during the night (from night_start_hour to night_end_hour)
#'            sleep_site_visits$ind_first_easting: easting of the individual's position at 
#'              the beginning of the night (at night_start_hour)
#'            sleep_site_visits$ind_first_northing: northing of the individual's position at 
#'              the beginning of the night (at night_start_hour)
#'            sleep_site_visits$ind_last_easting: easting of the individual's position at 
#'              the end of the night (at night_end_hour)
#'            sleep_site_visits$ind_last_northing: northing of the individual's position at 
#'              the end of the night (at night_end_hour)
#'            sleep_site_visits$ind_max_dist_centr: maximum distance from the individual's 
#'              nighttime centroid between the hours of night_start_hour and night_end_hour
#'            sleep_site_visits$arrive_time: time of arrival at the sleep site on the current
#'              night, defined as the time that the individual arrived within a radius 
#'              arrival_departure_radius of its first night time position (at night_start_hour)
#'            sleep_site_visits$leave_time: time of leaving the sleep site on the subsequent
#'              morning, defined as the time that the individual left a radius of
#'              arrival_departure_radius from its last night time position (at night_end_hour)
#'            sleep_site_visits$arrive_time_next: time of arrival at the next sleep site on 
#'              the subsequent day (defined as above)
#'              sleep_site_visits$curr_sleep_site_usage: overall fraction of usage of the current sleep site by that individual across all data
#'            sleep_site_visits$next_sleep_site_usage: overall fraction of usage of the next sleep site by that individual across all data
#'            sleep_site_visits$prev_sleep_site_usage: overall fraction of usage of the previous sleep site by that individual across all data
#'            sleep_site_visits$nearest_outgroup_neighbor_group_id: group id of the nearest neighboring group that night
#'            sleep_site_visits$nearest_outgroup_neighbor_dist: distance to the nearest neighboring group that night 
#'              (i.e. distance between the sleep trees the two groups are using)
#'     The output file also contains a params object which stores the parameters used (see below)
#'     
#'----------------------------------------------------------------------------------------------------------

rm(list = ls())

library(lubridate)
library(dbscan)
library(readr)
library(readxl)
library(data.table)

# List and source all useful R functions -------------------------------------------------------------------

files <- list.files("C:/Users/maevatiana.ratsim/Documents/Projects/Dev/Sifosa-landscape-of-fear/Utilities/", pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
lapply(files, source)

# Parameters -----------------------------------------------------------------------------------------

time_zone <- "Indian/Antananarivo"
curr_year = 2022
season = 'dry'

# Path to sifaka and fossa GPS data file

wdir <- "C:/Users/maevatiana.ratsim/Documents/Projects/Sifaka/Sifosa-landscape-of-fear-datasets/"
setwd(wdir)

# Output directory: where to store plots and results
dir <- "s4a/"

# Clean GPS data path
# With sifaka and fossa GPS data
gps_dir = "GPS/"

# Table containing all deployment, group and individual info
depinfo_file <- "C:/Users/maevatiana.ratsim/Documents/Projects/Sifaka/Sifosa-landscape-of-fear-datasets/GPS/reference.csv"

# Modifiable variables
make_plots <- T # whether to make plots or not (T or F)
make_anim <- F # whether to output pngs for the animation (T or F)
print_basic_stats <- T # whether to print some basic stats about sleep sites

night_start_hour <- 23 # the (true) night is defined to start at 23:00
night_end_hour <- 3 # the (true) night is defined to end at 03:00 
subsamp_mins <- 60 # subsample to only on the hour (if 60) or every 15 min (if 15)
radius <- 25 # radius to use for dbscan (eps) - in meters - 25 m
min_pts <- 1 # minimum number of points to determine a cluster - 1 point
arrival_departure_radius <- 80 # distance threshold for arrival/departure time computation 
min_per_fix <- 3
# you are considered to be at the sleep site when within a distance arrival_departure_thresh of
# your mean location in the subsequent night (computed from night_start_hour to night_end_hour), 
# and same for departure

# Initial setup -------------------------------------------------------------------------------

# Store parameters in a list (will be saved later)
params <- list()
params$sifaka_path <- gps_dir
params$fosa_path <- gps_dir
params$dir <- dir
params$year <- year
params$night_start_hour <- night_start_hour
params$night_end_hour <- night_end_hour
params$subsamp_mins <- subsamp_mins
params$radius <- radius
params$min_pts <- min_pts
params$arrival_departure_radius <- arrival_departure_radius

# Load data
gps_sif <- fread(paste0(gps_dir, "sifaka.", substr(curr_year, 3, 4), ".3min.csv"))         # Sifakas
gps_fos <- tryCatch({fread(paste0(gps_dir, "fosa.", substr(curr_year, 3, 4), ".3min.csv"))},  # Fossa
                 error = function(e) {message("Could not read file: ", e$message)
                   tibble()}
)

# Load deployment info
depinfo <- fread(depinfo_file)

setwd(paste0(wdir,dir)) # Now move to output dir


# Fix timezone
message(sprintf(
  "The current local time (e.g., first row = %s) was loaded assuming that it is %s. It will now be forced to be %s without changing the values.",
  gps_sif$study_local_timestamp[1], tz(gps_sif$study_local_timestamp[1]), time_zone))
gps_sif$study_local_timestamp <- force_tz(gps_sif$study_local_timestamp, tzone = time_zone)
gps_fos$study_local_timestamp <- force_tz(gps_fos$study_local_timestamp, tzone = time_zone)
head(gps_sif$study_local_timestamp) # first timestamp should be “2022-06-24 04:00:00 EAT” in 2022 and “2023-06-25 03:00:00 EAT” in 2023
head(gps_fos$study_local_timestamp) # first timestamp should be “2022-06-24 04:00:00 EAT” in 2022 and “2023-06-25 03:00:00 EAT” in 2023

year = year(gps_sif$study_local_timestamp[1])

#make plotting work on windows as well as mac
if(.Platform$OS.type=="windows") {
  quartz<-function() windows()
}

# Process ------------------------------------------------------------------------------------------------

# Get nighttime data

# filter to only the hours between night_start_hour and night_end_hour and non NA
gps_sif_night <- gps_sif[which(
  (hour(gps_sif$study_local_timestamp) >= night_start_hour |
     hour(gps_sif$study_local_timestamp) <= night_end_hour) &
    !is.na(gps_sif$utm_easting_smooth) &
    !is.na(gps_sif$utm_northing_smooth)
), ]

# subsample based on subsamp_mins (every hour or every 15 min)
idxs_use <- which(minute(gps_sif_night$study_local_timestamp) %% subsamp_mins == 0)
gps_sif_night <- gps_sif_night[idxs_use,]

# add a column that specifies which date the night is associated with
# We use a definition such that the date of the night is defined with respect
# to the previous day's date (e.g. "Thursday night" runs from Thursday evening to Friday morning)

# get date
gps_sif_night$night_date <- lubridate::date(gps_sif_night$study_local_timestamp) 

# for times after midnight, use the date of the previous day as the "night_date" (as stated above)
idxs_earlymorning <- which(hour(gps_sif_night$study_local_timestamp) <= night_end_hour)
gps_sif_night$night_date[idxs_earlymorning] <- gps_sif_night$night_date[idxs_earlymorning] - 1

# add group id to the data frame
gps_sif_night$animal_group_id <- depinfo$animal_group_id[match(gps_sif_night$individual_local_identifier, depinfo$animal_id)]

# Cluster the points using dbscan
clusts <- dbscan(cbind(gps_sif_night$utm_easting_smooth, gps_sif_night$utm_northing_smooth), eps = radius, minPts = min_pts)

# add site id to the plot
gps_sif_night$cluster_id <- clusts$cluster

# For each night, assign each individual to the cluster where it has the majority of points
gps_sif_night$id_night <- paste(gps_sif_night$individual_local_identifier, gps_sif_night$night_date, sep = '_')
gps_sif_night$sleep_site_id <- NA
id_nights <- unique(gps_sif_night$id_night)
for(i in 1:length(id_nights)){
  id_night <- id_nights[i]
  idxs <- which(gps_sif_night$id_night == id_night)
  maj_clust <- as.numeric(names(sort(-table(gps_sif_night$cluster_id[idxs])))[1])
  gps_sif_night$sleep_site_id[idxs] <- maj_clust
}

#Make a plot by original cluster and by sleep site id (after doing the majority rule)
if(make_plots){
  cols_sites <- rainbow(max(gps_sif_night$cluster_id))
  cols_sites <- sample(cols_sites)
  quartz()
  plot(gps_sif_night$utm_easting_smooth, gps_sif_night$utm_northing_smooth, pch = 19, cex = 0.4, asp = 1, col = cols_sites[gps_sif_night$cluster_id], main = 'Original clusters')
  dev.copy2pdf(file = paste0('original_clustering_,', year,'_',season, '.pdf'))
  dev.off()
  quartz()
  plot(gps_sif_night$utm_easting_smooth, gps_sif_night$utm_northing_smooth, pch = 19, cex = 0.4, asp = 1, col = cols_sites[gps_sif_night$sleep_site_id], main = 'Sleep site ID (after majority filtering)')
  dev.copy2pdf(file = paste0('sleep_sites_after_majority_filtering_',year, '_',season, '.pdf'))
  dev.off()
  quartz()
  plot(gps_sif_night$utm_easting_smooth, gps_sif_night$utm_northing_smooth, pch = 19, cex = 0.4, asp = 1, col = cols_sites[gps_sif_night$animal_group_id], main = 'Sleep sites by group')
  dev.copy2pdf(file = paste0('sleep_sites_by_group_',year,'_',season, '.pdf'))
  dev.off()
}

# Generate a final data frame of sleep site ids and their centroids
sleep_sites <- data.frame(sleep_site_id = unique(gps_sif_night$sleep_site_id))
sleep_sites$centr_easting <- sleep_sites$centr_northing <- NA
for(i in 1:nrow(sleep_sites)){
  idxs <- which(gps_sif_night$sleep_site_id == sleep_sites$sleep_site_id[i])
  sleep_sites$centr_easting[i] <- mean(gps_sif_night$utm_easting_smooth[idxs], na.rm=T)
  sleep_sites$centr_northing[i] <- mean(gps_sif_night$utm_northing_smooth[idxs], na.rm=T)
}

# Generate a final data frame of individuals on each date and which sleep site id they were at
sleep_site_visits <- gps_sif_night[,c('individual_local_identifier','night_date','sleep_site_id')]
sleep_site_visits <- sleep_site_visits[-which(duplicated(sleep_site_visits)),]
colnames(sleep_site_visits) <- c('animal_id','night_date','sleep_site_id')
sleep_site_visits$centr_easting <- sleep_sites$centr_easting[match(sleep_site_visits$sleep_site_id, sleep_sites$sleep_site_id)]
sleep_site_visits$centr_northing <- sleep_sites$centr_northing[match(sleep_site_visits$sleep_site_id, sleep_sites$sleep_site_id)]
sleep_site_visits$animal_group_id <- depinfo$animal_group_id[match(sleep_site_visits$animal_id, depinfo$animal_id)]

#Get previous site
sleep_site_visits$prev_sleep_site <- NA
for(i in 1:nrow(sleep_site_visits)){
  date_curr <- sleep_site_visits$night_date[i]
  date_prev <- date_curr - 1
  ind <- sleep_site_visits$animal_id[i]
  idx_prev <- which(sleep_site_visits$animal_id==ind & sleep_site_visits$night_date==date_prev)
  if(length(idx_prev)>0){
    sleep_site_visits$prev_sleep_site[i] <- sleep_site_visits$sleep_site_id[idx_prev]
  }
}

#add a column for the individual's sleep site on the next night
sleep_site_visits$next_sleep_site <- NA
for(i in 1:nrow(sleep_site_visits)){
  next_night <- sleep_site_visits$night_date[i] + 1
  ind <- sleep_site_visits$animal_id[i]
  idx_next <- which(sleep_site_visits$animal_id==ind & sleep_site_visits$night_date==next_night)
  if(length(idx_next)==1){
    sleep_site_visits$next_sleep_site[i] <- sleep_site_visits$sleep_site_id[idx_next]
  }
}

# distance to next sleep site and prev sleep site
sleep_site_visits$dist_prev_sleep_site <- NA
sleep_site_visits$dist_next_sleep_site <- NA
for(i in 1:nrow(sleep_site_visits)){
  sleep_site_now <- sleep_site_visits$sleep_site_id[i]
  sleep_site_prev <- sleep_site_visits$prev_sleep_site[i]
  sleep_site_next <- sleep_site_visits$next_sleep_site[i]
  
  x_now <- sleep_sites$centr_easting[which(sleep_sites$sleep_site_id == sleep_site_now)]
  y_now <- sleep_sites$centr_northing[which(sleep_sites$sleep_site_id == sleep_site_now)]
  
  x_prev <- sleep_sites$centr_easting[which(sleep_sites$sleep_site_id == sleep_site_prev)]
  y_prev <- sleep_sites$centr_northing[which(sleep_sites$sleep_site_id == sleep_site_prev)]
  
  x_next <- sleep_sites$centr_easting[which(sleep_sites$sleep_site_id == sleep_site_next)]
  y_next <- sleep_sites$centr_northing[which(sleep_sites$sleep_site_id == sleep_site_next)]
  
  if(length(x_prev) > 0){
    sleep_site_visits$dist_prev_sleep_site[i] <- sqrt( (x_now - x_prev)^2 + (y_now - y_prev)^2)
  }
  if(length(x_next) > 0){
    sleep_site_visits$dist_next_sleep_site[i] <- sqrt( (x_now - x_next)^2 + (y_now - y_next)^2)
  }
}

# Time of arrival at sleep site
sleep_site_visits$ind_centr_easting <- NA
sleep_site_visits$ind_centr_northing <- NA
sleep_site_visits$ind_first_easting <- NA
sleep_site_visits$ind_first_northing <- NA
sleep_site_visits$ind_last_easting <- NA
sleep_site_visits$ind_last_northing <- NA
sleep_site_visits$ind_max_dist_centr <- NA
sleep_site_visits$leave_time <- sleep_site_visits$arrive_time <- ''
sleep_site_visits$arrive_time_NAs <- NA
sleep_site_visits$leave_time_NAs <- NA

for(i in 1:nrow(sleep_site_visits)){
  
  #get individual's centroid on each night
  animal_id <- sleep_site_visits$animal_id[i]
  night_date <- sleep_site_visits$night_date[i]
  id_night <- paste0(animal_id, '_', night_date)
  idxs <- which(gps_sif_night$id_night == id_night)
  sleep_site_visits$ind_centr_easting[i] <- mean(gps_sif_night$utm_easting_smooth[idxs], na.rm=T)
  sleep_site_visits$ind_centr_northing[i] <- mean(gps_sif_night$utm_northing_smooth[idxs], na.rm=T)
  
  #get individual's first and last position on each night
  sleep_site_visits$ind_first_easting[i] <- gps_sif_night$utm_easting_smooth[min(idxs)]
  sleep_site_visits$ind_first_northing[i] <- gps_sif_night$utm_northing_smooth[min(idxs)]
  sleep_site_visits$ind_last_easting[i] <- gps_sif_night$utm_easting_smooth[max(idxs)]
  sleep_site_visits$ind_last_northing[i] <- gps_sif_night$utm_northing_smooth[max(idxs)]
  
  #get maximum distance from centroid during the night
  dx <- gps_sif_night$utm_easting_smooth[idxs] - sleep_site_visits$ind_centr_easting[i]
  dy <- gps_sif_night$utm_northing_smooth[idxs] - sleep_site_visits$ind_centr_northing[i]
  dists <- sqrt(dx^2 + dy^2)
  sleep_site_visits$ind_max_dist_centr[i] <- max(dists, na.rm=T)
  
  #get night start and night end times for the current night (23:00 and 03:00)
  night_start <- min(gps_sif_night$study_local_timestamp[idxs])
  night_end <- max(gps_sif_night$study_local_timestamp[idxs])
  
  #get gps data for the current individual
  gps_sif_ind <- gps_sif[which(gps_sif$individual_local_identifier == animal_id),]
  
  #get the distance of the individual from the first and last night points (at 11pm and 3am)
  gps_sif_ind$dist_first_night_point <- sqrt((gps_sif_ind$utm_easting_smooth - sleep_site_visits$ind_first_easting[i])^2 + (gps_sif_ind$utm_northing_smooth - sleep_site_visits$ind_first_northing[i])^2)
  gps_sif_ind$dist_last_night_point <- sqrt((gps_sif_ind$utm_easting_smooth - sleep_site_visits$ind_last_easting[i])^2 + (gps_sif_ind$utm_northing_smooth - sleep_site_visits$ind_last_northing[i])^2)
  
  #get indexes before and after the current night
  idxs_before <- which(gps_sif_ind$study_local_timestamp <= night_start)
  idxs_after <- which(gps_sif_ind$study_local_timestamp >= night_end)
  
  #going forward from night_end_hour, when does the individaul first leave a radius of arrival_departure_radius from its last night time position?
  left_idxs <- which(gps_sif_ind$dist_last_night_point[idxs_after] > arrival_departure_radius)
  if(sum(!is.na(left_idxs))>0){
    sleep_site_visits$leave_time[i] <- as.character(gps_sif_ind$study_local_timestamp[min(idxs_after[left_idxs])])
  } else{
    sleep_site_visits$leave_time[i] <- NA
  }
  
  #going backward in time, when does the individual arrive
  arrived_idxs <- which(gps_sif_ind$dist_first_night_point[idxs_before] > arrival_departure_radius)
  if(sum(!is.na(arrived_idxs))>0){
    sleep_site_visits$arrive_time[i] <- as.character(gps_sif_ind$study_local_timestamp[max(idxs_before[arrived_idxs])])
  } else{
    sleep_site_visits$arrive_time[i] <- NA
    
  }
  
  #check how many NAs there are in between night end and leave time
  #and night start and arrive time
  #if there are any NAs (the number of expected fixes is less than the number of actually recorded fixes in the window between
  #night and and leave time or arrive time and night start) --> make that arrive or leave time NA
  leave_time <- as.POSIXct(sleep_site_visits$leave_time[i], tz = time_zone)
  arrive_time <- as.POSIXct(sleep_site_visits$arrive_time[i], tz = time_zone)
  if(!is.na(arrive_time)){
    dt_arrive <- as.numeric(difftime(night_start, arrive_time, units = 'min'))
    expected_fixes <- dt_arrive / min_per_fix
    n_fixes <- sum(gps_sif_ind$study_local_timestamp > arrive_time & gps_sif_ind$study_local_timestamp <= night_start)
    n_nas <- expected_fixes - n_fixes
    if(n_nas > 0){
      sleep_site_visits$arrive_time[i] <- NA
    }
  }
  if(!is.na(leave_time)){
    dt_leave <- as.numeric(difftime(leave_time, night_end, units = 'min'))
    expected_fixes <- dt_leave / min_per_fix
    n_fixes <- sum(gps_sif_ind$study_local_timestamp > night_end & gps_sif_ind$study_local_timestamp <= leave_time)
    n_nas <- expected_fixes - n_fixes
    if(n_nas > 0){
      sleep_site_visits$leave_time[i] <- NA
    }
  }
  
}

# get time of arrival at next sleep site (on the next day)
id_night <- paste0(sleep_site_visits$animal_id, '_', sleep_site_visits$night_date)
id_night_next <- paste0(sleep_site_visits$animal_id, '_', sleep_site_visits$night_date + 1)
sleep_site_visits$arrive_time_next <- sleep_site_visits$arrive_time[match(id_night_next, id_night)]

# Get sleep site usage overall by that individual across all nights (for current, previous, and next sleep site)
sleep_site_visits$curr_sleep_site_usage <- sleep_site_visits$next_sleep_site_usage <- sleep_site_visits$prev_sleep_site_usage <- NA
for(i in 1:nrow(sleep_site_visits)){
  animal_id <- sleep_site_visits$animal_id[i]
  usage <- table(sleep_site_visits$sleep_site_id[which(sleep_site_visits$animal_id == animal_id)])
  usage_df <- data.frame(sleep_site_id = names(usage), usage = c(usage / sum(usage)))
  if(!is.na(sleep_site_visits$sleep_site_id[i])){
    sleep_site_visits$curr_sleep_site_usage[i] <- usage_df$usage[which(usage_df$sleep_site_id == sleep_site_visits$sleep_site_id[i])] 
  }
  if(!is.na(sleep_site_visits$next_sleep_site[i])){
    sleep_site_visits$next_sleep_site_usage[i] <- usage_df$usage[which(usage_df$sleep_site_id == sleep_site_visits$next_sleep_site[i])] 
  }
  if(!is.na(sleep_site_visits$prev_sleep_site[i])){
    sleep_site_visits$prev_sleep_site_usage[i] <- usage_df$usage[which(usage_df$sleep_site_id == sleep_site_visits$prev_sleep_site[i])] 
  }
}

# Get distance to nearest neighboring group 
# computed as the distance between the current sleep tree and any other sleep tree currently in use
sleep_site_visits$nearest_outgroup_neighbor_group_id <- NA
sleep_site_visits$nearest_outgroup_neighbor_dist <- NA
for(i in 1:nrow(sleep_site_visits)){
  group_id <- sleep_site_visits$animal_group_id[i]
  night_date <- sleep_site_visits$night_date[i]
  centr_x <- sleep_site_visits$centr_easting[i]
  centr_y <- sleep_site_visits$centr_northing[i]
  
  outgroup_rows <- which(sleep_site_visits$animal_group_id != group_id & sleep_site_visits$night_date == night_date)
  dists <- (sleep_site_visits$centr_easting[outgroup_rows] - centr_x)^2 + (sleep_site_visits$centr_northing[outgroup_rows] - centr_y)^2
  min_dist <- min(dists, na.rm=T)
  sleep_site_visits$nearest_outgroup_neighbor_dist[i] <- min_dist
  sleep_site_visits$nearest_outgroup_neighbor_group_id[i] <- sleep_site_visits$animal_group_id[outgroup_rows[which(dists==min_dist)[1]]]
}


# #Get group spread during the night
# reference$deploy_off_date <- as.Date(reference$deploy_off_date)
# reference$deploy_on_date <- as.Date(reference$deploy_on_date)
# sleep_site_visits$mean_dyad_dist_night <- sleep_site_visits$mean_dyad_dist_day <- NA
# print('getting group spread (a bit slow):')
# for(i in 1:nrow(sleep_site_visits)){
#   print(paste0(i,'/',nrow(sleep_site_visits)))
#   
#   #get animal group id
#   animal_group_id <- sleep_site_visits$animal_group_id[i]
#   
#   #get date
#   night_date <- sleep_site_visits$night_date[i]
#   
#   #get animals in that group during that time
#   animal_ids <- reference$animal_id[which(reference$animal_group_id == animal_group_id &
#                                             reference$deploy_on_date <= night_date &
#                                             reference$deploy_off_date >= night_date)]
#   
#   #get night times
#   night_start <- as.POSIXct(paste0(sleep_site_visits$night_date[i], night_start_hour, ':00:00'), tz = time_zone)
#   night_end <- as.POSIXct(paste0(sleep_site_visits$night_date[i]+1, night_end_hour, ':00:00'), tz = time_zone)
#   
#   #get day times - from leave time to next arrive time at sleep site
#   day_start <- as.POSIXct(sleep_site_visits$leave_time[i], tz = time_zone)
#   day_end <- as.POSIXct(sleep_site_visits$arrive_time_next[i], tz = time_zone)
#   
#   #get mean dyadic distance at each time point - night
#   if(!is.na(night_start) & !is.na(night_end)){
#     times_night <- seq(night_start,night_end,by = '3 min')
#     means_night <- rep(NA, length(times_night))
#     for(t in 1:length(times_night)){
#       rows <- which(gps_sif$individual_local_identifier %in% animal_ids & gps_sif$study_local_timestamp == times_night[t])
#       mat <- cbind(gps_sif$utm_easting_smooth[rows], gps_sif$study_local_timestamp[rows])
#       means_night[t] <- mean(c(dist(mat)),na.rm=T)
#     }
#     #mean dyadic distance across the whole night (mean of means)
#     sleep_site_visits$mean_dyad_dist_night[i] <- mean(means_night, na.rm=T)
#   }
#   
#   #get mean dyadic distance at each time point - day
#   if(!is.na(day_start) & !is.na(day_end)){
#     times_day <- seq(day_start, day_end, by = '3 min')
#     means_day <- rep(NA, length(times_day))
#     for(t in 1:length(times_day)){
#       rows <- which(gps_sif$individual_local_identifier %in% animal_ids & gps_sif$study_local_timestamp == times_day[t])
#       mat <- cbind(gps_sif$utm_easting_smooth[rows], gps_sif$study_local_timestamp[rows])
#       means_day[t] <- mean(c(dist(mat)),na.rm=T)
#     }
#     #mean dyadic distance across the day (mean of means)
#     sleep_site_visits$mean_dyad_dist_day[i] <- mean(means_day, na.rm=T)
#   }
# 
# }

#Make an animation of group sleep site visits across nights
if(make_anim){
  night_dates <- sort(night_dates)
  col_groups <- rainbow(max(reference$animal_group_id, na.rm=T))
  xmin <- min(sleep_site_visits$centr_easting, na.rm=T) - 50
  ymin <- min(sleep_site_visits$centr_northing, na.rm=T) - 50
  xmax <- max(sleep_site_visits$centr_easting, na.rm=T) + 50
  ymax <- max(sleep_site_visits$centr_northing, na.rm=T) + 50
  for(i in 1:length(night_dates)){
    png(width = 1200, height = 1200, filename = paste0('sleep_site_viz/', i, '.png'))
    curr <- sleep_site_visits[which(sleep_site_visits$night_date == night_dates[i]),]
    plot(NULL, asp =1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), main = curr$night_date[1], xlab = 'Easting (m)', ylab = 'Northing (m)', cex.lab = 2, cex.main=2, cex.axis = 2)
    points(gps_sif_night$utm_easting_smooth, gps_sif_night$utm_northing_smooth, cex = 1, col = '#00000033', pch = 19)
    
    points(curr$centr_easting, curr$centr_northing, col = col_groups[curr$animal_group_id], cex = 4, pch = 19)
    points(sleep_sites$centr_easting, sleep_sites$centr_northing, col = 'black', cex = 4, pch = 1)
    
    idxs_stay <- which(curr$sleep_site_id == curr$prev_sleep_site)
    points(curr$centr_easting[idxs_stay], curr$centr_northing[idxs_stay], pch = 4, cex = 4, lwd=4, col = 'red')
    points(curr$centr_easting[idxs_stay], curr$centr_northing[idxs_stay], pch = 1, cex = 4, lwd=4, col = 'red')
    
    
    dev.off()
  }
}

#----Basic stats---
if(print_basic_stats){
  #Check how often two groups on the same night share a sleep site
  night_dates <- unique(gps_sif_night$night_date)
  any_sites_shared <- rep(F, length(night_dates))
  for(i in 1:length(night_dates)){
    idxs <- which(gps_sif_night$night_date == night_dates[i])
    curr <- gps_sif_night[idxs,]
    sites_curr <- unique(curr$sleep_site_id)
    for(s in 1:length(sites_curr)){
      groups_at_site <- length(unique(curr$animal_group_id[which(curr$sleep_site_id==sites_curr[s])]))
      if(groups_at_site > 1){
        any_sites_shared[i] <- T
      }
    }
  }
  print('% of time that groups shared a site = ')
  print(mean(any_sites_shared, na.rm=T)*100)
  
  #How often do groups split across sites?
  night_dates <- unique(gps_sif_night$night_date)
  sites_per_group <- c()
  for(i in 1:length(night_dates)){
    idxs <- which(gps_sif_night$night_date == night_dates[i])
    curr <- gps_sif_night[idxs,]
    groups_curr <- unique(curr$animal_group_id)
    for(g in 1:length(groups_curr)){
      sites_per_group <- c(sites_per_group, length(unique(curr$sleep_site_id[which(curr$animal_group_id==groups_curr[g])])))
      
    }
  }
  print('% of times that individuals in a group split across sites:')
  print(mean(sites_per_group > 1, na.rm=T)*100)
  
  #How often do they switch sleep sites?
  print('What fraction of the time do they switch sleep sites?')
  print(mean(sleep_site_visits$sleep_site_id!=sleep_site_visits$prev_sleep_site, na.rm=T)*100)
  
  #How many sleep sites per group?
  groups <- sort(unique(sleep_site_visits$animal_group_id))
  n_sleep_sites <- rep(NA, length(groups))
  for(i in 1:length(groups)){
    sites <- sleep_site_visits$sleep_site_id[which(sleep_site_visits$animal_group_id == groups[i])]
    n_sleep_sites[i] <- length(unique(sites))
  }
  print('Number of sleep sites used per group')
  print(cbind(groups, n_sleep_sites))
}




# calculate arrival and departure times relative to sunset and sunrise

sleep_site_visits <- sleep_site_visits %>%
  rename(date = night_date)

# Calculate sunrise and sunset times for each date.
sun_times = f_get_suntimes(sleep_site_visits$date, -20.783, 44.174, "Indian/Antananarivo")

# Add sunrise and sunset times for the date on which the night begins.
sleep_site_visits <- sleep_site_visits %>%
  left_join(sun_times %>% select(date, sunrise, sunset), 
            by = c("date"))

# Shift solar dates backward by one day to align the following day's
# sunrise and sunset with the date on which the night begins.
sun_times$date <- as.Date(as.POSIXct(sun_times$date, format="%Y-%m-%d") - days(1))
sun_times <- sun_times %>% 
  rename(
    sunrise_next = sunrise,
    sunset_next = sunset
  )
sleep_site_visits <- sleep_site_visits %>%
  left_join(sun_times %>% select(date, sunrise_next, sunset_next), 
            by = c("date"))

# Calculate arrival time relative to sunset, in minutes.
# Arrival time - sunset: negative values indicate arrival before sunset,
# and positive values indicate arrival after sunset.
sleep_site_visits$arrive_minutes <-
  as.numeric(difftime(as.POSIXct(sleep_site_visits$arrive_time, format="%Y-%m-%d %H:%M:%S"), 
                      as.POSIXct(sleep_site_visits$sunset, format="%Y-%m-%d %H:%M:%S"), 
                      units = "mins"))

# Calculate departure time relative to the following day's sunrise, in minutes.
# Departure time - sunrise: negative values indicate departure before sunrise,
# and positive values indicate departure after sunrise.
sleep_site_visits$leave_minutes <-
  as.numeric(difftime(as.POSIXct(sleep_site_visits$leave_time, format="%Y-%m-%d %H:%M:%S"), 
                      as.POSIXct(sleep_site_visits$sunrise_next, format="%Y-%m-%d %H:%M:%S"), 
                      units = "mins"))

# See summary
nrow(sleep_site_visits) #n of sifaka nights
#2022: 248, 2023: 664
length(unique(sleep_site_visits$animal_id)) #n individuals
#2022: 17, 2023: 24
length(unique(sleep_site_visits$animal_group_id)) #n groups
#2022: 8, 2023: 13
length(unique(sleep_site_visits$date)) #n days
#2022: 15, 2023: 28
nrow(sleep_sites)
#2022: 52, 2023: 106

# Save the sleep sites table and sleep sites visit table ----------------------------------
save(list = c('sleep_sites','sleep_site_visits', 'params'), file = paste0('sleep_sites_',year,'_',season, '.RData'))

write.csv(sleep_site_visits, file = paste0('sleep_sites_',year,'_',season, '.csv'), row.names = FALSE)

