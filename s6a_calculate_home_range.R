#'----------------------------------------------------------------------------------------------------
#' Script: s6a_calculate_home_range.R
#'
#' Calculate AKDE home ranges using CTMM
#'
#' @author Tracy Montgomery (primary code author)
#' @author Maevatiana Ratsimbazafindranahaka (code reviewer)
#-----------------------------------------------------------------------------------------------------

##### 1: Set up workspace #####

# Set options
rm(list = ls())
options(stringsAsFactors = F)
options(digits.secs = 3)
time_zone = "Indian/Antananarivo"

# Set directories
wdir <- "C:/Users/maevatiana.ratsim/Documents/Projects/Sifaka/Sifosa-landscape-of-fear-datasets/"
setwd(wdir)
gps.data.dir <- "GPS/"
processed.data.dir <- "s6/"

# Install development version of ctmm
# devtools::install_github("ctmm-initiative/ctmm")

# Load libraries
library(tidyverse)
library(parallel)
library(sf)
library(ctmm)

# Load data
sifaka.22.3min <- fread(paste0(gps.data.dir, "sifaka.22.3min.csv"))
sifaka.23.3min <- fread(paste0(gps.data.dir, "sifaka.23.3min.csv"))

# Fix timezone
message(sprintf(
  "The current local time (e.g., first row = %s) was loaded assuming that it is %s. It will now be forced to be %s without changing the values.",
  sifaka.22.3min$study_local_timestamp[1], tz(sifaka.22.3min$study_local_timestamp[1]), time_zone))
sifaka.22.3min$study_local_timestamp <- force_tz(sifaka.22.3min$study_local_timestamp, tzone = time_zone)
sifaka.23.3min$study_local_timestamp <- force_tz(sifaka.23.3min$study_local_timestamp, tzone = time_zone)
head(sifaka.22.3min$study_local_timestamp) # first timestamp should be “2022-06-24 04:00:00 EAT” 
head(sifaka.23.3min$study_local_timestamp) # first timestamp should be “2023-06-25 03:00:00 EAT”

##### 2: Convert to lat/long #####

# Convert smoothed UTM coordinates (zone 38S, EPSG:32738) to longitude/latitude (WGS84, EPSG:4326)
for (j in c("sifaka.22.3min", "sifaka.23.3min")) {

  # Get data
  data <- get(j)

  # Project coordinates (missing locations come back as NaN, so set them to NA)
  lonlat <- sf_project(from = "EPSG:32738", to = "EPSG:4326",
                       pts = cbind(data$utm_easting_smooth, data$utm_northing_smooth))
  data$location_long <- ifelse(is.nan(lonlat[, 1]), NA, lonlat[, 1])
  data$location_lat <- ifelse(is.nan(lonlat[, 2]), NA, lonlat[, 2])

  # Save
  assign(j, data)
}
rm(data, lonlat)


##### 3: Create CTMMs #####

### Create ctmms for separate dataset

# Convert to CTMM
datasets <- c("22.3min", "23.3min")

for (j in datasets) {

  # Get data
  data.orig <- get(paste0("sifaka.", j))

  # Arrange data
  data.orig <- arrange(data.orig, individual_local_identifier, study_local_timestamp,)

  # Convert column names
  data.orig$individual.local.identifier <- data.orig$individual_local_identifier
  data.orig$timestamp <- data.orig$study_local_timestamp
  data.orig$location.long <- data.orig$location_long
  data.orig$location.lat <- data.orig$location_lat

  # Convert to telemetry object
  data.ctmm <- as.telemetry(data.orig)

  # Save
  assign(paste0("sifaka.", j, ".ctmm"), data.ctmm)
}


##### 4: Fit models and calculate AKDE #####

FITs <- list()
UDs <- list()

# Change dataset below to whichever one you want to run
start.time <- Sys.time()
for (i in 1:length(sifaka.23.3min.ctmm)) {

  # Get individual data
  data.ctmm <- sifaka.23.3min.ctmm[[i]]

  # Automated guesstimate for calibrated data - get starting values for models
  GUESS <- ctmm.guess(data.ctmm, interactive = FALSE)

  # Fit movement models and select top one based on AIC
  FIT <- ctmm.select(data.ctmm, GUESS)
  summary(FIT)

  # Calculate home range
  UD <- akde(data.ctmm, FIT, grid = list(dr = 1, align.to.origin = TRUE))
  summary(UD)$CI

  # Save
  FITs[[i]] <- FIT
  UDs[[i]] <- UD

  # Remove
  rm(data.ctmm, GUESS, FIT, UD)

}
end.time <- Sys.time()
print(end.time - start.time)
# 2022: 1.09 hours
# 2023: 2.89 hours

# Assign names
names(FITs) <- names(sifaka.23.3min.ctmm)
names(UDs) <- names(sifaka.23.3min.ctmm)

# Check data - change dataset
plot(sifaka.23.3min.ctmm[["S02"]], UD = UDs[["S02"]])

# Save
# FITs.22 <- FITs
# UDs.22 <- UDs
FITs.23 <- FITs
UDs.23 <- UDs
rm(UDs, FITs)

# save(list = c("FITs.22", "UDs.22", "FITs.23", "UDs.23"),
#      file = paste0(processed.data.dir, "ctmm_fits_akdes.Rdata"))


##### 4: Calculate group AKDEs #####

### 2022 ###

sort(unique(sifaka.22.3min$group_local_identifier))

unique(sifaka.22.3min[sifaka.22.3min$group_local_identifier == "G01",]$individual_local_identifier)
akde.22.grp01 <- mean(UDs.22[c('S11', 'S19')])
plot(akde.22.grp01)

unique(sifaka.22.3min[sifaka.22.3min$group_local_identifier == "G05",]$individual_local_identifier)
akde.22.grp05 <- mean(UDs.22[c('S03', 'S05', 'S24', 'S26')])
plot(akde.22.grp05)

unique(sifaka.22.3min[sifaka.22.3min$group_local_identifier == "G06",]$individual_local_identifier)
akde.22.grp06 <- mean(UDs.22[c('S08', 'S14', 'S27')])
plot(akde.22.grp06)

unique(sifaka.22.3min[sifaka.22.3min$group_local_identifier == "G07",]$individual_local_identifier)
akde.22.grp07 <- mean(UDs.22[c('S10', 'S16')])
plot(akde.22.grp07)

unique(sifaka.22.3min[sifaka.22.3min$group_local_identifier == "G10",]$individual_local_identifier)
akde.22.grp10 <- mean(UDs.22[c('S07', 'S13')])
plot(akde.22.grp10)

unique(sifaka.22.3min[sifaka.22.3min$group_local_identifier == "G12",]$individual_local_identifier)
akde.22.grp12 <- mean(UDs.22[c('S06', 'S21', 'S28')])
plot(akde.22.grp12)

unique(sifaka.22.3min[sifaka.22.3min$group_local_identifier == "G13",]$individual_local_identifier)
akde.22.grp13 <- mean(UDs.22[c('S01', 'S15')])
plot(akde.22.grp13)

unique(sifaka.22.3min[sifaka.22.3min$group_local_identifier == "G14",]$individual_local_identifier)
akde.22.grp14 <- mean(UDs.22[c('S22', 'S32')])
plot(akde.22.grp14)


### 2023 ###

sort(unique(sifaka.23.3min$group_local_identifier))

unique(sifaka.23.3min[sifaka.23.3min$group_local_identifier == "G01",]$individual_local_identifier)
akde.23.grp01 <- mean(UDs.23[c('S11', 'S19')])
plot(akde.23.grp01)

unique(sifaka.23.3min[sifaka.23.3min$group_local_identifier == "G02",]$individual_local_identifier)
akde.23.grp02 <- mean(UDs.23[c('S12', 'S18')])
plot(akde.23.grp02)

unique(sifaka.23.3min[sifaka.23.3min$group_local_identifier == "G03",]$individual_local_identifier)
akde.23.grp03 <- mean(UDs.23[c('S02')])
plot(akde.23.grp03)

unique(sifaka.23.3min[sifaka.23.3min$group_local_identifier == "G04",]$individual_local_identifier)
akde.23.grp04 <- mean(UDs.23[c('S29')])
plot(akde.23.grp04)

unique(sifaka.23.3min[sifaka.23.3min$group_local_identifier == "G05",]$individual_local_identifier)
akde.23.grp05 <- mean(UDs.23[c('S03', 'S05', 'S24')])
plot(akde.23.grp05)

unique(sifaka.23.3min[sifaka.23.3min$group_local_identifier == "G06",]$individual_local_identifier)
akde.23.grp06 <- mean(UDs.23[c('S08', 'S14', 'S27')])
plot(akde.23.grp06)

unique(sifaka.23.3min[sifaka.23.3min$group_local_identifier == "G07",]$individual_local_identifier)
akde.23.grp07 <- mean(UDs.23[c('S10', 'S16')])
plot(akde.23.grp07)

unique(sifaka.23.3min[sifaka.23.3min$group_local_identifier == "G08",]$individual_local_identifier)
akde.23.grp08 <- mean(UDs.23[c('S04')])
plot(akde.23.grp08)

unique(sifaka.23.3min[sifaka.23.3min$group_local_identifier == "G09",]$individual_local_identifier)
akde.23.grp09 <- mean(UDs.23[c('S25', 'S30', 'S31')])
plot(akde.23.grp09)

unique(sifaka.23.3min[sifaka.23.3min$group_local_identifier == "G11",]$individual_local_identifier)
akde.23.grp11 <- mean(UDs.23[c('S09', 'S17')])
plot(akde.23.grp11)

unique(sifaka.23.3min[sifaka.23.3min$group_local_identifier == "G12",]$individual_local_identifier)
akde.23.grp12 <- mean(UDs.23[c('S06', 'S21', 'S28')])
plot(akde.23.grp12)

unique(sifaka.23.3min[sifaka.23.3min$group_local_identifier == "G13",]$individual_local_identifier)
akde.23.grp13 <- mean(UDs.23[c('S15', 'S20', 'S23')])
plot(akde.23.grp13)

unique(sifaka.23.3min[sifaka.23.3min$group_local_identifier == "G14",]$individual_local_identifier)
akde.23.grp14 <- mean(UDs.23[c('S22', 'S32')])
plot(akde.23.grp14)


##### 5: Plot AKDEs for season to check #####

all.22 <- lapply(grep("akde.22", ls(), value = TRUE), get)
plot(all.22)

all.23 <- lapply(grep("akde.23", ls(), value = TRUE), get)
plot(all.23)


##### 6: Save as shapefile #####

for (i in 1:length(grep("akde", ls(), value = TRUE))) {
  ud <- get(grep("akde", ls(), value = TRUE)[i])
  st_write(as.sf(ud), paste0(processed.data.dir, "hr_shp/",
                             grep("akde", ls(), value = TRUE)[i], ".shp"),
           append = F)

}




