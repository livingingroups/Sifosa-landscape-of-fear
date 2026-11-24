#'----------------------------------------------------------------------------------------------------
#' Script: s7_test_risk_responses.R
#'
#' Run models on sifaka daily/nightly behavior
#'
#' @author Tracy Montgomery (primary code author)
#' @author Meredith Lutz (code reviewer)
#-----------------------------------------------------------------------------------------------------

##### 1: Set up workspace and data #####

# Set options
rm(list = ls())
options(stringsAsFactors = F)
options(digits.secs = 3)

# Set directories
wdir <- "C:/Users/maevatiana.ratsim/Documents/Projects/Sifaka/Sifosa-landscape-of-fear-datasets/"
setwd(wdir)
processed.data.dir <- "s5/"
plots.dir <- "s7/plots/"

# Load libraries
library(janitor)
library(GGally)
library(performance)
library(glmmTMB)
library(DHARMa)
library(broom.mixed)
library(tidyverse)

# Convert character columns to lower case & remove white-space
FormatAllCharacterStrings <- function(df) {
  df <- data.frame(lapply(df, function(variables) {
    if (is.character(variables)) {
      return(str_squish(tolower(enc2utf8(variables))))
    } else {
      return(variables)
    }
  }),
  stringsAsFactors = FALSE)
}

# Convert spaces to NAs
empty_as_na <- function(df) {
  df <- data.frame(lapply(df, function(variables) {
    if (is.character(variables)) {
      return(ifelse(as.character(variables) != "", variables, NA))
    } else {
      return(variables)
    }
  }),
  stringsAsFactors = FALSE)
}

# Load sifaka data
combined <- read.csv(paste0(processed.data.dir, "combined_metrics_all.csv"))


##### 2: Clean data #####

# Clean data
combined <- clean_names(combined)
combined <- FormatAllCharacterStrings(combined)
combined <- empty_as_na(combined)
combined %>% dplyr::select_at(setdiff(names(.), names(remove_empty(.)))) %>% unique() #find empty columns
combined %>% dplyr::select_at(setdiff(names(.), names(remove_constant(.)))) %>% unique() #find constant columns
get_one_to_one(combined) #okay
get_dupes(combined)

# Remove first night and last day
### NOTE: "2022-06-24 03:00:00" would be the night of 2022-06-23
combined <- filter(combined, !(date == "2022-06-23") & !(date == "2022-07-09") &
                     !(date == "2023-06-24") & !(date == "2023-07-23")) # remove 192
summary(combined) # fosa_detect_rate and pair_presence have no NAs, everything else does

# Split by day_night
daily.day <- combined[combined$day_night == "day",]
daily.day <- unique(daily.day)
daily.night <- combined[combined$day_night == "night",]
daily.night <- unique(daily.night)

# Make dyadic dataset
daily.day.ddd <- daily.day[,c("join_year", "date", "day_night", "group_id",
                              "ddd", "fosa_detect_rate", "pair_presence")]
daily.day.ddd <- unique(daily.day.ddd)
daily.night.ddd <- daily.night[,c("join_year", "date", "day_night", "group_id",
                                  "ddd", "fosa_detect_rate", "pair_presence")]
daily.night.ddd <- unique(daily.night.ddd)


##### 3: Look at data distributions #####

# PREDICTOR data distributions
hist(daily.day$fosa_detect_rate, breaks = "FD")
length(unique(daily.day$fosa_detect_rate)) # 8 rates
hist(daily.day$pair_presence, breaks = "FD")

hist(daily.night$fosa_detect_rate, breaks = "FD")
length(unique(daily.night$fosa_detect_rate)) # 21 rates
hist(daily.night$pair_presence, breaks = "FD")

# DAY data distributions
hist(daily.day$path_length, breaks = "FD")
daily.day[daily.day$path_length > 2800 &
            !is.na(daily.day$path_length),]$path_length <- NA # remove 1 outlier
hist(daily.day$path_length, breaks = "FD")
hist(daily.day$straightness, breaks = "FD")
hist(daily.day$vedba, breaks = "FD")
hist(daily.day$arrive_minutes, breaks = "FD")
daily.day[daily.day$arrive_minutes < -(12*60) &
            !is.na(daily.day$arrive_minutes),]$arrive_minutes <- NA # remove 9 animals who never left
hist(daily.day$arrive_minutes, breaks = "FD")

hist(daily.day.ddd$ddd, breaks = "FD")
daily.day.ddd[daily.day.ddd$ddd > 180 &
                !is.na(daily.day.ddd$ddd),]$ddd <- NA # remove 1 outlier
hist(daily.day.ddd$ddd, breaks = "FD")

# NIGHT data distributions
hist(daily.night$headup, breaks = "FD") # proportion
hist(daily.night$tst_la, breaks = "FD")
hist(daily.night$vedba, breaks = "FD")
hist(daily.night$leave_minutes, breaks = "FD")
daily.night[daily.night$leave_minutes > 12*60 &
              !is.na(daily.night$leave_minutes),]$leave_minutes <- NA # remove 12 animals who never left
hist(daily.night$leave_minutes, breaks = "FD")

hist(daily.night.ddd$ddd, breaks = "FD")
daily.night.ddd[daily.night.ddd$ddd > 180 &
                  !is.na(daily.night.ddd$ddd),]$ddd <- NA # remove 1 outlier
hist(daily.night.ddd$ddd, breaks = "FD")

# Subset data and remove rows where all NA
cols_check <- c("path_length", "straightness", "vedba", "arrive_minutes")

daily.day <- daily.day %>%
  filter(!if_all(all_of(cols_check), ~ is.na(.) | . == "")) # remove 17

cols_check <- c( "headup", "tst_la", "vedba", "leave_minutes")

daily.night <- daily.night %>%
  filter(!if_all(all_of(cols_check), ~ is.na(.) | . == "")) # remove 15

cols_check <- c("ddd")

daily.day.ddd <- daily.day.ddd %>%
  filter(!if_all(all_of(cols_check), ~ is.na(.) | . == "")) # remove 87

daily.night.ddd <- daily.night.ddd %>%
  filter(!if_all(all_of(cols_check), ~ is.na(.) | . == "")) # remove 139

# Scale data for comparison and create factors
daily.day$path_length_scaled <- scale(daily.day$path_length)[,1]
daily.day$straightness_scaled <- scale(daily.day$straightness)[,1]
daily.day$vedba_scaled <- scale(daily.day$vedba)[,1]
daily.day$arrive_minutes_scaled <- scale(daily.day$arrive_minutes)[,1]
daily.day$fosa_detect_rate_scaled <- scale(daily.day$fosa_detect_rate)[,1]
daily.day$pair_presence_scaled <- scale(daily.day$pair_presence)[,1]
daily.day$date_factor <- as.factor(daily.day$date)
daily.day$animal_id <- as.factor(daily.day$animal_id)
daily.day$group_id <- as.factor(daily.day$group_id)
daily.day$join_year <- as.factor(daily.day$join_year)

daily.night$headup_scaled <- scale(daily.night$headup)[,1]
daily.night$tst_la_scaled <- scale(daily.night$tst_la)[,1]
daily.night$vedba_scaled <- scale(daily.night$vedba)[,1]
daily.night$leave_minutes_scaled <- scale(daily.night$leave_minutes)[,1]
daily.night$fosa_detect_rate_scaled <- scale(daily.night$fosa_detect_rate)[,1]
daily.night$pair_presence_scaled <- scale(daily.night$pair_presence)[,1]
daily.night$date_factor <- as.factor(daily.night$date)
daily.night$animal_id <- as.factor(daily.night$animal_id)
daily.night$group_id <- as.factor(daily.night$group_id)
daily.night$join_year <- as.factor(daily.night$join_year)

daily.day.ddd$ddd_scaled <- scale(daily.day.ddd$ddd)[,1]
daily.day.ddd$ddd_log_scaled <- scale(log(daily.day.ddd$ddd))[,1]
daily.day.ddd$fosa_detect_rate_scaled <- scale(daily.day.ddd$fosa_detect_rate)[,1]
daily.day.ddd$pair_presence_scaled <- scale(daily.day.ddd$pair_presence)[,1]
daily.day.ddd$date_factor <- as.factor(daily.day.ddd$date)
daily.day.ddd$group_id <- as.factor(daily.day.ddd$group_id)
daily.day.ddd$join_year <- as.factor(daily.day.ddd$join_year)

daily.night.ddd$ddd_scaled <- scale(daily.night.ddd$ddd)[,1]
daily.night.ddd$ddd_log_scaled <- scale(log(daily.night.ddd$ddd))[,1]
daily.night.ddd$fosa_detect_rate_scaled <- scale(daily.night.ddd$fosa_detect_rate)[,1]
daily.night.ddd$pair_presence_scaled <- scale(daily.night.ddd$pair_presence)[,1]
daily.night.ddd$date_factor <- as.factor(daily.night.ddd$date)
daily.night.ddd$group_id <- as.factor(daily.night.ddd$group_id)
daily.night.ddd$join_year <- as.factor(daily.night.ddd$join_year)


##### 4: DAYTIME MODELS #####

# Response options
## vedba_scaled
#### Look at variables:
###### Outliers = possible > 4
###### join_year = no
###### fosa_detect_rate = no
###### pair_presence = no
#### Look at random effects:
###### (1 | group_id) - Adjusted ICC: 0.210
###### (1 | animal_id) -  Adjusted ICC: 0.357
###### (1 | date_factor) - Adjusted ICC: 0.032
###### (1 | group_id/animal_id) + (1 | date_factor) - Adjusted ICC: 0.396
#### Final model:
###### (1 | group_id/animal_id) + (1 | date_factor) - Adjusted ICC: 0.396
###### Look at family: Student-t df = 12.3932
###### Look at variables:
######## fosa_detect_rate = no
######## pair_presence = no
######## join_year = no
###### Look at residuals: good

## arrive_minutes_scaled
#### Look at variables:
###### Outliers = possible > 4
###### join_year = no
###### fosa_detect_rate = maybe
###### pair_presence = no
#### Look at random effects:
###### (1 | group_id) - Adjusted ICC: 0.117
###### (1 | animal_id) -  Adjusted ICC: 0.121
###### (1 | date_factor) - Adjusted ICC: 0.016
###### (1 | group_id/animal_id) + (1 | date_factor) - Adjusted ICC: NA
######## animal_id and group_id nearly identical, use group_id to match leave_minutes_scaled
#### Final model:
###### (1 | group_id) + (1 | date_factor) - Adjusted ICC: 0.156
###### Look at family: Student-t df = 1.848449
###### Look at variables:
######## fosa_detect_rate = no
######## pair_presence = no
######## join_year = no
###### Look at residuals: mediocre but okay

## path_length_scaled
#### Look at variables:
###### Outliers = possible > 4
###### join_year = yes
###### fosa_detect_rate = no
###### pair_presence = no
#### Look at random effects:
###### (1 | group_id) - Adjusted ICC: 0.185
###### (1 | animal_id) -  Adjusted ICC: 0.258
###### (1 | date_factor) - Adjusted ICC: 0.004
###### (1 | group_id/animal_id) + (1 | date_factor) - Adjusted ICC: 0.272
#### Final model:
###### (1 | group_id/animal_id) + (1 | date_factor) - Adjusted ICC: 0.272
###### Look at family: Student-t df = 5.848089
###### Look at variables:
######## fosa_detect_rate = no
######## pair_presence = no
######## join_year = yes
###### Look at residuals: good

## straightness_scaled
#### Look at variables:
###### Outliers = no
###### join_year = maybe
###### fosa_detect_rate = no
###### pair_presence = no
#### Look at random effects:
###### (1 | group_id) - Adjusted ICC: 0.034
###### (1 | animal_id) -  Adjusted ICC: 0.509
###### (1 | date_factor) - Adjusted ICC: 0.008
###### (1 | group_id/animal_id) + (1 | date_factor) - Adjusted ICC: NA
######## Little variance explained by group_id - drop
#### Final model:
###### (1 | animal_id) + (1 | date_factor) - Adjusted ICC: 0.519
###### Look at family: Student-t df = 5.523174
###### Look at variables:
######## fosa_detect_rate = no
######## pair_presence = no
######## join_year = yes
###### Look at residuals: good

# Set up for models - CHANGE HERE
tmp <- daily.day[complete.cases(daily.day[,c("join_year", "arrive_minutes_scaled",
                                             "fosa_detect_rate_scaled", "pair_presence_scaled",
                                             "animal_id", "group_id", "date_factor")]),]
tmp$response <- tmp$arrive_minutes_scaled

# Look at variables
hist(tmp$response, breaks = "FD")
boxplot(tmp$response ~ tmp$join_year)
ggplot(data = tmp, aes(x = as.Date(date), y = response)) +
  facet_wrap(~join_year, scales = "free_x") +
  geom_point() +
  geom_smooth()
ggplot(data = tmp, aes(x = fosa_detect_rate_scaled, y = response)) +
  geom_jitter() +
  geom_smooth(method = "lm")
boxplot(response ~ pair_presence_scaled, data = tmp)

mod <- glmmTMB(response ~ fosa_detect_rate_scaled + pair_presence_scaled +
                 join_year + (1 | group_id) + (1 | date_factor), # (1 | group_id/animal_id) + (1 | date_factor),
               data = tmp, family = t_family())

icc(mod)
VarCorr(mod)
family_params(mod)
summary(mod)
## For reference: (t_family)
# df > 30 → essentially Gaussian, t_family unnecessary
# df 10–30 → mildly heavy tails, modest improvement
# df 3–10 → good range — genuine heavy tails, t_family clearly justified
# df < 3 → extremely heavy tails, worth investigating outliers

simulationOutput <- simulateResiduals(fittedModel = mod, plot = F)
plot(simulationOutput)
plotResiduals(simulationOutput, tmp$fosa_detect_rate_scaled, asFactor = F)
plotResiduals(simulationOutput, tmp$pair_presence_scaled)
plot(tmp$response ~ predict(mod), asp = 1)
abline(a = 0, b = 1)

# Clean up
rm(tmp, mod, simulationOutput)


##### 5: NIGHTTIME MODELS #####

# Response options
## vedba_scaled
#### Look at variables:
###### Outliers = possible > 4
###### join_year = yes
###### fosa_detect_rate = no
###### pair_presence = no
#### Look at random effects:
###### (1 | group_id) - Adjusted ICC: 0.124
###### (1 | animal_id) -  Adjusted ICC: 0.202
###### (1 | date_factor) - Adjusted ICC: 0.100
###### (1 | group_id/animal_id) + (1 | date_factor) - Adjusted ICC: 0.275
#### Final model:
###### (1 | group_id/animal_id) + (1 | date_factor) - Adjusted ICC: 0.275
###### Look at family: Student-t df = 3.94412
###### Look at variables:
######## fosa_detect_rate = no
######## pair_presence = no
######## join_year = yes
###### Look at residuals: okay

## leave_minutes_scaled
#### Look at variables:
###### Outliers = possible > 4
###### join_year = no
###### fosa_detect_rate = no
###### pair_presence = no
#### Look at random effects:
###### (1 | group_id) - Adjusted ICC: 0.083
###### (1 | animal_id) -  Adjusted ICC: 0.065
###### (1 | date_factor) - Adjusted ICC: 0.093
###### (1 | group_id/animal_id) + (1 | date_factor) - Adjusted ICC: NA
######## No variance explained by animal_id - drop
#### Final model:
###### (1 | group_id) + (1 | date_factor) - Adjusted ICC: 0.167
###### Look at family: Student-t df = 2.23724
###### Look at variables:
######## fosa_detect_rate = no
######## pair_presence = maybe
######## join_year = yes
###### Look at residuals: mediocre but okay

## tst_la_scaled
#### Look at variables:
###### Outliers = possible < -6
###### join_year = yes
###### fosa_detect_rate = no
###### pair_presence = no
#### Look at random effects:
###### (1 | group_id) - Adjusted ICC: 0.066
###### (1 | animal_id) -  Adjusted ICC: 0.101
###### (1 | date_factor) - Adjusted ICC: 0.172
###### (1 | group_id/animal_id) + (1 | date_factor) - Adjusted ICC: 0.228
#### Final model:
###### (1 | group_id/animal_id) + (1 | date_factor) - Adjusted ICC: 0.228
###### Look at family: Student-t df = 2.954118
###### Look at variables:
######## fosa_detect_rate = no
######## pair_presence = no
######## join_year = yes
###### Look at residuals: mediocre but okay

## headup_scaled
#### Look at variables:
###### Outliers = possible > 4
###### join_year = no
###### fosa_detect_rate = no
###### pair_presence = no
#### Look at random effects:
###### (1 | group_id) - Adjusted ICC: 0.003
###### (1 | animal_id) -  Adjusted ICC: 0.005
###### (1 | date_factor) - Adjusted ICC: 0.015
###### (1 | group_id/animal_id) + (1 | date_factor) - Adjusted ICC: 0.020
#### Final model:
###### (1 | group_id/animal_id) + (1 | date_factor) - Adjusted ICC: 0.020
###### Look at family: Student-t df = 2.246518
###### Look at variables:
######## fosa_detect_rate = no
######## pair_presence = no
######## join_year = no
###### Look at residuals: mediocre

# Set up for models - CHANGE HERE
tmp <- daily.night[complete.cases(daily.night[,c("join_year", "headup_scaled",
                                                 "fosa_detect_rate_scaled", "pair_presence_scaled",
                                                 "animal_id", "group_id", "date_factor")]),]
tmp$response <- tmp$headup_scaled

# Look at variables
hist(tmp$response, breaks = "FD")
boxplot(tmp$response ~ tmp$join_year)
ggplot(data = tmp, aes(x = as.Date(date), y = response)) +
  facet_wrap(~join_year, scales = "free_x") +
  geom_point() +
  geom_smooth()
ggplot(data = tmp, aes(x = fosa_detect_rate_scaled, y = response)) +
  geom_jitter() +
  geom_smooth(method = "lm")
boxplot(response ~ pair_presence_scaled, data = tmp)

mod <- glmmTMB(response ~ fosa_detect_rate_scaled + pair_presence_scaled +
                 join_year + (1 | group_id/animal_id) + (1 | date_factor), # (1 | group_id/animal_id) + (1 | date_factor),
               data = tmp, family = t_family())

icc(mod)
VarCorr(mod)
family_params(mod)
summary(mod)
## For reference: (t_family)
# df > 30 → essentially Gaussian, t_family unnecessary
# df 10–30 → mildly heavy tails, modest improvement
# df 3–10 → good range — genuine heavy tails, t_family clearly justified
# df < 3 → extremely heavy tails, worth investigating outliers

simulationOutput <- simulateResiduals(fittedModel = mod, plot = F)
plot(simulationOutput)
plotResiduals(simulationOutput, tmp$fosa_detect_rate_scaled, asFactor = F)
plotResiduals(simulationOutput, tmp$pair_presence_scaled)
plot(tmp$response ~ predict(mod), asp = 1)
abline(a = 0, b = 1)

# Clean up
rm(tmp, mod, simulationOutput)


# ##### 6: DAILY DYADIC MODELS #####
#
# # Response options
# ## DAYTIME ddd_log_scaled
# #### Look at variables:
# ###### Outliers = possible > 4
# ###### join_year = no
# ###### fosa_detect_rate = no
# ###### pair_presence = no
# #### Look at random effects:
# ###### (1 | group_id) - Adjusted ICC: 0.629
# ###### (1 | date_factor) - Adjusted ICC: NA
# ###### (1 | group_id) + (1 | date_factor) - Adjusted ICC: 0.642
# #### Final model:
# ###### (1 | group_id) + (1 | date_factor) - Adjusted ICC: 0.642
# ###### Look at family: Student-t df = 2.572349
# ###### Look at variables:
# ######## fosa_detect_rate = no
# ######## pair_presence = no
# ######## join_year = yes
# ###### Look at residuals: good
#
# ## NIGHTTIME ddd_log_scaled
# #### Look at variables:
# ###### Outliers = possible > 4
# ###### join_year = no
# ###### fosa_detect_rate = no
# ###### pair_presence = no
# #### Look at random effects:
# ###### (1 | group_id) - Adjusted ICC: 0.430
# ###### (1 | date_factor) - Adjusted ICC: NA
# ###### (1 | group_id) + (1 | date_factor) - Adjusted ICC: 0.446
# #### Final model:
# ###### (1 | group_id) + (1 | date_factor) - Adjusted ICC: 0.446
# ###### Look at family: Student-t df = 1.703316
# ###### Look at variables:
# ######## fosa_detect_rate = no
# ######## pair_presence = no
# ######## join_year = yes
# ###### Look at residuals: okay

# Need to use log transformation here as one-sided skew is too strong & models don't converge

# Set up for models - CHANGE HERE
tmp <- daily.night.ddd[complete.cases(daily.night.ddd[,c("join_year", "ddd_log_scaled",
                                                         "fosa_detect_rate_scaled", "pair_presence_scaled",
                                                         "group_id", "date_factor")]),]
tmp$response <- tmp$ddd_log_scaled

# Look at variables
hist(tmp$response, breaks = "FD")
boxplot(tmp$response ~ tmp$join_year)
ggplot(data = tmp, aes(x = as.Date(date), y = response)) +
  facet_wrap(~join_year, scales = "free_x") +
  geom_point() +
  geom_smooth()
ggplot(data = tmp, aes(x = fosa_detect_rate_scaled, y = response)) +
  geom_jitter() +
  geom_smooth(method = "lm")
boxplot(response ~ pair_presence_scaled, data = tmp)

mod <- glmmTMB(response ~ fosa_detect_rate_scaled + pair_presence_scaled +
                 join_year + (1 | group_id) + (1 | date_factor), # (1 | group_id) + (1 | date_factor),
               data = tmp, family = t_family())

icc(mod)
VarCorr(mod)
diagnose(mod) # Unusually large Z-statistics (|x|>5): Alternately, they may simply represent very well-estimated parameters; intercepts of non-centered models may fall in this category.
family_params(mod)
summary(mod)
## For reference: (t_family)
# df > 30 → essentially Gaussian, t_family unnecessary
# df 10–30 → mildly heavy tails, modest improvement
# df 3–10 → good range — genuine heavy tails, t_family clearly justified
# df < 3 → extremely heavy tails, worth investigating outliers

simulationOutput <- simulateResiduals(fittedModel = mod, plot = F)
plot(simulationOutput)
plotResiduals(simulationOutput, tmp$fosa_detect_rate_scaled, asFactor = F)
plotResiduals(simulationOutput, tmp$pair_presence_scaled)
plot(tmp$response ~ predict(mod), asp = 1)
abline(a = 0, b = 1)

# Clean up
rm(tmp, mod, simulationOutput)


##### 7: Run final models #####

# Get smallest sample size - dyadic distance is by group, so smallest set
nrow(daily.day.ddd[complete.cases(daily.day.ddd[,c("ddd_scaled", "fosa_detect_rate_scaled",
                                                   "pair_presence_scaled","join_year",
                                                   "group_id", "date_factor")]),]) # 397
nrow(daily.night.ddd[complete.cases(daily.night.ddd[,c("ddd_scaled", "fosa_detect_rate_scaled",
                                                       "pair_presence_scaled", "join_year",
                                                       "group_id", "date_factor")]),]) # 345

# Daytime models
day_mods <- list(
  path_length = glmmTMB(path_length_scaled ~ fosa_detect_rate_scaled + pair_presence_scaled +
                          join_year + (1 | group_id/animal_id) + (1 | date_factor),
                        data = daily.day, family = t_family()),
  path_straightness = glmmTMB(straightness_scaled ~ fosa_detect_rate_scaled + pair_presence_scaled +
                                join_year + (1 | animal_id) + (1 | date_factor),
                              data = daily.day, family = t_family()),
  vedba = glmmTMB(vedba_scaled ~ fosa_detect_rate_scaled + pair_presence_scaled +
                    join_year + (1 | group_id/animal_id) + (1 | date_factor),
                  data = daily.day, family = t_family()),
  slp_site = glmmTMB(arrive_minutes_scaled ~ fosa_detect_rate_scaled + pair_presence_scaled +
                       join_year + (1 | group_id) + (1 | date_factor),
                     data = daily.day, family = t_family()),
  dyadic_dist = glmmTMB(ddd_log_scaled ~ fosa_detect_rate_scaled + pair_presence_scaled +
                          join_year + (1 | group_id) + (1 | date_factor),
                        data = daily.day.ddd, family = t_family())
)

# Results from daytime models
results_day <- bind_rows(
  lapply(names(day_mods), function(outcome) {
    broom.mixed::tidy(day_mods[[outcome]], effects = "fixed", conf.int = TRUE, component = "cond") %>%
      filter(term != "(Intercept)") %>%
      mutate(outcome = outcome)
  })
)
results_day <- results_day %>%
  filter(term != "join_year2023") %>%
  mutate(term = factor(term, levels = c("pair_presence_scaled", "fosa_detect_rate_scaled"))) %>%
  mutate(Outcome = factor(outcome, levels = c("dyadic_dist", "slp_site", "vedba",
                                              "path_straightness", "path_length")))

# Nighttime models
night_mods <- list(
  head_up = glmmTMB(headup_scaled ~ fosa_detect_rate_scaled + pair_presence_scaled +
                      join_year + (1 | group_id/animal_id) + (1 | date_factor),
                    data = daily.night, family = t_family()),
  tst_la = glmmTMB(tst_la_scaled ~ fosa_detect_rate_scaled + pair_presence_scaled +
                     join_year + (1 | group_id/animal_id) + (1 | date_factor),
                   data = daily.night, family = t_family()),
  vedba = glmmTMB(vedba_scaled ~ fosa_detect_rate_scaled + pair_presence_scaled +
                    join_year + (1 | group_id/animal_id) + (1 | date_factor),
                  data = daily.night, family = t_family()),
  slp_site = glmmTMB(leave_minutes_scaled ~ fosa_detect_rate_scaled + pair_presence_scaled +
                       join_year + (1 | group_id) + (1 | date_factor),
                     data = daily.night, family = t_family()),
  dyadic_dist = glmmTMB(ddd_log_scaled ~ fosa_detect_rate_scaled + pair_presence_scaled +
                          join_year + (1 | group_id) + (1 | date_factor),
                        data = daily.night.ddd, family = t_family())
)

# Results from nighttime models
results_night <- bind_rows(
  lapply(names(night_mods), function(outcome) {
    broom.mixed::tidy(night_mods[[outcome]], effects = "fixed", conf.int = TRUE, component = "cond") %>%
      filter(term != "(Intercept)") %>%
      mutate(outcome = outcome)
  })
)
results_night <- results_night %>%
  filter(term != "join_year2023") %>%
  mutate(term = factor(term, levels = c("pair_presence_scaled", "fosa_detect_rate_scaled"))) %>%
  mutate(Outcome = factor(outcome, levels = c("dyadic_dist", "slp_site","vedba",
                                              "tst_la", "head_up")))

# Add time of day to results dataframes
results_day$time <- "day"
results_night$time <- "night"

# Combine into one dataframe
results <- bind_rows(results_day, results_night)

# Multiple-comparisons correction - FDR
## Benjamini-Hochberg (FDR) applied within each predictor across the 10 outcomes
## (5 day + 5 night). fosa_detect_rate and pair_presence are distinct hypotheses, so
## each defines its own family of 10 tests. FDR (not Bonferroni/Holm) because the
## behavioral outcomes are correlated and this is a multi-outcome study.

results <- results %>%
  group_by(term) %>%
  mutate(p.value.adj = p.adjust(p.value, method = "BH")) %>%
  ungroup()


##### 8: Table of models - Table S2 #####

# Add columns
# Human-readable outcome labels (one per model x time of day)
outcome_labels <- c(
  day_path_length       = "Path length",
  day_path_straightness = "Path straightness",
  day_vedba             = "VeDBA (activity)",
  day_slp_site          = "Sleep site use (arrival time)",
  day_dyadic_dist       = "Interindividual distance",
  night_head_up         = "Proportion head-up (vigilance)",
  night_tst_la          = "Total sleep time",
  night_vedba           = "VeDBA (activity)",
  night_slp_site        = "Sleep site use (departure time)",
  night_dyadic_dist     = "Interindividual distance"
)

# Figure display order (top -> bottom); shared with the forest plot below so the
# table and figure stay in the same order. Models are numbered 1-10 to match.
figure_order <- c(
  "day_vedba", "day_slp_site", "day_dyadic_dist", "day_path_length", "day_path_straightness",
  "night_vedba", "night_slp_site", "night_dyadic_dist", "night_tst_la", "night_head_up"
)

# Per-model metadata (response transformation + random-effects structure),
# read directly from each fitted model so the table stays in sync with the models.
model_meta_row <- function(mod, period, nm) {
  fstr <- paste(deparse(formula(mod)), collapse = " ")
  resp <- trimws(sub("~.*", "", fstr))
  bars <- regmatches(fstr, gregexpr("\\([^()]*\\|[^()]*\\)", fstr))[[1]]
  re <- gsub("\\s+", " ", paste(bars, collapse = "; "))
  re <- gsub("(1 | group_id/animal_id)", "Individual within group", re, fixed = TRUE)
  re <- gsub("(1 | animal_id)", "Individual", re, fixed = TRUE)
  re <- gsub("(1 | group_id)", "Group", re, fixed = TRUE)
  re <- gsub("(1 | date_factor)", "Date", re, fixed = TRUE)
  # Rows the model actually used (complete cases on the formula variables)
  d <- eval(mod$call$data, envir = environment(formula(mod)))
  used <- d[complete.cases(d[, all.vars(formula(mod))]), ]
  stopifnot(nrow(used) == nobs(mod))
  data.frame(time = period, outcome = nm,
             resp_transform = ifelse(grepl("_log", resp), "Log", ""),
             random_effects = re,
             t_df     = unname(family_params(mod)[1]),  # Student's t degrees of freedom
             n_obs    = nobs(mod),
             n_sifaka = if ("animal_id" %in% names(used)) length(unique(used$animal_id)) else NA,
             n_groups = length(unique(used$group_id)),
             n_dates  = length(unique(used$date_factor)),
             stringsAsFactors = FALSE)
}
model_meta <- rbind(
  do.call(rbind, Map(model_meta_row, day_mods,   "day",   names(day_mods))),
  do.call(rbind, Map(model_meta_row, night_mods, "night", names(night_mods)))
)

# Number models 1-10 in figure order (top -> bottom)
model_meta$model_num <- match(paste(model_meta$time, model_meta$outcome, sep = "_"),
                              figure_order)

# Build a tidy results table (2 predictor rows per model = 20 rows), sorted to
# match the figure: outcomes top -> bottom, fosa detection rate before pair presence.
results_table <- results %>%
  left_join(model_meta, by = c("time", "outcome")) %>%
  arrange(model_num,
          factor(as.character(term),
                 levels = c("fosa_detect_rate_scaled", "pair_presence_scaled"))) %>%
  transmute(
    Period                              = ifelse(time == "day", "Day", "Night"),
    Model                               = model_num,
    Outcome                             = outcome_labels[paste(time, outcome, sep = "_")],
    `Response variable transformation`  = resp_transform,
    `Random effects`                    = random_effects,
    `Sample size`                       = paste0(n_obs, " obs; ",
                                                 ifelse(is.na(n_sifaka), "",
                                                        paste0(n_sifaka, " sifaka; ")),
                                                 n_groups, " groups; ", n_dates, " dates"),
    df                                  = round(t_df, 2),
    Term                                = recode(as.character(term),
                                                 pair_presence_scaled    = "Fosa pair presence",
                                                 fosa_detect_rate_scaled = "Fosa detection rate"),
    Estimate                            = round(estimate, 3),
    `95% CI`                            = paste0("[", round(conf.low, 3), ", ",
                                                 round(conf.high, 3), "]"),
    `P-value`                           = round(p.value, 3),
    `P-value (adjusted)`                = round(p.value.adj, 3)
  )

# Save as csv
write.csv(results_table, paste0(processed.data.dir, "model_results_table.csv"),
          row.names = FALSE, na = "")


##### 9: Single-panel forest plot (all 10 outcomes) - Figure 4 #####

# One row per outcome x time (10 total). Order top -> bottom in the display,
# so factor levels are set bottom -> top (reverse). Two estimates per outcome:
# solid = pair of fosa present, dashed = fosa detection rate.
forest_levels <- rev(figure_order)  # bottom -> top; single source of truth is figure_order

forest_labels <- c(
  night_vedba         = "VeDBA (activity)",
  night_slp_site      = "Sleep site use \n (departure time)",
  night_dyadic_dist   = "Interindividual distance",
  night_tst_la        = "Total sleep time",
  night_head_up       = "Proportion head-up \n (vigilance)",
  day_vedba           = "VeDBA (activity)",
  day_slp_site        = "Sleep site use \n (arrival time)",
  day_dyadic_dist     = "Interindividual distance",
  day_path_length     = "Path length",
  day_path_straightness = "Path straightness"
)

# Colorblind-safe palette (Okabe-Ito), one color per outcome
forest_colors <- c(
  path_length     = "#0072B2",  # blue
  path_straightness = "#009E73",  # bluish green
  vedba           = "#CC79A7",  # reddish purple
  slp_site        = "#56B4E9",  # sky blue
  dyadic_dist     = "#D55E00",  # vermillion
  tst_la          = "#E69F00",  # orange
  head_up         = "#000000"   # black
)

forest_df <- results %>%
  mutate(row_key = factor(paste(time, outcome, sep = "_"), levels = forest_levels))

forest_plot <- ggplot(forest_df,
                      aes(x = estimate, y = row_key, color = outcome, linetype = term)) +
  # light-gray band behind the night block (rows 1-5)
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = 5.5,
           fill = "gray90") +
  # block labels along the left edge (day = rows 6-10, night = rows 1-5)
  annotate("text", x = -0.19, y = 8, label = "Day", angle = 90,
           fontface = "bold", size = 6) +
  annotate("text", x = -0.19, y = 3, label = "Night", angle = 90,
           fontface = "bold", size = 6) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 position = position_dodge(width = 0.7), height = 0.25, linewidth = 0.9) +
  geom_point(position = position_dodge(width = 0.7), size = 2.2) +
  scale_y_discrete(labels = forest_labels, drop = FALSE) +
  scale_x_continuous(limits = c(-0.2, 0.2), breaks = c(-0.2, 0.0, 0.2)) +
  scale_color_manual(values = forest_colors) +
  scale_linetype_manual(
    name = "Predictor",
    values = c(pair_presence_scaled = "solid", fosa_detect_rate_scaled = "dashed"),
    breaks = c("fosa_detect_rate_scaled", "pair_presence_scaled"),  # legend order only
    labels = c(pair_presence_scaled = "Fosa pair presence",
               fosa_detect_rate_scaled = "Fosa detection rate")
  ) +
  guides(color = "none") +   # keep only the (small) predictor legend
  labs(x = "Standardized fosa effect\n(SD of outcome per SD of predictor)") +
  labs(y = "Sifaka behavior") +
  theme_classic(base_size = 22) +
  theme(legend.position = "right",
        legend.key.width = unit(1.8, "cm"),   # longer lines: solid vs dashed clearer
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13),
        axis.line = element_line(linewidth = 1.4, color = "black"),
        axis.ticks = element_line(linewidth = 1.4, color = "black"),
        axis.ticks.length = unit(6, "pt"))

forest_plot

ggsave(
  filename = paste0(plots.dir, "fosa_behavior_forest.pdf"),
  plot = forest_plot,
  width = 13.333, height = 7.5, units = "in"   # landscape 16:9
)

