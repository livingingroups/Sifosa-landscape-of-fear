#'----------------------------------------------------------------------------------------------------
#' Script: s6b_fosa_stats_plots.R
#'
#' Run fosa stats and create fosa plots
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
cam.data.dir <- "CAM/"
processed.data.dir <- "s6/"
plots.dir <- "s6/plots/"

# Load libraries
library(tidyverse)
library(sf)
library(suncalc)
library(patchwork)
library(data.table)


##### 2: Load data #####

# Fosa detections and camera trap locations (camera_table and detection_table)
camera_table <- fread(paste0(cam.data.dir, "camera_table.csv"))
detection_table <- fread(paste0(cam.data.dir, "detection_table.csv"))

# Fix timezone
# First time should be 2022-06-01 00:48:00
detection_table$timestamp <- as.POSIXct(format(detection_table$timestamp, tz = "UTC"),
                                        tz = "Indian/Antananarivo")
head(detection_table$timestamp)

# detection_table$timestamp <- with_tz(
#   detection_table$timestamp,
#   tzone = time_zone
# )

# Add the year and hour of each detection
cam_data <- detection_table %>%
  mutate(year = as.integer(year(timestamp)),
         hour = hour(timestamp))

# Camera locations
cams_sf <- camera_table %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  select(placename)

rm(detection_table)

# Sifaka home ranges (AKDE, 95% point estimate)
hr_files <- list.files(paste0(processed.data.dir, "hr_shp/"),
                       pattern = "^akde\\.2[23]\\.grp[0-9]+\\.shp$", full.names = TRUE)

# Each file has its own projection, so transform each to lat/lon before combining
hr_sf <- map(hr_files, st_read, quiet = TRUE) %>%
  map(st_transform, 4326) %>%
  set_names(basename(hr_files)) %>%
  bind_rows(.id = "file") %>%
  filter(grepl("est", name)) %>%
  mutate(year = 2000 + as.integer(str_extract(file, "(?<=akde\\.)[0-9]+")),
         group = as.integer(str_extract(file, "(?<=grp)[0-9]+")),
         group = factor(group, levels = sort(unique(group))))

rm(hr_files)


##### 3: Prep dataframes for fosa stats ####

### Set up: day/night, survey effort and camera positions

# Classify each detection as day or night using that date's sunrise and sunset
# (location from Movebank)
cam_data$date <- as.Date(cam_data$timestamp, tz = "Indian/Antananarivo")

# Every camera ran from 1 June 00:00 to 31 July 23:59 in both years
survey_sun <- tibble(date = c(seq(as.Date("2022-06-01"), as.Date("2022-07-31"), by = "day"),
                              seq(as.Date("2023-06-01"), as.Date("2023-07-31"), by = "day"))) %>%
  mutate(year = year(date),
         getSunlightTimes(date = date, lat = -20.783, lon = 44.174,
                          keep = c("sunrise", "sunset"),
                          tz = "Indian/Antananarivo")[, c("sunrise", "sunset")],
         day_hours = as.numeric(difftime(sunset, sunrise, units = "hours")))

cam_data <- cam_data %>%
  left_join(select(survey_sun, date, sunrise, sunset), by = "date") %>%
  mutate(period = ifelse(timestamp >= sunrise & timestamp < sunset, "day", "night"),
         pair = group_size == 2)

# Total daylight and dark hours in each year's survey period (same for every camera)
survey_hours <- survey_sun %>%
  group_by(year) %>%
  summarise(day = sum(day_hours), night = n() * 24 - sum(day_hours)) %>%
  pivot_longer(c(day, night), names_to = "period", values_to = "hours")

# All cameras in each year (including those with zero detections), one row per camera-year.
# Camera names are trail column + row (e.g. "GG17", "AS8").
# Outer-trail cameras are on the outermost row or column of that year's grid (all four sides);
# the 2023 grid is larger, so its outer ring is further out.
cams <- camera_table %>%
  separate_longer_delim(years, delim = ", ") %>%
  mutate(year = as.integer(years),
         row = str_extract(placename, "(S8|[0-9]+)$"),
         col = str_remove(placename, "(S8|[0-9]+)$")) %>%
  group_by(year) %>%
  mutate(position = ifelse(col %in% col[c(which.min(longitude), which.max(longitude))] |
                             row %in% row[c(which.min(latitude), which.max(latitude))],
                           "outer", "interior")) %>%
  ungroup() %>%
  select(year, placename, position)

rm(camera_table)


##### 4: Write functions for analyses ####

# Detection rate per camera and period: detections per 100 hours of daylight or darkness
period_rates <- function(detections) {
  cams %>%
    cross_join(tibble(period = c("day", "night"))) %>%
    left_join(count(detections, year, placename, period, name = "detections"),
              by = c("year", "placename", "period")) %>%
    mutate(detections = replace_na(detections, 0)) %>%
    left_join(survey_hours, by = c("year", "period")) %>%
    mutate(rate = detections / hours * 100)
}

# Overall detection rate per camera (day and night combined)
camera_rates <- function(rates) {
  rates %>%
    group_by(year, placename, position) %>%
    summarise(rate = sum(detections) / sum(hours) * 100, .groups = "drop")
}

# Summarise camera detection rates by a grouping column (e.g. period or camera/position)
rate_summary <- function(rates, group) {
  rates %>%
    group_by({{ group }}) %>%
    summarise(n_cameras = n(), mean_rate = mean(rate), sd_rate = sd(rate))
}


##### 5: Fosa stats ####

# All tests treat camera-years as the unit, with both years pooled.
# The 36 cameras used in both years are treated as independent between years because
# 1) the two surveys are ~10 months apart and
# 2) the new trails in 2023 changed each camera's position relative to the trail network.
# Wilcoxon tests rather than t-tests because rates are non-normal: many cameras have zero daytime (or pair) detections and rates are right-skewed.
# Rates rather than counts because night (~13 h) is longer than day (~11 h).
# Normal approximation (exact = FALSE) because of ties and zeros.
#  - Day vs night: paired signed-rank test, since each camera has both a day and a night rate and the two are not independent.
#     Cameras with no night - day difference are dropped by the test (n reported is the number used).
#  - Outer vs interior: unpaired rank-sum (Mann-Whitney) test, since these are different cameras.

day_night_test <- function(rates) {
  wide <- rates %>%
    select(year, placename, period, rate) %>%
    pivot_wider(names_from = period, values_from = rate)
  test <- wilcox.test(wide$night, wide$day, paired = TRUE, exact = FALSE)
  tibble(V = test$statistic, n = sum(wide$night != wide$day), p = test$p.value)
}


### Detections

# Total detections, by year
count(cam_data, year)
# year   n
# 2022 312
# 2023 424
nrow(cam_data) # 736


### Day vs night

fosa_rates <- period_rates(cam_data)

rate_summary(fosa_rates, period)
# period n_cameras mean_rate sd_rate
# day           90     0.178   0.259
# night         90     0.878   0.767

day_night_test(fosa_rates)
#    V     n        p
# 3347    82 2.84e-14

# Lag-1 autocorrelation of daily and nightly detection rates, per season.
# Grid-wide rate for each sampling day or night: detections at all cameras / camera-hours in that period x 100.
# A night runs from sunset on date D to sunrise on D + 1 and is assigned to D, so pre-dawn detections belong to the previous date's night.
# Only complete periods are used (61 days and 60 nights per season): the night before 1 June and the night of 31 July are only partly inside the survey
period_effort <- survey_sun %>%
  group_by(year) %>%
  mutate(night = as.numeric(difftime(lead(sunrise), sunset, units = "hours"))) %>%
  ungroup() %>%
  select(year, sample_date = date, day = day_hours, night) %>%
  pivot_longer(c(day, night), names_to = "period", values_to = "period_hours") %>%
  filter(!is.na(period_hours)) %>%
  left_join(count(cams, year, name = "n_cameras"), by = "year")

daily_rates <- period_effort %>%
  left_join(cam_data %>%
              mutate(sample_date = if_else(period == "night" & timestamp < sunrise,
                                           date - 1, date)) %>%
              count(year, sample_date, period, name = "detections"),
            by = c("year", "sample_date", "period")) %>%
  mutate(detections = replace_na(detections, 0),
         rate = detections / (n_cameras * period_hours) * 100) %>%
  arrange(year, period, sample_date)

# Values outside +/- ci95 (1.96 / sqrt(n)) differ from zero at p < 0.05
daily_rates %>%
  group_by(year, period) %>%
  summarise(n = n(), lag1_acf = acf(rate, lag.max = 1, plot = FALSE)$acf[2],
            ci95 = 1.96 / sqrt(n()), .groups = "drop")
# year period  n lag1_acf  ci95
# 2022 day    61    0.221 0.251
# 2022 night  60   -0.003 0.253
# 2023 day    61    0.327 0.251
# 2023 night  60   -0.061 0.253

rm(period_effort, daily_rates)


### Outer-trail vs interior cameras

fosa_space <- camera_rates(fosa_rates)

rate_summary(fosa_space, position)
# position n_cameras mean_rate sd_rate
# interior        46     0.257   0.308
# outer           44     0.874   0.449

wilcox.test(rate ~ position, data = fosa_space, exact = FALSE)
# W = 222, p-value = 1.682e-10


### Fosa pairs (detections with group_size = 2)

# Number and percentage of detections that were pairs
sum(cam_data$pair) # 129
mean(cam_data$pair) * 100 # 17.52

# Pair detections per season (pair dyads and their sex composition are in s7a, section 3)
cam_data %>%
  group_by(year) %>%
  summarise(pair_detections = sum(pair), pct_pairs = mean(pair) * 100)
# year pair_detections pct_pairs
# 2022              73     23.40
# 2023              56     13.21

# Day vs night
pair_rates <- period_rates(filter(cam_data, pair))

rate_summary(pair_rates, period)
# period n_cameras mean_rate sd_rate
# day           90    0.0266  0.0730
# night         90    0.158   0.226

day_night_test(pair_rates)
#    V     n            p
# 1095    47 0.0000000182

# Outer-trail vs interior cameras
pair_space <- camera_rates(pair_rates)

rate_summary(pair_space, position)
# position n_cameras mean_rate sd_rate
# interior        46    0.0208  0.0349
# outer           44    0.179   0.162

wilcox.test(rate ~ position, data = pair_space, exact = FALSE)
# W = 347, p-value = 1.082e-08

### Summary table by year and time of day
# Mean +/- SD across cameras, in detections per 100 camera-hours (as in the results text)
fosa_table <- fosa_rates %>%
  group_by(year, period) %>%
  summarise(total = sum(detections), mean_rate = mean(rate), sd_rate = sd(rate),
            .groups = "drop") %>%
  left_join(pair_rates %>%
              group_by(year, period) %>%
              summarise(pairs = sum(detections), pair_mean = mean(rate), pair_sd = sd(rate),
                        .groups = "drop"),
            by = c("year", "period")) %>%
  transmute(Year = year,
            `Time of Day` = str_to_title(period),
            `Total Number of Detections` = total,
            "Mean Detection Rate \u00b1 SD" = sprintf("%.2f \u00b1 %.2f", mean_rate, sd_rate), # "\u00b1" is the plus-minus sign
            `Number of Pair Detections` = pairs,
            "Pair Detection Rate \u00b1 SD" = sprintf("%.2f \u00b1 %.2f", pair_mean, pair_sd))

fosa_table

# Save
write_excel_csv(fosa_table, paste0(processed.data.dir, "fosa_detection_table.csv"))

rm(fosa_rates, fosa_space, pair_rates, pair_space, survey_hours,
   period_rates, camera_rates, day_night_test, rate_summary, fosa_table)


##### 6: Plot detections on map #####

# Maps are in UTM zone 38S (EPSG:32738; metres), the projection of the trail shapefile
hr_sf <- st_transform(hr_sf, 32738)

# Count fosa detections per camera and year, keeping cameras with zero detections
cam_det <- cams_sf %>%
  inner_join(select(cams, year, placename), by = "placename") %>%
  left_join(count(cam_data, year, placename, name = "detections"),
            by = c("year", "placename")) %>%
  mutate(detections = replace_na(detections, 0)) %>%
  st_transform(32738)

# A distinct colour for each group (anonymized groups G01-G14), the same in both years
group_cols <- c("1" = "#8AB17D",   # sage
                "2" = "#5FA8D3",   # sky blue
                "3" = "#6A994E",   # olive green
                "4" = "#A38DC7",   # lavender
                "5" = "#E9C46A",   # mustard
                "6" = "#2A9D8F",   # teal
                "7" = "#264653",   # deep navy
                "8" = "#F4A261",   # apricot
                "9" = "#6D597A",   # plum
                "10" = "#4A6FA5",  # slate blue
                "11" = "#9B2226",  # burgundy
                "12" = "#7F5539",  # chocolate
                "13" = "#B56576",  # dusty rose
                "14" = "#D1603D")  # terracotta

# Shared map extent so the two years are comparable
map_bbox <- st_bbox(bind_rows(select(hr_sf, geometry), select(cam_det, geometry)))
map_xlim <- map_bbox[c("xmin", "xmax")] + c(-50, 50)
map_ylim <- map_bbox[c("ymin", "ymax")] + c(-50, 50)

# 250 m scale bar in the top left of each map (map units are metres)
scale_m <- 250
scale_y <- map_bbox[["ymax"]] - 20
scale_x <- map_bbox[["xmin"]] + 30 + c(0, scale_m)

# Side-by-side maps for 2022 and 2023 with a shared detection legend
fosa_map <- ggplot() +
  geom_sf(data = hr_sf, aes(color = group), fill = NA, linewidth = 1) +
  geom_sf(data = filter(cam_det, detections > 0), aes(size = detections),
          color = "grey30", alpha = 0.7) +
  geom_sf(data = cam_det, color = "black", size = 0.3) +
  ggplot2::annotate("segment", x = scale_x[1], xend = scale_x[2], y = scale_y, yend = scale_y,
                    linewidth = 0.8) +
  ggplot2::annotate("segment", x = scale_x, xend = scale_x, y = scale_y - 13,
                    yend = scale_y + 13, linewidth = 0.8) +
  ggplot2::annotate("text", x = mean(scale_x), y = scale_y - 22, label = paste(scale_m, "m"),
                    vjust = 1, size = 3.5) +
  facet_wrap(~ year) +
  scale_color_manual(values = group_cols, guide = "none") +
  scale_size_area(max_size = 9, breaks = c(5, 15, 25, 35), name = "Fosa detections") +
  # datum = 32738 labels the axes in UTM easting/northing (m) rather than lat/lon
  coord_sf(xlim = map_xlim, ylim = map_ylim, expand = FALSE, datum = 32738) +
  labs(x = "Easting (UTM)", y = "Northing (UTM)") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, face = "bold"))

fosa_map

ggsave(paste0(plots.dir, "fosa_detections_hr_2022_2023.png"), fosa_map,
        width = 12, height = 5.5, dpi = 300)

rm(group_cols, map_bbox, scale_m, scale_y, scale_x)


##### 7: Plot detections by time of day #####

# Detection rate in each hour of the day, keeping hours with zero detections.
# 2023 had more cameras than 2022, so divide each hourly count by the effort behind it
# (cameras x survey days, i.e. camera-hours in that hour of the day) to compare years
hour_effort <- cams %>%
  distinct(year, placename) %>%
  count(year, name = "n_cameras") %>%
  left_join(count(survey_sun, year, name = "n_days"), by = "year") %>%
  mutate(camera_hours = n_cameras * n_days)

hourly_det <- cam_data %>%
  count(year, hour, name = "detections") %>%
  complete(year, hour = 0:23, fill = list(detections = 0)) %>%
  left_join(hour_effort, by = "year") %>%
  mutate(rate = detections / camera_hours * 100)

# Mean sunrise and sunset (decimal hours, local time) over each year's survey period
sun_times <- survey_sun %>%
  group_by(year) %>%
  summarise(sunrise = mean(hour(sunrise) + minute(sunrise) / 60),
            sunset = mean(hour(sunset) + minute(sunset) / 60))

# Night shading from sunset to sunrise. Each bar is centered on its hour label, so clock
# time t sits at x = t - 0.5 on the axis; night wraps past midnight, so use two pieces
night_shade <- sun_times %>%
  reframe(year = rep(year, 2),
          xmin = c(sunset - 0.5, rep(-0.5, n())),
          xmax = c(rep(23.5, n()), sunrise - 0.5))

# Circular histogram with midnight at the top, one panel per year
fosa_circ <- ggplot(hourly_det, aes(x = hour, y = rate)) +
  geom_rect(data = night_shade, aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = Inf),
            inherit.aes = FALSE, fill = "grey40", alpha = 0.15) +
  geom_col(width = 0.95, fill = "#2F4858") +
  facet_wrap(~ year) +
  scale_x_continuous(limits = c(-0.5, 23.5), breaks = 0:23, expand = c(0, 0)) +
  coord_polar(start = -pi / 24) +
  labs(x = NULL, y = "Detections per 100 camera-hours") +
  theme_bw() +
  theme(panel.grid.major = element_line(color = "grey85"),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10))

fosa_circ

ggsave(paste0(plots.dir, "fosa_detections_by_hour_2022_2023.png"), fosa_circ,
        width = 10, height = 5.5, dpi = 300)

rm(hour_effort, sun_times, night_shade)


##### 8: Combine maps and histograms #####

# Four panels: maps on top, histograms below, one column per year.
# Year labels only on the top row, since each histogram sits under its map.
# Panel letters (A-D, plain text) sit just above the top-left corner of each panel: drawn as text
# outside the map panels (clip = "off"), and as left-aligned facet strips on the histograms
panel_tags <- tibble(year = c(2022, 2023), label = c("A", "B"))
# Smaller detection circles, with the legend inside the 2022 map below the scale bar
# (legend position is relative to the whole panel area spanning both maps)
fosa_combined <- (fosa_map +
                    scale_size_area(max_size = 6, breaks = c(5, 15, 25, 35),
                                    name = "Fosa detections") +
                    theme(legend.position = "inside",
                          legend.position.inside = c(0.01, 0.88),
                          legend.justification = c(0, 1),
                          legend.background = element_blank(),
                          legend.key = element_blank(),
                          legend.title = element_text(size = 8),
                          legend.text = element_text(size = 7),
                          legend.key.size = unit(0.4, "cm")) +
                    geom_text(data = panel_tags, aes(label = label),
                              x = map_xlim[[1]], y = map_ylim[[2]], hjust = 0, vjust = -0.5,
                              fontface = "plain", size = 14 / .pt, inherit.aes = FALSE) +
                    coord_sf(xlim = map_xlim, ylim = map_ylim, expand = FALSE,
                             datum = 32738, clip = "off")) /
  (fosa_circ +
     facet_wrap(~ year, labeller = as_labeller(c(`2022` = "C", `2023` = "D"))) +
     theme(strip.text = element_text(size = 14, face = "plain", hjust = 0,
                                     margin = margin(0, 0, 2, 0)))) +
  plot_layout(heights = c(3.1, 3.95))

rm(panel_tags)

fosa_combined

# Save as a square PDF
ggsave(paste0(plots.dir, "fosa_maps_and_hours_2022_2023.pdf"), fosa_combined,
       width = 8.5, height = 8.5, units = "in")


