# Sifaka–fosa Behavioral Analysis Workflow

This repository contains the R scripts used to process and analyze GPS, accelerometer (ACC), and camera-trap data collected on Verreaux's sifakas (*Propithecus verreauxi*) and fosa (*Cryptoprocta ferox*).

The workflow combines movement, activity, sleep-site use, predator–prey encounters, and camera-trap detections to investigate sifaka responses to fosa presence and variation in fosa activity.

## 1. Workflow overview

The complete analytical workflow is illustrated in `workflow.png`

The workflow is organized around three main data sources:

- **GPS data:** Sifaka and fosa locations, sampled every 3 minutes.
- **Accelerometer (ACC) data:** Sifaka activity data, including pitch and VeDBA, sampled every 1 or 1.5 minutes.
- **Camera-trap data:** fosa detection records.

These data streams are processed separately before being combined to generate datasets for the different analyses.

## 2. Input data

### GPS data

Files:

- `fosa.22.3min.csv`
- `fosa.23.3min.csv`
- `reference.csv`
- `sifaka.22.3min.csv`
- `sifaka.23.3min.csv`

GPS data are used to derive movement metrics, identify sleep-site use, and reconstruct predator–prey encounters.

### Accelerometer data

Files:

- `S01_2022_dry_regular.csv`
- `S02_2023_dry_regular.csv`
- `S03_2022_dry_regular.csv`
- `S03_2023_dry_regular.csv`
-  etc.

ACC data are used to derive activity metrics, including pitch and VeDBA, and to calculate total sleep time.

### Camera-trap data

Files:

- `camera_table.csv`
- `detection_table.csv`

These data are used to estimate daily and nightly fosa presence and associated predation risk.

## 3. R scripts and processing steps

### A. Predator–prey encounters and sifaka responses

These scripts process GPS data to identify encounters between sifakas and fosa and extract immediate behavioral responses.

| Script | Description | Execution |
|---|---|---|
| `s0_make_ari_matrix.R` | Converts GPS data into matrix format | Per year and season |
| `s1_extract_sifosa_encounters.R` | Extracts sifaka–fosa encounters | Per year and season |
| `s2_extract_immediate_responses.R` | Extracts immediate responses following encounters, based on the selected metric (VeDBA, pitch or movement) | Per year, season and metric |
| `s3_test_encounter_responses.R` | Tests sifaka responses to fosa encounters | Per metric and day/night period |

**Main output:** Predator–prey encounter dataset and analyses of sifaka reactions to fosa encounters.

### B. Movement and sleep-site behavior

These scripts derive movement metrics and sleep-site information from GPS data.

| Script | Description | Execution |
|---|---|---|
| `s4a_get_sleep_site_data.R` | Identifies sleep-site departure times and nightly arrival times | Per year and season |
| `s4b_mov_metrics.R` | Calculates daily path length and straightness | Once (both years processed) |

**Main outputs:**

- Daily path length and straightness.
- Daily sleep-site departure times and nightly arrival times.

### C. Daily and nightly activity and antipredator behavior

These scripts combine GPS, accelerometer and camera-trap information to characterise daily and nightly variation in fosa activity and sifaka antipredator behavior.

| Script | Description | Execution |
|---|---|---|
| `s4c_id_overnight_activity.R` | Identifies overnight activity and calculates nightly total sleep time | Per season (both years processed) |
| `s4d_fosa_presence.R` | Estimates daily and nightly fosa presence and associated predation risk from camera-trap data | Once (both years processed) |
| `s4e_compute_day_night_behav.R` | Computes daily/nightly interindividual distance, VeDBA and percentage of head-up behavior | Per year and season |
| `s5_combine_day_night_data.R` | Combines daily and nightly behavioral and predation-related data | Per season |
| `s7_test_risk_responses.R` | Tests sifaka behavioral responses to day-to-day variation in fosa activity | Once (both years processed) |

**Main output:** Daily/nightly antipredator behavior dataset and analyses of sifaka responses to variation in fosa activity.

### D. Home range and fosa activity patterns

| Script | Description | Execution |
|---|---|---|
| `s6a_calculate_home_range.R` | Calculates home ranges | Once (both years processed) |
| `s6b_fosa_stats_plots.R` | Produces descriptive statistics and plots of fosa activity patterns | Once (both years processed) |

**Main outputs:**

- Sifaka home ranges.
- fosa activity patterns.

## 4. Execution guidelines

The scripts do not all need to be executed with the same frequency.

- **Per year and season:** Scripts requiring year- and season-specific input data should be run separately for each corresponding dataset.
- **Per metric:** Scripts examining immediate behavioral responses should be repeated for the relevant activity metric (VeDBA, pitch or movement).
- **Once:** Scripts indicated as *Run this once* in the workflow are intended to be executed once for the complete dataset.
- **Both years:** Some scripts process both study years together, as indicated in the workflow.

Refer to the workflow diagram for the dependencies between scripts and the datasets generated at each step.

## 5. Analysis outputs

The workflow produces datasets and analyses addressing three main questions:

1. **Sifaka reactions to fosa encounters:** How do sifakas respond immediately to predator–prey encounters?
2. **Sifaka responses to variation in fosa activity:** How does sifaka behavior vary in relation to daily and nightly changes in fosa activity?
3. **fosa activity patterns:** What are the daily and nightly activity patterns and home ranges of fosa?
