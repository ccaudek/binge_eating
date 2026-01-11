# ==============================================================================
# 01_read_flagstaff.R
# ==============================================================================
# Project: Binge Eating and Self-Compassion EMA Study
# Purpose: Read and clean EMA data from Flagstaff (binge eating clinical group)
# Author:  Corrado Caudek
# Date:    Sat Jan 10 08:13:12 2026
#
# INPUT:
#   - data/raw/flagstaff/10.25.23_EMA_redcap.sav (SPSS format)
#
# OUTPUT:
#   - data/processed/d_flagstaff.rds
#   - data/dictionaries/data_dictionary_flagstaff.csv
#
# NOTES:
#   - Flagstaff participants are the CLINICAL (binge eating) group
#   - Data collected via REDCap, exported as SPSS file
#   - Contains variable labels and value labels from SPSS
#   - Notification_No indicates beep number (1-35 over 7 days)
# ==============================================================================

# ------------------------------------------------------------------------------
# SETUP
# ------------------------------------------------------------------------------

# Clear workspace
rm(list = ls())

# Source utility functions
source(here::here("scripts", "00_preprocessing", "00_utils.R"))

# Load packages
load_preprocessing_packages()

# Get paths
paths <- get_project_paths()

# Ensure output directories exist
setup_project_dirs()

# ------------------------------------------------------------------------------
# READ SPSS DATA
# ------------------------------------------------------------------------------

cat("\n========================================\n")
cat("Reading Flagstaff EMA Data\n")
cat("========================================\n\n")

# Path to SPSS file
spss_file <- file.path(paths$raw_flagstaff, "10.25.23_EMA_redcap.sav")

# Check file exists
if (!file.exists(spss_file)) {
  stop(sprintf("SPSS file not found: %s", spss_file))
}

cat(sprintf("Reading: %s\n\n", spss_file))

# Read SPSS file (haven preserves variable labels and value labels)
d_raw <- haven::read_sav(spss_file)

cat(sprintf(
  "Raw data: %d observations, %d variables\n",
  nrow(d_raw),
  ncol(d_raw)
))

# ------------------------------------------------------------------------------
# EXTRACT SPSS METADATA FOR DATA DICTIONARY
# ------------------------------------------------------------------------------

cat("\nExtracting SPSS metadata...\n")

# Function to extract variable label
get_var_label <- function(x) {
  lab <- attr(x, "label")
  if (is.null(lab)) NA_character_ else as.character(lab)
}

# Function to extract value labels
get_value_labels <- function(x) {
  labs <- attr(x, "labels")
  if (is.null(labs)) return(NA_character_)
  paste0(unname(labs), "=", names(labs), collapse = "; ")
}

# Create metadata table
spss_metadata <- tibble::tibble(
  variable = names(d_raw),
  var_label = purrr::map_chr(d_raw, get_var_label),
  value_labels = purrr::map_chr(d_raw, get_value_labels),
  class = purrr::map_chr(d_raw, ~ paste(class(.x), collapse = ", "))
)

# ------------------------------------------------------------------------------
# INITIAL CLEANING AND RENAMING
# ------------------------------------------------------------------------------

cat("\nCleaning and standardizing...\n")

d_flagstaff <- d_raw %>%
  # Rename key columns to match standard naming
  dplyr::rename(
    user_id = Participant_ID,
    WBIS_2 = WBI_2 # Fix typo in original data
  ) %>%
  # Convert user_id to character for consistency
  dplyr::mutate(
    user_id = as.character(user_id)
  )

# ------------------------------------------------------------------------------
# CREATE DAY AND BEEP VARIABLES
# ------------------------------------------------------------------------------

# The Flagstaff data uses Notification_No (1-35) to track beeps
# 5 beeps per day over 7 days
# Also has a Day variable we can use directly

d_flagstaff <- d_flagstaff %>%
  dplyr::mutate(
    # Use existing Day variable if available, otherwise calculate from Notification_No
    by_subj_day = if ("Day" %in% names(.)) {
      as.integer(Day)
    } else {
      dplyr::case_when(
        Notification_No >= 1 & Notification_No <= 5 ~ 1L,
        Notification_No >= 6 & Notification_No <= 10 ~ 2L,
        Notification_No >= 11 & Notification_No <= 15 ~ 3L,
        Notification_No >= 16 & Notification_No <= 20 ~ 4L,
        Notification_No >= 21 & Notification_No <= 25 ~ 5L,
        Notification_No >= 26 & Notification_No <= 30 ~ 6L,
        Notification_No >= 31 & Notification_No <= 35 ~ 7L,
        TRUE ~ NA_integer_
      )
    }
  )

# Create beep number within day (1-5)
d_flagstaff <- d_flagstaff %>%
  dplyr::group_by(user_id, by_subj_day) %>%
  dplyr::arrange(Notification_No, .by_group = TRUE) %>%
  dplyr::mutate(
    beep = dplyr::row_number()
  ) %>%
  dplyr::ungroup() %>%
  # Keep only beeps 1-5 (remove extras if any)
  dplyr::filter(beep %in% 1:5)

# Add time band
d_flagstaff <- d_flagstaff %>%
  dplyr::mutate(
    time_band = beep_to_time_band(beep),
    time_band = factor(time_band, levels = TIME_BAND_LEVELS)
  )

# Add group indicator
d_flagstaff$group <- "binge_eating"

# ------------------------------------------------------------------------------
# CREATE REVERSE-CODED SC ITEMS (if not already present)
# ------------------------------------------------------------------------------

# Check which SC items exist
sc_cols <- paste0("SC_", 1:6)
sc_cols_present <- intersect(sc_cols, names(d_flagstaff))

if (length(sc_cols_present) > 0) {
  # SC_2, SC_4, SC_6 are negative items - create reverse coded versions
  if ("SC_2" %in% names(d_flagstaff) && !"SC_2R" %in% names(d_flagstaff)) {
    d_flagstaff$SC_2R <- 6L - as.integer(d_flagstaff$SC_2)
  }
  if ("SC_4" %in% names(d_flagstaff) && !"SC_4R" %in% names(d_flagstaff)) {
    d_flagstaff$SC_4R <- 6L - as.integer(d_flagstaff$SC_4)
  }
  if ("SC_6" %in% names(d_flagstaff) && !"SC_6R" %in% names(d_flagstaff)) {
    d_flagstaff$SC_6R <- 6L - as.integer(d_flagstaff$SC_6)
  }
}

# ------------------------------------------------------------------------------
# CONVERT HAVEN LABELLED TO STANDARD R TYPES
# ------------------------------------------------------------------------------

# haven::read_sav creates "haven_labelled" objects
# Convert to standard R types while preserving values

d_flagstaff <- d_flagstaff %>%
  dplyr::mutate(
    dplyr::across(
      where(haven::is.labelled),
      ~ as.integer(haven::zap_labels(.x))
    )
  )

# Handle Did_you_eat specifically (ensure 0/1 coding)
if ("Did_you_eat" %in% names(d_flagstaff)) {
  d_flagstaff <- d_flagstaff %>%
    dplyr::mutate(
      Did_you_eat = as.integer(Did_you_eat)
    )
}

# ------------------------------------------------------------------------------
# SELECT AND ORDER FINAL COLUMNS
# ------------------------------------------------------------------------------

# Define column order (matching Firenze structure)
final_cols <- c(
  # Identifiers
  "group",
  "user_id",
  "by_subj_day",
  "beep",
  "time_band",

  # Self-Compassion (original + reverse coded)
  # Note: Flagstaff has SC_1-SC_6, Firenze has SC_1-SC_8
  "SC_1",
  "SC_2",
  "SC_3",
  "SC_4",
  "SC_5",
  "SC_6",
  "SC_2R",
  "SC_4R",
  "SC_6R",

  # Weight Bias
  "WBIS_1",
  "WBIS_2",
  "WBIS_3",

  # Affect
  "affect_Frustrat_Ang",
  "affect_Sad_Depressed",
  "affect_stress",

  # Eating
  "Did_you_eat",

  # Binge Eating Scale
  "BES_1",
  "BES_2",
  "BES_3",
  "BES_4",
  "BES_5",
  "BES_6",

  # Perceived Stress
  "PerceivedStress_1",
  "PerceivedStress_2"
)

# Select available columns in order
available_final_cols <- intersect(final_cols, names(d_flagstaff))

d_flagstaff_final <- d_flagstaff %>%
  dplyr::select(dplyr::all_of(available_final_cols)) %>%
  dplyr::mutate(
    group = factor(group, levels = c("control", "binge_eating")),
    user_id = as.character(user_id),
    by_subj_day = as.integer(by_subj_day),
    beep = as.integer(beep)
  ) %>%
  dplyr::arrange(user_id, by_subj_day, beep)

# Add responded indicator
d_flagstaff_final$responded <- 1L

# ------------------------------------------------------------------------------
# VALIDATION
# ------------------------------------------------------------------------------

cat("\n")
validation <- validate_ema_structure(d_flagstaff_final)
print_validation_report(validation)

# Additional checks
cat("\n--- Variable Summary ---\n")
cat("\nSelf-Compassion items (SC_1 to SC_6):\n")
if (any(grepl("^SC_\\d$", names(d_flagstaff_final)))) {
  d_flagstaff_final %>%
    dplyr::select(dplyr::matches("^SC_\\d$")) %>%
    summary() %>%
    print()
}

cat("\nBinge Eating Scale items:\n")
if (any(grepl("^BES_", names(d_flagstaff_final)))) {
  d_flagstaff_final %>%
    dplyr::select(dplyr::starts_with("BES_")) %>%
    summary() %>%
    print()
}

# ------------------------------------------------------------------------------
# SAVE PROCESSED DATA
# ------------------------------------------------------------------------------

output_file <- file.path(paths$processed, "d_flagstaff.rds")

saveRDS(d_flagstaff_final, output_file)

cat(sprintf("\n✓ Saved processed data to: %s\n", output_file))
cat(sprintf(
  "  - %d participants\n",
  dplyr::n_distinct(d_flagstaff_final$user_id)
))
cat(sprintf("  - %d observations\n", nrow(d_flagstaff_final)))

# ------------------------------------------------------------------------------
# CREATE DATA DICTIONARY
# ------------------------------------------------------------------------------

cat("\nCreating data dictionary...\n")

# Function to summarize a column
summarize_column <- function(x, col_name) {
  tibble::tibble(
    variable = col_name,
    class = paste(class(x), collapse = ", "),
    n_obs = length(x),
    n_missing = sum(is.na(x)),
    pct_missing = round(100 * sum(is.na(x)) / length(x), 2),
    n_unique = dplyr::n_distinct(x, na.rm = TRUE),
    min = if (is.numeric(x)) min(x, na.rm = TRUE) else NA_real_,
    max = if (is.numeric(x)) max(x, na.rm = TRUE) else NA_real_
  )
}

data_dictionary <- purrr::map2_dfr(
  d_flagstaff_final,
  names(d_flagstaff_final),
  summarize_column
)

# Merge with SPSS metadata
data_dictionary <- data_dictionary %>%
  dplyr::left_join(
    spss_metadata %>% dplyr::select(variable, var_label, value_labels),
    by = "variable"
  ) %>%
  dplyr::select(variable, var_label, value_labels, dplyr::everything())

# Save dictionary
dict_file <- file.path(paths$dictionaries, "data_dictionary_flagstaff.csv")
readr::write_csv(data_dictionary, dict_file)

cat(sprintf("✓ Saved data dictionary to: %s\n", dict_file))

# ------------------------------------------------------------------------------
# FINAL SUMMARY
# ------------------------------------------------------------------------------

cat("\n========================================\n")
cat("Flagstaff EMA Processing Complete\n")
cat("========================================\n\n")

# Participant-level summary
participant_summary <- d_flagstaff_final %>%
  dplyr::group_by(user_id) %>%
  dplyr::summarise(
    n_obs = dplyr::n(),
    n_days = dplyr::n_distinct(by_subj_day),
    .groups = "drop"
  )

cat("Observations per participant:\n")
print(summary(participant_summary$n_obs))

cat("\nDays per participant:\n")
print(table(participant_summary$n_days))

# Group summary
cat("\n--- Group Summary ---\n")
cat(sprintf("Group: %s (clinical)\n", unique(d_flagstaff_final$group)))
cat(sprintf(
  "N participants: %d\n",
  dplyr::n_distinct(d_flagstaff_final$user_id)
))
cat(sprintf("N observations: %d\n", nrow(d_flagstaff_final)))

cat("\n========================================\n")

# ==============================================================================
# END OF SCRIPT
# ==============================================================================
