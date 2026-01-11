# ==============================================================================
# 02_read_firenze_ema.R
# ==============================================================================
# Project: Binge Eating and Self-Compassion EMA Study
# Purpose: Read and clean EMA data from Florence (control group)
# Author:  Corrado Caudek
# Date:    Sat Jan 10 08:21:48 2026
#
# INPUT:
#   - Multiple Excel files in data/raw/firenze/ema/controls/
#   - Each file contains EMA responses for one participant
#   - File naming convention: initials_surname_birthdate_code_suffix.xlsx
#
# OUTPUT:
#   - data/processed/d_firenze_ema.rds
#   - data/dictionaries/data_dictionary_firenze.csv
#
# NOTES:
#   - Florence participants are the CONTROL group
#   - Data collected via m-Path app, exported as individual Excel files
#   - Expected: 7 days × 5 beeps = 35 observations per participant
#   - Some participants have 8-9 days (corrected in this script)
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
# READ ALL INDIVIDUAL EMA FILES
# ------------------------------------------------------------------------------

cat("\n========================================\n")
cat("Reading Florence EMA Data\n")
cat("========================================\n\n")

# List all Excel files
file_paths <- list.files(
  path = paths$raw_firenze_ema,
  pattern = "\\.xlsx$",
  full.names = TRUE
)
file_names <- basename(file_paths)
n_files <- length(file_paths)

cat(sprintf("Found %d Excel files to process\n\n", n_files))

# Clear the day correction log before processing
clear_day_correction_log()

# ------------------------------------------------------------------------------
# FUNCTION: Read and process a single participant file
# ------------------------------------------------------------------------------

#' Read and process a single Florence EMA Excel file
#'
#' @param file_path Full path to the Excel file
#' @param file_name Basename of the file (used as user_id)
#' @return Processed data frame for one participant, or NULL if reading fails
read_single_firenze_ema <- function(file_path, file_name) {
  # Extract user_id from filename (remove .xlsx extension)
  user_id <- sub("\\.xlsx$", "", file_name)

  # Attempt to read file
  tryCatch(
    {
      # Read Excel file (skip first row which contains headers)
      d1 <- readxl::read_excel(file_path, skip = 1)

      # Check if file has data
      if (nrow(d1) == 0) {
        warning(sprintf("Empty file: %s", file_name))
        return(NULL)
      }

      # Remove system/intro columns if present
      d1 <- d1 %>%
        dplyr::select(-dplyr::any_of(FIRENZE_EMA_DROP_COLS))

      # Check if required columns exist before renaming
      available_cols <- names(d1)
      rename_map_available <- FIRENZE_EMA_RENAME_MAP[
        FIRENZE_EMA_RENAME_MAP %in% available_cols
      ]

      if (length(rename_map_available) == 0) {
        warning(sprintf("No expected columns found in: %s", file_name))
        return(NULL)
      }

      # Rename columns (only those that exist)
      d1 <- d1 %>%
        dplyr::rename(dplyr::any_of(FIRENZE_EMA_RENAME_MAP))

      # Add participant identifier
      d1$user_id <- user_id

      # Parse datetime and extract date components
      if ("dat_time" %in% names(d1)) {
        d1 <- d1 %>%
          dplyr::mutate(
            datetime = parse_firenze_datetime(dat_time),
            date = as.Date(datetime),
            time = format(datetime, "%H:%M:%S")
          )

        # Create within-subject day number (1, 2, 3, ...)
        d1 <- d1 %>%
          dplyr::mutate(
            by_subj_day = as.integer(factor(date))
          )
      }

      # Apply day correction (handle 8 or 9 day protocols)
      d1 <- correct_day_numbering(d1)

      # ----------------------------------------------------------------------
      # Convert text responses to numeric codes
      # ----------------------------------------------------------------------

      # Self-Compassion items (SC_1 to SC_8)
      sc_cols <- paste0("SC_", 1:8)
      sc_cols_present <- intersect(sc_cols, names(d1))

      if (length(sc_cols_present) > 0) {
        d1 <- d1 %>%
          dplyr::mutate(
            dplyr::across(dplyr::all_of(sc_cols_present), convert_sc)
          )

        # Create reverse-coded items for negative SC items
        # SC_2, SC_4, SC_6, SC_7 are negative items (reverse code: 6 - x)
        if ("SC_2" %in% names(d1)) d1$SC_2R <- 6L - d1$SC_2
        if ("SC_4" %in% names(d1)) d1$SC_4R <- 6L - d1$SC_4
        if ("SC_6" %in% names(d1)) d1$SC_6R <- 6L - d1$SC_6
        if ("SC_7" %in% names(d1)) d1$SC_7R <- 6L - d1$SC_7
      }

      # Weight Bias items (WBIS_1 to WBIS_3)
      wbis_cols <- paste0("WBIS_", 1:3)
      wbis_cols_present <- intersect(wbis_cols, names(d1))

      if (length(wbis_cols_present) > 0) {
        d1 <- d1 %>%
          dplyr::mutate(
            dplyr::across(dplyr::all_of(wbis_cols_present), convert_wbis)
          )
      }

      # Affect items
      affect_cols <- c(
        "affect_Frustrat_Ang",
        "affect_Sad_Depressed",
        "affect_stress"
      )
      affect_cols_present <- intersect(affect_cols, names(d1))

      if (length(affect_cols_present) > 0) {
        d1 <- d1 %>%
          dplyr::mutate(
            dplyr::across(dplyr::all_of(affect_cols_present), convert_affect)
          )
      }

      # Eating question (yes/no to 1/0)
      if ("Did_you_eat" %in% names(d1)) {
        d1 <- d1 %>%
          dplyr::mutate(
            Did_you_eat = dplyr::case_when(
              tolower(Did_you_eat) == "yes" ~ 1L,
              tolower(Did_you_eat) == "no" ~ 0L,
              TRUE ~ NA_integer_
            )
          )
      }

      # Binge Eating Scale items (BES_1 to BES_6)
      bes_cols <- paste0("BES_", 1:6)
      bes_cols_present <- intersect(bes_cols, names(d1))

      if (length(bes_cols_present) > 0) {
        d1 <- d1 %>%
          dplyr::mutate(
            dplyr::across(dplyr::all_of(bes_cols_present), convert_bes)
          )
      }

      # Perceived Stress items
      ps_cols <- c("PerceivedStress_1", "PerceivedStress_2")
      ps_cols_present <- intersect(ps_cols, names(d1))

      if (length(ps_cols_present) > 0) {
        d1 <- d1 %>%
          dplyr::mutate(
            dplyr::across(
              dplyr::all_of(ps_cols_present),
              convert_perceived_stress
            )
          )
      }

      # ----------------------------------------------------------------------
      # Create beep number within day and time band
      # ----------------------------------------------------------------------

      d1 <- d1 %>%
        dplyr::arrange(user_id, by_subj_day, datetime) %>%
        dplyr::group_by(user_id, by_subj_day) %>%
        dplyr::mutate(
          beep = dplyr::row_number(),
          time_band = beep_to_time_band(beep)
        ) %>%
        dplyr::ungroup()

      # Keep only valid beeps (1-5, remove extras if any)
      d1 <- d1 %>%
        dplyr::filter(beep %in% 1:5)

      # Add response indicator
      d1$responded <- 1L

      # Add group indicator
      d1$group <- "control"

      # Return processed data
      d1
    },
    error = function(e) {
      warning(sprintf("Error reading %s: %s", file_name, e$message))
      return(NULL)
    }
  )
}

# ------------------------------------------------------------------------------
# PROCESS ALL FILES
# ------------------------------------------------------------------------------

# Process each file with progress indication
d_list <- vector("list", n_files)

for (i in seq_len(n_files)) {
  if (i %% 10 == 0 || i == n_files) {
    cat(sprintf("Processing file %d/%d...\n", i, n_files))
  }

  d_list[[i]] <- read_single_firenze_ema(file_paths[i], file_names[i])
}

# Remove NULL entries (failed reads)
d_list <- d_list[!sapply(d_list, is.null)]

cat(sprintf("\nSuccessfully read %d/%d files\n", length(d_list), n_files))

# ------------------------------------------------------------------------------
# COMBINE ALL PARTICIPANTS
# ------------------------------------------------------------------------------

cat("\nCombining all participants...\n")

d_firenze_ema <- dplyr::bind_rows(d_list)

# ------------------------------------------------------------------------------
# STANDARDIZE COLUMN SELECTION AND ORDER
# ------------------------------------------------------------------------------

# Define final column order
final_cols <- c(
  # Identifiers
  "group",
  "user_id",
  "by_subj_day",
  "beep",
  "time_band",
  "datetime",

  # Self-Compassion (original + reverse coded)
  "SC_1",
  "SC_2",
  "SC_3",
  "SC_4",
  "SC_5",
  "SC_6",
  "SC_7",
  "SC_8",
  "SC_2R",
  "SC_4R",
  "SC_6R",
  "SC_7R",

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
  "PerceivedStress_2",

  # Metadata
  "responded"
)

# Select and order columns (only those that exist)
available_final_cols <- intersect(final_cols, names(d_firenze_ema))

d_firenze_ema <- d_firenze_ema %>%
  dplyr::select(dplyr::all_of(available_final_cols)) %>%
  dplyr::mutate(
    group = factor(group, levels = c("control", "binge_eating")),
    user_id = as.character(user_id),
    by_subj_day = as.integer(by_subj_day),
    beep = as.integer(beep),
    time_band = factor(time_band, levels = TIME_BAND_LEVELS)
  ) %>%
  dplyr::arrange(user_id, by_subj_day, beep)

# ------------------------------------------------------------------------------
# VALIDATION
# ------------------------------------------------------------------------------

cat("\n")
validation <- validate_ema_structure(d_firenze_ema)
print_validation_report(validation)

# Additional summary statistics
cat("\n--- Variable Ranges ---\n")

# Self-Compassion items
if (any(grepl("^SC_\\d$", names(d_firenze_ema)))) {
  sc_summary <- d_firenze_ema %>%
    dplyr::select(dplyr::matches("^SC_\\d$")) %>%
    summary()
  print(sc_summary)
}

# ------------------------------------------------------------------------------
# SAVE PROCESSED DATA
# ------------------------------------------------------------------------------

output_file <- file.path(paths$processed, "d_firenze_ema.rds")

saveRDS(d_firenze_ema, output_file)

cat(sprintf("\n✓ Saved processed data to: %s\n", output_file))
cat(sprintf("  - %d participants\n", dplyr::n_distinct(d_firenze_ema$user_id)))
cat(sprintf("  - %d observations\n", nrow(d_firenze_ema)))

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
    max = if (is.numeric(x)) max(x, na.rm = TRUE) else NA_real_,
    example_values = paste(head(unique(na.omit(x)), 5), collapse = "; ")
  )
}

data_dictionary <- purrr::map2_dfr(
  d_firenze_ema,
  names(d_firenze_ema),
  summarize_column
)

# Add variable labels
var_labels <- c(
  group = "Group (control vs binge_eating)",
  user_id = "Participant ID",
  by_subj_day = "Day within subject (1-7)",
  beep = "Beep number within day (1-5)",
  time_band = "Time band label",
  datetime = "Response datetime",
  SC_1 = "Self-compassion item 1 (positive)",
  SC_2 = "Self-compassion item 2 (negative)",
  SC_3 = "Self-compassion item 3 (positive)",
  SC_4 = "Self-compassion item 4 (negative)",
  SC_5 = "Self-compassion item 5 (positive)",
  SC_6 = "Self-compassion item 6 (negative)",
  SC_7 = "Self-compassion item 7 (negative, new)",
  SC_8 = "Self-compassion item 8 (positive, new)",
  SC_2R = "SC item 2 reverse coded (6 - SC_2)",
  SC_4R = "SC item 4 reverse coded (6 - SC_4)",
  SC_6R = "SC item 6 reverse coded (6 - SC_6)",
  SC_7R = "SC item 7 reverse coded (6 - SC_7)",
  WBIS_1 = "Weight Bias Internalization item 1 (1-7)",
  WBIS_2 = "Weight Bias Internalization item 2 (1-7)",
  WBIS_3 = "Weight Bias Internalization item 3 (1-7)",
  affect_Frustrat_Ang = "Affect: Frustrated/Angry (1-4)",
  affect_Sad_Depressed = "Affect: Sad/Depressed (1-4)",
  affect_stress = "Affect: Stressed (1-4)",
  Did_you_eat = "Did you eat in past 2 hours? (0=No, 1=Yes)",
  BES_1 = "Binge Eating Scale item 1 (1-5)",
  BES_2 = "Binge Eating Scale item 2 (1-5)",
  BES_3 = "Binge Eating Scale item 3 (1-5)",
  BES_4 = "Binge Eating Scale item 4 (1-5)",
  BES_5 = "Binge Eating Scale item 5 (1-5)",
  BES_6 = "Binge Eating Scale item 6 (1-5)",
  PerceivedStress_1 = "Perceived Stress item 1 (1-4)",
  PerceivedStress_2 = "Perceived Stress item 2 (1-4)",
  responded = "Response indicator (always 1)"
)

data_dictionary <- data_dictionary %>%
  dplyr::mutate(
    label = var_labels[variable]
  ) %>%
  dplyr::select(variable, label, dplyr::everything())

# Save dictionary
dict_file <- file.path(paths$dictionaries, "data_dictionary_firenze.csv")
readr::write_csv(data_dictionary, dict_file)

cat(sprintf("✓ Saved data dictionary to: %s\n", dict_file))

# ------------------------------------------------------------------------------
# FINAL SUMMARY
# ------------------------------------------------------------------------------

cat("\n========================================\n")
cat("Florence EMA Processing Complete\n")
cat("========================================\n\n")

# Participant-level summary
participant_summary <- d_firenze_ema %>%
  dplyr::group_by(user_id) %>%
  dplyr::summarise(
    n_obs = dplyr::n(),
    n_days = dplyr::n_distinct(by_subj_day),
    first_date = min(datetime, na.rm = TRUE),
    last_date = max(datetime, na.rm = TRUE),
    .groups = "drop"
  )

cat("Observations per participant:\n")
print(summary(participant_summary$n_obs))

cat("\nDays per participant:\n")
print(table(participant_summary$n_days))

# List participants with < 28 observations (80% compliance)
low_compliance <- participant_summary %>%
  dplyr::filter(n_obs < 28)

if (nrow(low_compliance) > 0) {
  cat(sprintf(
    "\n⚠ %d participants with < 28 observations (< 80%% compliance)\n",
    nrow(low_compliance)
  ))
}

# ------------------------------------------------------------------------------
# SAVE DAY CORRECTION LOG
# ------------------------------------------------------------------------------

day_correction_log <- get_day_correction_log()

if (nrow(day_correction_log) > 0) {
  cat(sprintf(
    "\n⚠ %d participants required day correction:\n",
    nrow(day_correction_log)
  ))
  print(day_correction_log)

  # Save log for review
  log_file <- file.path(paths$processed, "firenze_day_corrections.csv")
  readr::write_csv(day_correction_log, log_file)
  cat(sprintf("\n✓ Day correction log saved to: %s\n", log_file))
}

cat("\n========================================\n")

# ==============================================================================
# END OF SCRIPT
# ==============================================================================
