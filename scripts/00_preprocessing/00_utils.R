# ==============================================================================
# 00_utils.R
# ==============================================================================
# Project: Binge Eating and Self-Compassion EMA Study
# Purpose: Common utility functions for data preprocessing
# Author:  [Your Name]
# Date:    January 2026
#
# This script contains all helper functions used across preprocessing scripts.
# Source this file at the beginning of each preprocessing script.
# ==============================================================================

# ------------------------------------------------------------------------------
# PACKAGE LOADING
# ------------------------------------------------------------------------------

#' Load all required packages for preprocessing
#' @return NULL (loads packages as side effect
load_preprocessing_packages <- function() {
  suppressPackageStartupMessages({
    library(dplyr)
    library(tidyr)
    library(purrr)
    library(stringr)
    library(readr)
    library(readxl)
    library(haven)
    library(lubridate)
    library(here)
    library(forcats)
  })
  invisible(NULL)
}

# ------------------------------------------------------------------------------
# RESPONSE CONVERSION FUNCTIONS
# ------------------------------------------------------------------------------
# These functions convert Italian text responses to numeric codes.
# Each scale has its own conversion function to handle different response formats.

#' Convert Self-Compassion Scale responses (Italian) to numeric
#' @param x Character vector with Italian response labels
#' @return Integer vector (1-5 scale)
convert_sc <- function(x) {
  dplyr::case_when(
    x == "Totalmente falso" ~ 1L,
    x == "Un po' falso" ~ 2L,
    x == "Né vero né falso" ~ 3L,
    x == "Un po' vero" ~ 4L,
    x == "Totalmente vero" ~ 5L,
    TRUE ~ NA_integer_
  )
}

#' Convert Weight Bias Internalization Scale responses (Italian) to numeric
#' @param x Character vector with Italian response labels
#' @return Integer vector (1-7 scale)
convert_wbis <- function(x) {
  dplyr::case_when(
    x == "Fortemente in disaccordo" ~ 1L,
    x == "Moderatamente in disaccordo" ~ 2L,
    x == "Lievemente in disaccordo" ~ 3L,
    x == "Né in disaccordo né in accordo" ~ 4L,
    x == "Lievemente in accordo" ~ 5L,
    x == "Moderatamente in accordo" ~ 6L,
    x == "Fortemente in accordo" ~ 7L,
    TRUE ~ NA_integer_
  )
}

#' Convert Affect items responses (Italian) to numeric
#' @param x Character vector with Italian response labels
#' @return Integer vector (1-4 scale)
convert_affect <- function(x) {
  dplyr::case_when(
    x == "Per niente" ~ 1L,
    x == "Un po'" ~ 2L,
    x == "Abbastanza" ~ 3L,
    x == "Estremamente" ~ 4L,
    TRUE ~ NA_integer_
  )
}

#' Convert Binge Eating Scale responses (Italian) to numeric
#' @param x Character vector with Italian response labels
#' @return Integer vector (1-5 scale)
convert_bes <- function(x) {
  dplyr::case_when(
    x == "Per niente" ~ 1L,
    x == "Un poco" ~ 2L,
    x == "Abbastanza" ~ 3L,
    x == "Molto" ~ 4L,
    x == "Estremamente" ~ 5L,
    TRUE ~ NA_integer_
  )
}

#' Convert Perceived Stress responses (Italian) to numeric
#' Note: Uses same labels as affect scale
#' @param x Character vector with Italian response labels
#' @return Integer vector (1-4 scale)
convert_perceived_stress <- function(x) {
  dplyr::case_when(
    x == "Per niente" ~ 1L,
    x == "Un po'" ~ 2L,
    x == "Abbastanza" ~ 3L,
    x == "Estremamente" ~ 4L,
    TRUE ~ NA_integer_
  )
}

# ------------------------------------------------------------------------------
# TIME BAND FUNCTIONS
# ------------------------------------------------------------------------------
# EMA notifications are organized into 5 daily time bands.
# These functions handle the mapping between beep numbers and time bands.

#' Define standard time band levels (ordered factor levels)
TIME_BAND_LEVELS <- c(
  "t1_morning",
  "t2_midmorning",
  "t3_afternoon",
  "t4_evening",
  "t5_late_evening"
)

#' Convert beep number (1-5) to time band label
#' @param beep Integer vector of beep numbers (1-5)
#' @return Character vector of time band labels
beep_to_time_band <- function(beep) {
  dplyr::case_when(
    beep == 1L ~ "t1_morning",
    beep == 2L ~ "t2_midmorning",
    beep == 3L ~ "t3_afternoon",
    beep == 4L ~ "t4_evening",
    beep == 5L ~ "t5_late_evening",
    TRUE ~ NA_character_
  )
}

# ------------------------------------------------------------------------------
# DATE/TIME PARSING
# ------------------------------------------------------------------------------

#' Parse datetime strings from Firenze EMA exports
#' Handles multiple date formats commonly found in the Excel exports
#' @param datetime_string Character vector of datetime strings
#' @param tz Timezone (default: "Europe/Rome")
#' @return POSIXct vector
parse_firenze_datetime <- function(datetime_string, tz = "Europe/Rome") {
  parsed <- lubridate::parse_date_time(
    datetime_string,
    orders = c(
      "a, d b Y H:M:S",
      # "Mon, 15 Jan 2024 09:30:00"
      "d b Y H:M:S", # "15 Jan 2024 09:30:00"
      "Y-m-d H:M:S", # "2024-01-15 09:30:00"
      "d/m/Y H:M:S" # "15/01/2024 09:30:00"
    ),
    tz = tz
  )
  as.POSIXct(parsed, tz = tz)
}

# ------------------------------------------------------------------------------
# DAY CORRECTION FUNCTIONS
# ------------------------------------------------------------------------------
# Some participants have data spanning more than the expected 7 days.
# These functions handle the correction by selecting the best 7-day window.

#' Global environment to track day correction issues (for reporting)
.day_correction_log <- new.env(parent = emptyenv())
.day_correction_log$issues <- list()

#' Clear the day correction log
#' Call this before processing a batch of files
clear_day_correction_log <- function() {
  .day_correction_log$issues <- list()
  invisible(NULL)
}

#' Get the day correction log as a data frame
#' @return Data frame with columns: user_id, original_days, max_day, action, days_kept
get_day_correction_log <- function() {
  if (length(.day_correction_log$issues) == 0) {
    return(tibble::tibble(
      user_id = character(),
      original_days = integer(),
      max_day = integer(),
      action = character(),
      days_kept = character()
    ))
  }
  dplyr::bind_rows(.day_correction_log$issues)
}

#' Log a day correction issue (internal function)
log_day_correction <- function(
  user_id,
  original_days,
  max_day,
  action,
  days_kept = NA
) {
  .day_correction_log$issues[[
    length(.day_correction_log$issues) + 1
  ]] <- tibble::tibble(
    user_id = as.character(user_id),
    original_days = as.integer(original_days),
    max_day = as.integer(max_day),
    action = action,
    days_kept = as.character(days_kept)
  )
  invisible(NULL)
}

#' Correct day numbering for participants with extra days
#'
#' The EMA protocol expects 7 days of data collection. Some participants
#' have more days due to:
#' - Starting a day early (practice day)
#' - Extending collection
#' - Protocol interruptions creating gaps
#'
#' Strategy:
#' - 7 days or fewer: no correction
#' - 8 days: remove day 1 (practice), keep days 2-8 → renumber 1-7
#' - 9 days: remove days 1 and 9, keep days 2-8 → renumber 1-7
#' - 10+ days: keep first 7 days with most data, log warning
#'
#' @param df Data frame with columns: user_id, by_subj_day
#' @param target_days Target number of days (default: 7)
#' @return Data frame with corrected day numbering
correct_day_numbering <- function(df, target_days = 7) {
  max_day <- max(df$by_subj_day, na.rm = TRUE)
  min_day <- min(df$by_subj_day, na.rm = TRUE)

  if (!is.finite(max_day) || max_day <= target_days) {
    # No correction needed
    return(df)
  }

  # Get user_id for logging (if available)
  user_id_str <- if ("user_id" %in% names(df)) {
    unique(df$user_id)[1]
  } else {
    "unknown"
  }

  days_present <- sort(unique(df$by_subj_day))
  n_days_present <- length(days_present)

  if (max_day == 8) {
    # Remove day 1 (likely practice day), shift remaining days
    df <- df %>%
      dplyr::filter(by_subj_day != 1) %>%
      dplyr::mutate(by_subj_day = by_subj_day - 1L)

    log_day_correction(
      user_id_str,
      n_days_present,
      max_day,
      "removed_day_1",
      "2-8 → 1-7"
    )
  } else if (max_day == 9) {
    # Remove day 1 and day 9, shift remaining days
    df <- df %>%
      dplyr::filter(!by_subj_day %in% c(1, 9)) %>%
      dplyr::mutate(by_subj_day = by_subj_day - 1L)

    log_day_correction(
      user_id_str,
      n_days_present,
      max_day,
      "removed_days_1_and_9",
      "2-8 → 1-7"
    )
  } else {
    # More than 9 days: select 7 consecutive days with most observations

    if (n_days_present <= target_days) {
      # Few unique days despite high max - just renumber
      df <- df %>%
        dplyr::mutate(
          by_subj_day = as.integer(factor(by_subj_day))
        )

      log_day_correction(
        user_id_str,
        n_days_present,
        max_day,
        "renumbered_sparse",
        paste(days_present, collapse = ",")
      )

      message(
        sprintf(
          "  [%s] %d unique days (max=%d). Renumbered to 1-%d.",
          user_id_str,
          n_days_present,
          max_day,
          n_days_present
        )
      )
      return(df)
    }

    # Count observations per day
    obs_per_day <- df %>%
      dplyr::count(by_subj_day, name = "n_obs")

    # Find best 7-day consecutive window
    best_start <- 1
    best_obs <- 0

    for (start_idx in 1:(n_days_present - target_days + 1)) {
      window_days <- days_present[start_idx:(start_idx + target_days - 1)]
      window_obs <- obs_per_day %>%
        dplyr::filter(by_subj_day %in% window_days) %>%
        dplyr::pull(n_obs) %>%
        sum()

      if (window_obs > best_obs) {
        best_obs <- window_obs
        best_start <- start_idx
      }
    }

    # Keep only the best window
    keep_days <- days_present[best_start:(best_start + target_days - 1)]

    df <- df %>%
      dplyr::filter(by_subj_day %in% keep_days) %>%
      dplyr::mutate(
        by_subj_day = as.integer(factor(by_subj_day))
      )

    log_day_correction(
      user_id_str,
      n_days_present,
      max_day,
      "selected_best_window",
      paste(keep_days, collapse = ",")
    )

    message(
      sprintf(
        "  [%s] %d days (max=%d) → kept days [%s], renumbered to 1-%d.",
        user_id_str,
        n_days_present,
        max_day,
        paste(keep_days, collapse = ","),
        target_days
      )
    )
  }

  df
}

# ------------------------------------------------------------------------------
# ID NORMALIZATION
# ------------------------------------------------------------------------------
# Functions for cleaning and standardizing participant IDs for matching.

#' Normalize participant ID string for matching
#' Useful for fuzzy matching between EMA and questionnaire data
#' @param id Character vector of participant IDs
#' @return Character vector of normalized IDs (lowercase, trimmed, standardized)
normalize_id <- function(id) {
  id %>%
    as.character() %>%
    stringr::str_squish() %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[^a-z0-9_]", "_") %>%
    stringr::str_replace_all("_+", "_") %>%
    stringr::str_remove("^_|_$")
}

#' Extract components from Firenze-style participant ID
#' IDs follow pattern: initials_surname_birthdate_code_suffix
#' Example: "ma_ro_1993_08_28_376_m"
#' @param id Character vector of participant IDs
#' @return Data frame with extracted components
parse_firenze_id <- function(id) {
  # Pattern: 2 letters _ 2 letters _ 4 digits (year) _ rest
  tibble::tibble(
    original_id = id,
    normalized = normalize_id(id)
  ) %>%
    tidyr::extract(
      normalized,
      into = c("initials", "surname", "birth_year", "remainder"),
      regex = "^([a-z]{2})_([a-z]{2})_(\\d{4})_(.+)$",
      remove = FALSE
    )
}

# ------------------------------------------------------------------------------
# DATA VALIDATION FUNCTIONS
# ------------------------------------------------------------------------------

#' Check EMA data structure and report issues
#' @param df Data frame to validate
#' @param expected_days Expected number of days (default: 7)
#' @param expected_beeps Expected beeps per day (default: 5)
#' @return List with validation results
validate_ema_structure <- function(df, expected_days = 7, expected_beeps = 5) {
  # Check required columns
  required_cols <- c("user_id", "by_subj_day")
  missing_cols <- setdiff(required_cols, names(df))

  if (length(missing_cols) > 0) {
    stop(sprintf(
      "Missing required columns: %s",
      paste(missing_cols, collapse = ", ")
    ))
  }

  # Summary by participant
  participant_summary <- df %>%
    dplyr::group_by(user_id) %>%
    dplyr::summarise(
      n_obs = dplyr::n(),
      n_days = dplyr::n_distinct(by_subj_day),
      min_day = min(by_subj_day, na.rm = TRUE),
      max_day = max(by_subj_day, na.rm = TRUE),
      .groups = "drop"
    )

  # Identify issues
  issues <- list(
    wrong_n_days = participant_summary %>%
      dplyr::filter(n_days != expected_days) %>%
      dplyr::pull(user_id),

    day_range_issues = participant_summary %>%
      dplyr::filter(min_day != 1 | max_day != expected_days) %>%
      dplyr::pull(user_id)
  )

  # Beeps per day check
  beeps_per_day <- df %>%
    dplyr::group_by(user_id, by_subj_day) %>%
    dplyr::summarise(n_beeps = dplyr::n(), .groups = "drop")

  issues$irregular_beeps <- beeps_per_day %>%
    dplyr::filter(n_beeps != expected_beeps) %>%
    dplyr::distinct(user_id) %>%
    dplyr::pull(user_id)

  # Return results
  list(
    n_participants = dplyr::n_distinct(df$user_id),
    n_observations = nrow(df),
    participant_summary = participant_summary,
    beeps_per_day_summary = table(beeps_per_day$n_beeps),
    issues = issues,
    is_valid = all(sapply(issues, length) == 0)
  )
}

#' Print validation report
#' @param validation_result Output from validate_ema_structure()
print_validation_report <- function(validation_result) {
  cat("\n========== EMA Data Validation Report ==========\n\n")

  cat(sprintf("Total participants: %d\n", validation_result$n_participants))
  cat(sprintf("Total observations: %d\n", validation_result$n_observations))

  cat("\nBeeps per day distribution:\n")
  print(validation_result$beeps_per_day_summary)

  if (validation_result$is_valid) {
    cat("\n✓ All validations passed!\n")
  } else {
    cat("\n⚠ Issues detected:\n")

    if (length(validation_result$issues$wrong_n_days) > 0) {
      cat(sprintf(
        "  - Wrong number of days: %d participants\n",
        length(validation_result$issues$wrong_n_days)
      ))
    }

    if (length(validation_result$issues$day_range_issues) > 0) {
      cat(sprintf(
        "  - Day range issues: %d participants\n",
        length(validation_result$issues$day_range_issues)
      ))
    }

    if (length(validation_result$issues$irregular_beeps) > 0) {
      cat(sprintf(
        "  - Irregular beeps per day: %d participants\n",
        length(validation_result$issues$irregular_beeps)
      ))
    }
  }

  cat("\n================================================\n")
  invisible(validation_result)
}

# ------------------------------------------------------------------------------
# COLUMN RENAME MAPPINGS
# ------------------------------------------------------------------------------
# Standard column names used across all datasets

#' Column renaming map for Firenze EMA Excel files
#' Maps original Excel column names to standardized names
FIRENZE_EMA_RENAME_MAP <- c(
  # Datetime
  "dat_time" = "Date and time",

  # Self-Compassion Scale items
  "SC_1" = "scs_pos_1 (multipleChoice)",
  "SC_2" = "scs_neg_2 (multipleChoice)",
  "SC_3" = "scs_pos_3 (multipleChoice)",
  "SC_4" = "scs_neg_4 (multipleChoice)",
  "SC_5" = "scs_pos_5 (multipleChoice)",
  "SC_6" = "scs_neg_6 (multipleChoice)",
  "SC_7" = "scs_neg_7_new (multipleChoice)",
  "SC_8" = "scs_pos_8_new (multipleChoice)",

  # Weight Bias Internalization Scale items
  "WBIS_1" = "weight_quest_1 (multipleChoice)",
  "WBIS_2" = "weight_quest_2 (multipleChoice)",
  "WBIS_3" = "weight_quest_3 (multipleChoice)",

  # Affect items
  "affect_Frustrat_Ang" = "emotion_quest_1 (multipleChoice)",
  "affect_Sad_Depressed" = "emotion_quest_2 (multipleChoice)",
  "affect_stress" = "emotion_quest_3 (multipleChoice)",

  # Eating
  "Did_you_eat" = "yesno_eating (yesno)",

  # Perceived Stress
  "PerceivedStress_1" = "quest_1 (multipleChoice)",
  "PerceivedStress_2" = "quest_2 (multipleChoice)",

  # Binge Eating Scale items
  "BES_1" = "eating_1 (multipleChoice)",
  "BES_2" = "eating_2 (multipleChoice)",
  "BES_3" = "eating_3 (multipleChoice)",
  "BES_4" = "eating_4 (multipleChoice)",
  "BES_5" = "eating_5 (multipleChoice)",
  "BES_6" = "eating_6 (multipleChoice)"
)

#' Columns to remove from Firenze EMA files (system/intro columns)
FIRENZE_EMA_DROP_COLS <- c(
  "system_devspecs (devicespecs)",
  "intro_phrase1 (basic)",
  "intro_phrase2 (basic)"
)

# ------------------------------------------------------------------------------
# FILE PATH HELPERS
# ------------------------------------------------------------------------------

#' Get standard paths for the project
#' @return Named list of paths
get_project_paths <- function() {
  list(
    # Raw data
    raw_flagstaff = here::here("data", "raw", "flagstaff"),
    raw_firenze_ema = here::here("data", "raw", "firenze", "ema", "controls"),
    raw_firenze_quest = here::here("data", "raw", "firenze", "quest"),

    # Processed data
    processed = here::here("data", "processed"),

    # Dictionaries
    dictionaries = here::here("data", "dictionaries"),

    # Output
    figures = here::here("output", "figures"),
    tables = here::here("output", "tables"),
    reports = here::here("output", "reports"),

    # Models
    models = here::here("models")
  )
}

#' Create all project directories if they don't exist
setup_project_dirs <- function() {
  paths <- get_project_paths()

  for (path in paths) {
    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE)
      message(sprintf("Created: %s", path))
    }
  }

  invisible(paths)
}

# ==============================================================================
# END OF FILE
# ==============================================================================
