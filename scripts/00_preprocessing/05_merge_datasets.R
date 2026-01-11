# ==============================================================================
# 05_merge_datasets.R
# ==============================================================================
# Project: Binge Eating and Self-Compassion EMA Study
# Purpose: Merge EMA data from Flagstaff (clinical) and Florence (control)
# Author:  [Your Name]
# Date:    January 2026
#
# INPUT:
#   - data/processed/d_flagstaff.rds (clinical group - binge eating)
#   - data/processed/d_firenze_ema.rds (control group)
#   - (Optional) data/processed/d_firenze_matched.rds (if questionnaire data needed)
#
# OUTPUT:
#   - data/processed/d_ema_combined.rds (all EMA data, both groups)
#   - data/processed/d_analysis_raw.rds (with computed scale scores)
#
# NOTES:
#   - Flagstaff has SC_1-SC_6, Florence has SC_1-SC_8
#   - Common variables are kept; group-specific variables preserved as NA
#   - Scale scores computed from available items
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

# ------------------------------------------------------------------------------
# LOAD DATA
# ------------------------------------------------------------------------------

cat("\n========================================\n")
cat("Merging EMA Datasets\n")
cat("========================================\n\n")

# Load Flagstaff data (clinical group)
flagstaff_file <- file.path(paths$processed, "d_flagstaff.rds")
if (!file.exists(flagstaff_file)) {
  stop("Flagstaff data not found. Run 01_read_flagstaff.R first.")
}
d_flagstaff <- readRDS(flagstaff_file)

# Load Florence data (control group)
firenze_file <- file.path(paths$processed, "d_firenze_ema.rds")
if (!file.exists(firenze_file)) {
  stop("Florence data not found. Run 02_read_firenze_ema.R first.")
}
d_firenze <- readRDS(firenze_file)

cat("Loaded datasets:\n")
cat(sprintf("  Flagstaff (clinical): %d participants, %d observations\n",
            dplyr::n_distinct(d_flagstaff$user_id), nrow(d_flagstaff)))
cat(sprintf("  Florence (control): %d participants, %d observations\n",
            dplyr::n_distinct(d_firenze$user_id), nrow(d_firenze)))

# ------------------------------------------------------------------------------
# HARMONIZE COLUMN STRUCTURES
# ------------------------------------------------------------------------------

cat("\n--- Harmonizing variables ---\n")

# Check column differences
cols_flagstaff <- names(d_flagstaff)
cols_firenze <- names(d_firenze)

common_cols <- intersect(cols_flagstaff, cols_firenze)
only_flagstaff <- setdiff(cols_flagstaff, cols_firenze)
only_firenze <- setdiff(cols_firenze, cols_firenze)

cat(sprintf("Common columns: %d\n", length(common_cols)))
cat(sprintf("Only in Flagstaff: %s\n", 
            if(length(only_flagstaff) > 0) paste(only_flagstaff, collapse = ", ") else "none"))
cat(sprintf("Only in Florence: %s\n",
            if(length(only_firenze) > 0) paste(only_firenze, collapse = ", ") else "none"))

# Note: Florence has SC_7, SC_8, SC_7R which Flagstaff doesn't have
# bind_rows will automatically fill with NA

# Ensure consistent factor levels for group
d_flagstaff <- d_flagstaff %>%
  dplyr::mutate(
    group = factor(group, levels = c("control", "binge_eating"))
  )

d_firenze <- d_firenze %>%
  dplyr::mutate(
    group = factor(group, levels = c("control", "binge_eating"))
  )

# ------------------------------------------------------------------------------
# COMBINE DATASETS
# ------------------------------------------------------------------------------

cat("\n--- Combining datasets ---\n")

d_combined <- dplyr::bind_rows(
  d_flagstaff,
  d_firenze
) %>%
  dplyr::arrange(group, user_id, by_subj_day, beep)

cat(sprintf("\nCombined dataset:\n"))
cat(sprintf("  Total participants: %d\n", dplyr::n_distinct(d_combined$user_id)))
cat(sprintf("  Total observations: %d\n", nrow(d_combined)))

# Group breakdown
cat("\nBy group:\n")
group_summary <- d_combined %>%
  dplyr::group_by(group) %>%
  dplyr::summarise(
    n_participants = dplyr::n_distinct(user_id),
    n_observations = dplyr::n(),
    .groups = "drop"
  )
print(group_summary)

# ------------------------------------------------------------------------------
# COMPUTE SCALE SCORES
# ------------------------------------------------------------------------------

cat("\n--- Computing scale scores ---\n")

d_analysis <- d_combined %>%
  dplyr::mutate(
    # Self-Compassion (positive items): SC_1, SC_3, SC_5
    # Using only items available in both datasets
    ssc_sc = SC_1 + SC_3 + SC_5,
    
    # Self-Criticism (negative items, NOT reverse-coded): SC_2, SC_4, SC_6
    ssc_usc = SC_2 + SC_4 + SC_6,
    
    # Total State Self-Compassion (positive + reverse-coded negative)
    # SC_2R, SC_4R, SC_6R are reverse-coded versions
    ssc = SC_1 + SC_3 + SC_5 + SC_2R + SC_4R + SC_6R,
    
    # Weight Bias Internalization Scale (sum of 3 items)
    wbis = WBIS_1 + WBIS_2 + WBIS_3,
    
    # Negative Affect (sum of 3 affect items)
    neg_aff = affect_Frustrat_Ang + affect_Sad_Depressed + affect_stress,
    
    # Binge Eating Scale (sum of 6 items)
    # Note: BES only meaningful when has_eaten == 1
    bes = BES_1 + BES_2 + BES_3 + BES_4 + BES_5 + BES_6,
    
    # Perceived Stress (sum of 2 items)
    stress = as.numeric(PerceivedStress_1) + as.numeric(PerceivedStress_2),
    
    # Binary eating indicator
    has_eaten = dplyr::case_when(
      Did_you_eat == 1 ~ "eaten",
      Did_you_eat == 0 ~ "not_eaten",
      TRUE ~ NA_character_
    ),
    has_eaten = factor(has_eaten, levels = c("not_eaten", "eaten"))
  )

# Summary of computed scores
cat("\nScale score summary:\n")
score_summary <- d_analysis %>%
  dplyr::group_by(group) %>%
  dplyr::summarise(
    n = dplyr::n(),
    ssc_mean = mean(ssc, na.rm = TRUE),
    ssc_sc_mean = mean(ssc_sc, na.rm = TRUE),
    ssc_usc_mean = mean(ssc_usc, na.rm = TRUE),
    neg_aff_mean = mean(neg_aff, na.rm = TRUE),
    wbis_mean = mean(wbis, na.rm = TRUE),
    bes_mean = mean(bes, na.rm = TRUE),
    .groups = "drop"
  )
print(score_summary)

# ------------------------------------------------------------------------------
# CHECK FOR ISSUES
# ------------------------------------------------------------------------------
cat("\n--- Data quality checks ---\n")

# Missing data summary
missing_summary <- d_analysis %>%
  dplyr::summarise(
    dplyr::across(
      c(ssc, ssc_sc, ssc_usc, wbis, neg_aff, bes, stress),
      ~sum(is.na(.)) / dplyr::n() * 100
    )
  ) %>%
  tidyr::pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "pct_missing"
  )

cat("\nMissing data (%):\n")
print(missing_summary)

# Check scale ranges
cat("\nScale score ranges:\n")
range_summary <- d_analysis %>%
  dplyr::summarise(
    ssc_min = min(ssc, na.rm = TRUE),
    ssc_max = max(ssc, na.rm = TRUE),
    ssc_sc_min = min(ssc_sc, na.rm = TRUE),
    ssc_sc_max = max(ssc_sc, na.rm = TRUE),
    ssc_usc_min = min(ssc_usc, na.rm = TRUE),
    ssc_usc_max = max(ssc_usc, na.rm = TRUE),
    neg_aff_min = min(neg_aff, na.rm = TRUE),
    neg_aff_max = max(neg_aff, na.rm = TRUE),
    wbis_min = min(wbis, na.rm = TRUE),
    wbis_max = max(wbis, na.rm = TRUE),
    bes_min = min(bes, na.rm = TRUE),
    bes_max = max(bes, na.rm = TRUE)
  )
print(t(range_summary))

# Expected ranges:
# ssc_sc: 3-15 (3 items × 1-5 scale)
# ssc_usc: 3-15 (3 items × 1-5 scale)
# ssc: 6-30 (6 items × 1-5 scale)
# neg_aff: 3-12 (3 items × 1-4 scale)
# wbis: 3-21 (3 items × 1-7 scale)
# bes: 6-30 (6 items × 1-5 scale)

# ------------------------------------------------------------------------------
# SAVE COMBINED DATA
# ------------------------------------------------------------------------------

# Save raw combined data (before imputation)
combined_file <- file.path(paths$processed, "d_ema_combined.rds")
saveRDS(d_combined, combined_file)
cat(sprintf("\n✓ Combined raw data saved to: %s\n", combined_file))

# Save analysis-ready data (with computed scores)
analysis_file <- file.path(paths$processed, "d_analysis_raw.rds")
saveRDS(d_analysis, analysis_file)
cat(sprintf("✓ Analysis data (with scores) saved to: %s\n", analysis_file))

# Also save as CSV for inspection
csv_file <- file.path(paths$processed, "d_ema_combined.csv")
readr::write_csv(d_combined, csv_file)
cat(sprintf("✓ CSV version saved to: %s\n", csv_file))

# ------------------------------------------------------------------------------
# CREATE ANALYSIS-READY SUBSET
# ------------------------------------------------------------------------------

cat("\n--- Creating analysis subset ---\n")

# Select key variables for main analyses
d_for_analysis <- d_analysis %>%
  dplyr::select(
    # Identifiers
    group,
    user_id,
    by_subj_day,
    beep,
    time_band,
    
    # Computed scores
    ssc,
    ssc_sc,
    ssc_usc,
    wbis,
    neg_aff,
    bes,
    stress,
    
    # Eating indicator
    has_eaten,
    Did_you_eat,
    
    # Keep individual items for potential item-level analyses
    SC_1, SC_2, SC_3, SC_4, SC_5, SC_6,
    SC_2R, SC_4R, SC_6R,
    WBIS_1, WBIS_2, WBIS_3,
    affect_Frustrat_Ang, affect_Sad_Depressed, affect_stress,
    BES_1, BES_2, BES_3, BES_4, BES_5, BES_6,
    PerceivedStress_1, PerceivedStress_2
  ) %>%
  # Ensure proper factor types
  dplyr::mutate(
    group = factor(group, levels = c("control", "binge_eating")),
    user_id = factor(user_id),
    by_subj_day = factor(by_subj_day),
    time_band = factor(time_band, levels = TIME_BAND_LEVELS)
  )

# Save analysis subset
subset_file <- file.path(paths$processed, "d_for_analysis.rds")
saveRDS(d_for_analysis, subset_file)
cat(sprintf("✓ Analysis subset saved to: %s\n", subset_file))

# ------------------------------------------------------------------------------
# SUMMARY
# ------------------------------------------------------------------------------

cat("\n========================================\n")
cat("Dataset Merging Complete\n")
cat("========================================\n\n")

cat("Final dataset summary:\n")
cat(sprintf("  Total participants: %d\n", dplyr::n_distinct(d_analysis$user_id)))
cat(sprintf("    - Clinical (binge_eating): %d\n", 
            dplyr::n_distinct(d_analysis$user_id[d_analysis$group == "binge_eating"])))
cat(sprintf("    - Control: %d\n", 
            dplyr::n_distinct(d_analysis$user_id[d_analysis$group == "control"])))
cat(sprintf("  Total observations: %d\n", nrow(d_analysis)))
cat(sprintf("  Variables: %d\n", ncol(d_for_analysis)))

cat("\n--- Files created ---\n")
cat(sprintf("  1. %s (raw combined EMA)\n", combined_file))
cat(sprintf("  2. %s (with computed scores)\n", analysis_file))
cat(sprintf("  3. %s (analysis-ready subset)\n", subset_file))
cat(sprintf("  4. %s (CSV for inspection)\n", csv_file))

cat("\n--- Next steps ---\n")
cat("1. Review missing data patterns\n")
cat("2. Run 06_imputation.R to handle missing values\n")
cat("3. Proceed with analysis scripts\n")

cat("\n========================================\n")

# ==============================================================================
# END OF SCRIPT
# ==============================================================================
