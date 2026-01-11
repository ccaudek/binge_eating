# ==============================================================================
# 06_imputation.R
# ==============================================================================
# Project: Binge Eating and Self-Compassion EMA Study
# Purpose: Handle missing data through imputation
# Author:  Corrado Caudek
# Date:    Sat Jan 10 09:23:56 2026
#
# INPUT:
#   - data/processed/d_for_analysis.rds (from 05_merge_datasets.R)
#
# OUTPUT:
#   - data/processed/d_imp.rds (imputed dataset - main analysis file)
#   - data/processed/d_imp.csv (CSV version for inspection)
#   - data/processed/imputation_diagnostics.pdf (visual diagnostics)
#
# IMPUTATION STRATEGY:
#   - Use missRanger (random forest-based) for single imputation
#   - Impute within-person (respects multilevel structure via PMM)
#   - Do NOT impute outcome variables (bes, stress) - keep as NA
#   - BES is only valid when has_eaten == 1
#
# NOTES:
#   - EMA data has complex missing patterns (non-response, partial response)
#   - Imputation is for predictors only, not outcomes
#   - Consider sensitivity analyses with complete cases
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

# Additional packages for imputation and visualization
suppressPackageStartupMessages({
  library(missRanger)
  library(visdat)
})

# Get paths
paths <- get_project_paths()

# ------------------------------------------------------------------------------
# LOAD DATA
# ------------------------------------------------------------------------------

cat("\n========================================\n")
cat("Missing Data Imputation\n")
cat("========================================\n\n")

# Load analysis data
analysis_file <- file.path(paths$processed, "d_for_analysis.rds")
if (!file.exists(analysis_file)) {
  stop("Analysis data not found. Run 05_merge_datasets.R first.")
}

d <- readRDS(analysis_file)

cat(sprintf("Loaded data: %d observations, %d variables\n", nrow(d), ncol(d)))
cat(sprintf("Participants: %d\n", dplyr::n_distinct(d$user_id)))

# ------------------------------------------------------------------------------
# MISSING DATA EXPLORATION
# ------------------------------------------------------------------------------

cat("\n--- Missing Data Patterns ---\n")

# Overall missing rates
missing_rates <- d %>%
  dplyr::summarise(
    dplyr::across(
      everything(),
      ~ round(sum(is.na(.)) / dplyr::n() * 100, 2)
    )
  ) %>%
  tidyr::pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "pct_missing"
  ) %>%
  dplyr::filter(pct_missing > 0) %>%
  dplyr::arrange(dplyr::desc(pct_missing))

cat("\nVariables with missing data:\n")
print(missing_rates, n = 30)

# Missing by group
cat("\nMissing rates by group:\n")
missing_by_group <- d %>%
  dplyr::group_by(group) %>%
  dplyr::summarise(
    n = dplyr::n(),
    pct_miss_ssc = round(sum(is.na(ssc)) / dplyr::n() * 100, 2),
    pct_miss_neg_aff = round(sum(is.na(neg_aff)) / dplyr::n() * 100, 2),
    pct_miss_wbis = round(sum(is.na(wbis)) / dplyr::n() * 100, 2),
    pct_miss_bes = round(sum(is.na(bes)) / dplyr::n() * 100, 2),
    pct_miss_stress = round(sum(is.na(stress)) / dplyr::n() * 100, 2),
    .groups = "drop"
  )
print(missing_by_group)

# Visualize missing data pattern
cat("\nGenerating missing data visualization...\n")
pdf_file <- file.path(paths$processed, "imputation_diagnostics.pdf")
pdf(pdf_file, width = 12, height = 8)

# Plot 1: Overall missing pattern
p1 <- visdat::vis_miss(d %>% dplyr::select(-user_id), warn_large_data = FALSE) +
  ggplot2::ggtitle("Missing Data Pattern - All Variables")
print(p1)

# Plot 2: Missing pattern for key variables only
key_vars <- c(
  "ssc",
  "ssc_sc",
  "ssc_usc",
  "wbis",
  "neg_aff",
  "bes",
  "stress",
  "has_eaten"
)
p2 <- visdat::vis_miss(
  d %>% dplyr::select(dplyr::any_of(key_vars)),
  warn_large_data = FALSE
) +
  ggplot2::ggtitle("Missing Data Pattern - Key Variables")
print(p2)

dev.off()
cat(sprintf("✓ Diagnostics saved to: %s\n", pdf_file))

# ------------------------------------------------------------------------------
# DEFINE IMPUTATION STRATEGY
# ------------------------------------------------------------------------------

cat("\n--- Imputation Strategy ---\n")

# Variables TO impute (predictors)
vars_to_impute <- c(
  "ssc",
  "ssc_sc",
  "ssc_usc",
  "wbis",
  "neg_aff",
  "has_eaten"
)

# Variables NOT to impute (outcomes or identifiers)
vars_no_impute <- c(
  "bes", # Outcome - only valid when has_eaten == 1
  "stress", # Outcome
  "group", # Identifier
  "user_id", # Identifier
  "by_subj_day",
  "beep",
  "time_band"
)

# Individual items - impute if computing scores, otherwise skip
# For simplicity, we impute scores directly (not individual items)

cat("Variables to impute:\n")
cat(paste(" -", vars_to_impute, collapse = "\n"), "\n")

cat("\nVariables NOT imputed (outcomes/identifiers):\n")
cat(paste(" -", vars_no_impute, collapse = "\n"), "\n")

# ------------------------------------------------------------------------------
# PREPARE DATA FOR IMPUTATION
# ------------------------------------------------------------------------------

cat("\n--- Preparing data ---\n")

# Create dataset for imputation
# IMPORTANT: Remove bes and stress - they should NOT be imputed
# - bes: only valid when has_eaten == 1 (structural missing)
# - stress: outcome variable, keep original missing pattern
d_for_imp <- d %>%
  dplyr::select(
    # Identifiers (used for grouping, not imputed)
    group,
    user_id,
    by_subj_day,
    beep,

    # Variables to impute
    ssc,
    ssc_sc,
    ssc_usc,
    wbis,
    neg_aff,
    has_eaten
  ) %>%
  dplyr::mutate(
    # Convert factors for imputation
    group = as.factor(group),
    user_id = as.factor(user_id),
    by_subj_day = as.integer(by_subj_day),
    has_eaten = as.factor(has_eaten)
  )

# Keep bes and stress separately (will be added back after imputation)
d_outcomes <- d %>%
  dplyr::select(bes, stress)

cat(sprintf(
  "Data for imputation: %d obs × %d vars\n",
  nrow(d_for_imp),
  ncol(d_for_imp)
))
cat("Note: bes and stress excluded from imputation (will be preserved as-is)\n")

# ------------------------------------------------------------------------------
# IMPUTATION WITH missRanger
# ------------------------------------------------------------------------------

cat("\n--- Running missRanger imputation ---\n")

# missRanger uses random forests with predictive mean matching (PMM)
# PMM helps preserve the distribution of imputed values

# Set seed for reproducibility
set.seed(42)

# Run imputation
# pmm.k = number of nearest neighbors for PMM
# num.trees = number of trees in random forest
# verbose = 1 shows progress
#
# Formula specifies:
# - LEFT side: variables TO impute (ssc, ssc_sc, ssc_usc, wbis, neg_aff, has_eaten)
# - RIGHT side: variables to USE AS PREDICTORS (group, user_id, by_subj_day, beep + the imputed vars)

d_imp <- missRanger::missRanger(
  data = d_for_imp,
  formula = ssc + ssc_sc + ssc_usc + wbis + neg_aff + has_eaten ~ .,
  pmm.k = 5, # PMM with 5 neighbors
  num.trees = 100, # Number of trees
  verbose = 1, # Show progress
  seed = 42,
  returnOOB = TRUE # Return out-of-bag error
)

cat("\n✓ Imputation complete\n")

# Check imputation result
cat("\nPost-imputation missing rates (imputed variables):\n")
post_imp_missing <- d_imp %>%
  dplyr::summarise(
    dplyr::across(
      c(ssc, ssc_sc, ssc_usc, wbis, neg_aff, has_eaten),
      ~ sum(is.na(.))
    )
  )
print(post_imp_missing)

# Add back the outcome variables (bes, stress) with original missing pattern
d_imp <- d_imp %>%
  dplyr::bind_cols(d_outcomes)

cat("\nOutcome variables (NOT imputed, original missing preserved):\n")
cat(sprintf(
  "  bes missing: %d (%.1f%%)\n",
  sum(is.na(d_imp$bes)),
  100 * sum(is.na(d_imp$bes)) / nrow(d_imp)
))
cat(sprintf(
  "  stress missing: %d (%.1f%%)\n",
  sum(is.na(d_imp$stress)),
  100 * sum(is.na(d_imp$stress)) / nrow(d_imp)
))

# ------------------------------------------------------------------------------
# VALIDATE IMPUTATION
# ------------------------------------------------------------------------------

cat("\n--- Validating imputation ---\n")

# Compare distributions before and after imputation
cat("\nComparing distributions (observed vs imputed):\n")

compare_vars <- c("ssc", "ssc_sc", "ssc_usc", "wbis", "neg_aff")

for (var in compare_vars) {
  obs_mean <- mean(d_for_imp[[var]], na.rm = TRUE)
  imp_mean <- mean(d_imp[[var]], na.rm = TRUE)
  obs_sd <- sd(d_for_imp[[var]], na.rm = TRUE)
  imp_sd <- sd(d_imp[[var]], na.rm = TRUE)

  cat(sprintf(
    "  %s: obs mean=%.2f (sd=%.2f), imp mean=%.2f (sd=%.2f)\n",
    var,
    obs_mean,
    obs_sd,
    imp_mean,
    imp_sd
  ))
}

# Visualize: Add comparison plots to PDF
pdf(pdf_file, width = 12, height = 8, onefile = TRUE)

# Re-create missing pattern plot
p1 <- visdat::vis_miss(
  d_for_imp %>% dplyr::select(-user_id),
  warn_large_data = FALSE
) +
  ggplot2::ggtitle("Missing Data Pattern - Before Imputation")
print(p1)

# Post-imputation check
p2 <- visdat::vis_miss(
  d_imp %>% dplyr::select(-user_id),
  warn_large_data = FALSE
) +
  ggplot2::ggtitle("Missing Data Pattern - After Imputation")
print(p2)

# Distribution comparisons
for (var in compare_vars) {
  # Create comparison data
  comp_df <- data.frame(
    value = c(d_for_imp[[var]], d_imp[[var]]),
    type = rep(c("Original (with NA)", "Imputed"), each = nrow(d_for_imp))
  )

  p <- ggplot2::ggplot(comp_df, ggplot2::aes(x = value, fill = type)) +
    ggplot2::geom_density(alpha = 0.5) +
    ggplot2::labs(
      title = sprintf("Distribution comparison: %s", var),
      x = var,
      y = "Density"
    ) +
    ggplot2::theme_minimal()
  print(p)
}

dev.off()
cat(sprintf("\n✓ Updated diagnostics saved to: %s\n", pdf_file))

# ------------------------------------------------------------------------------
# MERGE IMPUTED DATA BACK WITH FULL DATASET
# ------------------------------------------------------------------------------

cat("\n--- Creating final imputed dataset ---\n")

# Get columns from original data that weren't in imputation
other_cols <- setdiff(names(d), names(d_imp))

# Merge imputed values back
d_final <- d_imp %>%
  dplyr::bind_cols(
    d %>% dplyr::select(dplyr::all_of(other_cols))
  ) %>%
  # Reorder columns to match original
  dplyr::select(names(d)) %>%
  # Ensure proper types
  dplyr::mutate(
    group = factor(group, levels = c("control", "binge_eating")),
    user_id = factor(user_id),
    by_subj_day = factor(by_subj_day),
    time_band = factor(time_band, levels = TIME_BAND_LEVELS),
    has_eaten = factor(has_eaten, levels = c("not_eaten", "eaten"))
  ) %>%
  dplyr::arrange(group, user_id, by_subj_day, beep)

# ------------------------------------------------------------------------------
# SAVE IMPUTED DATA
# ------------------------------------------------------------------------------

# Save as RDS (main analysis file)
imp_file <- file.path(paths$processed, "d_imp.rds")
saveRDS(d_final, imp_file)
cat(sprintf("\n✓ Imputed data saved to: %s\n", imp_file))

# Save as CSV for inspection
csv_file <- file.path(paths$processed, "d_imp.csv")
readr::write_csv(d_final, csv_file)
cat(sprintf("✓ CSV version saved to: %s\n", csv_file))

# ------------------------------------------------------------------------------
# FINAL SUMMARY
# ------------------------------------------------------------------------------

cat("\n========================================\n")
cat("Imputation Complete\n")
cat("========================================\n\n")

cat("Final dataset summary:\n")
cat(sprintf("  Observations: %d\n", nrow(d_final)))
cat(sprintf("  Participants: %d\n", dplyr::n_distinct(d_final$user_id)))
cat(sprintf("  Variables: %d\n", ncol(d_final)))

cat("\nBy group:\n")
d_final %>%
  dplyr::group_by(group) %>%
  dplyr::summarise(
    n_participants = dplyr::n_distinct(user_id),
    n_observations = dplyr::n(),
    .groups = "drop"
  ) %>%
  print()

cat("\nScale score summary (post-imputation):\n")
d_final %>%
  dplyr::group_by(group) %>%
  dplyr::summarise(
    ssc_mean = round(mean(ssc, na.rm = TRUE), 2),
    ssc_sc_mean = round(mean(ssc_sc, na.rm = TRUE), 2),
    ssc_usc_mean = round(mean(ssc_usc, na.rm = TRUE), 2),
    neg_aff_mean = round(mean(neg_aff, na.rm = TRUE), 2),
    wbis_mean = round(mean(wbis, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  print()

cat("\n--- Files created ---\n")
cat(sprintf("  1. %s (main analysis file)\n", imp_file))
cat(sprintf("  2. %s (CSV for inspection)\n", csv_file))
cat(sprintf("  3. %s (diagnostic plots)\n", pdf_file))

cat("\n--- Notes ---\n")
cat("  • BES and stress were NOT imputed (outcomes)\n")
cat("  • BES is only valid when has_eaten == 'eaten'\n")
cat("  • Consider sensitivity analysis with complete cases\n")

cat("\n--- Ready for analysis! ---\n")
cat("The imputed dataset (d_imp.rds) is ready for:\n")
cat("  • scripts/01_analysis/01_between_within.R\n")
cat("  • scripts/01_analysis/02_meal_timing.R\n")
cat("  • scripts/01_analysis/03_temporal_dynamics.R\n")

cat("\n========================================\n")

# ==============================================================================
# OPTIONAL: MICE-BASED MULTILEVEL IMPUTATION
# ==============================================================================
#
# For more sophisticated multilevel imputation that explicitly accounts for
# the nested structure (observations within participants), you can use the
# mice package with 2-level methods. This is commented out as it requires
# more computation time.
#
# library(mice)
#
# # Build method vector
# meth <- make.method(d_for_imp)
# meth[] <- ""  # Default: don't impute
#
# # Continuous variables: use 2-level normal imputation
# meth[c("ssc", "ssc_sc", "ssc_usc", "wbis", "neg_aff")] <- "2l.norm"
#
# # Binary variable: use 2-level binary imputation
# meth["has_eaten"] <- "2l.bin"
#
# # Build predictor matrix
# pred <- make.predictorMatrix(d_for_imp)
# pred[,] <- 0
#
# # Set cluster variable (user_id must be integer)
# d_for_imp$id <- as.integer(factor(d_for_imp$user_id))
# pred[c("ssc", "ssc_sc", "ssc_usc", "wbis", "neg_aff", "has_eaten"), "id"] <- -2
#
# # Run mice
# imp_mice <- mice(d_for_imp, m = 5, method = meth, predictorMatrix = pred,
#                  maxit = 20, seed = 42)
#
# # Get single imputed dataset (or use all m datasets for multiple imputation)
# d_imp_mice <- complete(imp_mice, 1)
#
# ==============================================================================

# ==============================================================================
# END OF SCRIPT
# ==============================================================================
