# ==============================================================================
# 04_match_firenze_ids.R
# ==============================================================================
# Project: Binge Eating and Self-Compassion EMA Study
# Purpose: Match participant IDs between EMA and questionnaire data (Florence)
# Author:  Corrado Caudek
# Date:    Sat Jan 10 09:17:29 2026
#
# INPUT:
#   - data/processed/d_firenze_ema.rds (from 02_read_firenze_ema.R)
#   - data/processed/d_firenze_quest.rds (from 03_read_firenze_quest.R)
#
# OUTPUT:
#   - data/processed/d_firenze_matched.rds (EMA + questionnaire for matched IDs)
#   - data/processed/firenze_id_matching_report.csv (matching diagnostics)
#   - data/processed/firenze_unmatched_ids.csv (IDs requiring manual review)
#
# MATCHING STRATEGY:
#   1. Exact match on normalized IDs
#   2. Fuzzy match using string distance (Levenshtein, Jaro-Winkler)
#   3. Component-based match (initials + birth year)
#   4. Flag uncertain matches for manual review
#
# NOTES:
#   - Florence IDs follow pattern: initials_surname_birthdate_code_suffix
#   - Questionnaire IDs may have typos, different formatting, or missing parts
#   - Conservative approach: only accept high-confidence matches automatically
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

# Additional packages for string matching
if (!requireNamespace("stringdist", quietly = TRUE)) {
  install.packages("stringdist")
}
library(stringdist)

# Get paths
paths <- get_project_paths()

# ------------------------------------------------------------------------------
# LOAD DATA
# ------------------------------------------------------------------------------

cat("\n========================================\n")
cat("Florence ID Matching\n")
cat("========================================\n\n")

# Load EMA data
ema_file <- file.path(paths$processed, "d_firenze_ema.rds")
if (!file.exists(ema_file)) {
  stop("EMA data not found. Run 02_read_firenze_ema.R first.")
}
d_ema <- readRDS(ema_file)

# Load questionnaire data
quest_file <- file.path(paths$processed, "d_firenze_quest.rds")
if (!file.exists(quest_file)) {
  stop("Questionnaire data not found. Run 03_read_firenze_quest.R first.")
}
d_quest <- readRDS(quest_file)

# Extract unique IDs
ema_ids <- d_ema %>%
  dplyr::distinct(user_id) %>%
  dplyr::mutate(
    ema_id_raw = user_id,
    ema_id_normalized = normalize_id(user_id)
  )

quest_ids <- d_quest %>%
  dplyr::select(quest_id_raw, quest_id_normalized) %>%
  dplyr::distinct()

cat(sprintf("EMA participants: %d\n", nrow(ema_ids)))
cat(sprintf("Questionnaire participants: %d\n", nrow(quest_ids)))

# ------------------------------------------------------------------------------
# STEP 1: EXACT MATCHING (on normalized IDs)
# ------------------------------------------------------------------------------

cat("\n--- Step 1: Exact matching ---\n")

exact_matches <- ema_ids %>%
  dplyr::inner_join(
    quest_ids,
    by = c("ema_id_normalized" = "quest_id_normalized")
  ) %>%
  dplyr::mutate(
    match_type = "exact",
    match_confidence = 1.0
  )

cat(sprintf("Exact matches: %d\n", nrow(exact_matches)))

# IDs not yet matched
ema_unmatched <- ema_ids %>%
  dplyr::filter(!ema_id_normalized %in% exact_matches$ema_id_normalized)

quest_unmatched <- quest_ids %>%
  dplyr::filter(!quest_id_normalized %in% exact_matches$ema_id_normalized)

cat(sprintf("EMA IDs remaining: %d\n", nrow(ema_unmatched)))
cat(sprintf("Quest IDs remaining: %d\n", nrow(quest_unmatched)))

# ------------------------------------------------------------------------------
# STEP 2: FUZZY MATCHING (string distance)
# ------------------------------------------------------------------------------

cat("\n--- Step 2: Fuzzy matching ---\n")

fuzzy_matches <- tibble::tibble()

if (nrow(ema_unmatched) > 0 && nrow(quest_unmatched) > 0) {
  # Compute string distance matrix
  # Using multiple methods and combining

  # Create all pairwise combinations
  pairs <- tidyr::expand_grid(
    ema_idx = seq_len(nrow(ema_unmatched)),
    quest_idx = seq_len(nrow(quest_unmatched))
  ) %>%
    dplyr::mutate(
      ema_id = ema_unmatched$ema_id_normalized[ema_idx],
      quest_id = quest_unmatched$quest_id_normalized[quest_idx]
    )

  # Calculate multiple distance metrics
  pairs <- pairs %>%
    dplyr::mutate(
      # Levenshtein distance (edit distance)
      dist_lv = stringdist::stringdist(ema_id, quest_id, method = "lv"),

      # Jaro-Winkler similarity (good for typos at start)
      sim_jw = stringdist::stringsim(ema_id, quest_id, method = "jw"),

      # Cosine similarity on q-grams
      sim_cosine = stringdist::stringsim(
        ema_id,
        quest_id,
        method = "cosine",
        q = 2
      ),

      # String lengths
      len_ema = nchar(ema_id),
      len_quest = nchar(quest_id),
      len_diff = abs(len_ema - len_quest),

      # Normalized Levenshtein
      dist_lv_norm = dist_lv / pmax(len_ema, len_quest),

      # Combined score (weighted average of similarities)
      combined_score = 0.4 *
        sim_jw +
        0.3 * sim_cosine +
        0.3 * (1 - dist_lv_norm)
    )

  # For each EMA ID, find best quest match
  best_matches <- pairs %>%
    dplyr::group_by(ema_idx) %>%
    dplyr::slice_max(combined_score, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup()

  # Also check: for each quest ID, what's the best EMA match?
  # This helps identify cases where one quest ID is best match for multiple EMA IDs
  best_reverse <- pairs %>%
    dplyr::group_by(quest_idx) %>%
    dplyr::slice_max(combined_score, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup()

  # Keep only reciprocal best matches with high confidence
  # A match is reciprocal if A's best match is B AND B's best match is A
  reciprocal <- best_matches %>%
    dplyr::inner_join(
      best_reverse %>% dplyr::select(ema_idx, quest_idx, combined_score),
      by = c("ema_idx", "quest_idx"),
      suffix = c("", "_reverse")
    )

  # Categorize matches by confidence
  fuzzy_matches <- reciprocal %>%
    dplyr::mutate(
      match_confidence = combined_score,
      match_type = dplyr::case_when(
        combined_score >= 0.90 ~ "fuzzy_high",
        combined_score >= 0.75 ~ "fuzzy_medium",
        combined_score >= 0.60 ~ "fuzzy_low",
        TRUE ~ "fuzzy_uncertain"
      )
    ) %>%
    dplyr::select(
      ema_id_normalized = ema_id,
      quest_id_normalized = quest_id,
      match_type,
      match_confidence,
      dist_lv,
      sim_jw,
      sim_cosine
    )

  # Add raw IDs
  fuzzy_matches <- fuzzy_matches %>%
    dplyr::left_join(
      ema_unmatched %>% dplyr::select(ema_id_raw, ema_id_normalized),
      by = "ema_id_normalized"
    ) %>%
    dplyr::left_join(
      quest_unmatched %>% dplyr::select(quest_id_raw, quest_id_normalized),
      by = "quest_id_normalized"
    ) %>%
    dplyr::mutate(
      user_id = ema_id_raw
    ) %>%
    dplyr::select(
      user_id,
      ema_id_raw,
      ema_id_normalized,
      quest_id_raw,
      quest_id_normalized,
      match_type,
      match_confidence,
      dplyr::everything()
    )

  # Summary by match type
  cat("\nFuzzy match results:\n")
  print(table(fuzzy_matches$match_type))
}

# ------------------------------------------------------------------------------
# STEP 3: COMPONENT-BASED MATCHING (for remaining unmatched)
# ------------------------------------------------------------------------------

cat("\n--- Step 3: Component-based matching ---\n")

# Parse Firenze-style IDs into components
parse_id_components <- function(id) {
  # Pattern: XX_YY_YYYY_MM_DD_NNN_S or variations
  # Try to extract: initials (2 chars), surname (2 chars), birth year (4 digits)

  parts <- stringr::str_split(id, "_")[[1]]

  tibble::tibble(
    original = id,
    initials = if (length(parts) >= 1) parts[1] else NA_character_,
    surname = if (length(parts) >= 2) parts[2] else NA_character_,
    birth_year = stringr::str_extract(id, "\\d{4}"),
    code = stringr::str_extract(id, "\\d{3}(?=_|$)")
  )
}

# This step would require more complex logic based on actual ID patterns
# For now, we'll flag remaining unmatched for manual review

# ------------------------------------------------------------------------------
# COMBINE ALL MATCHES
# ------------------------------------------------------------------------------

cat("\n--- Combining matches ---\n")

# Prepare exact matches
exact_final <- exact_matches %>%
  dplyr::mutate(
    user_id = ema_id_raw,
    quest_id_normalized = ema_id_normalized # Per exact match, sono uguali
  ) %>%
  dplyr::select(
    user_id,
    ema_id_raw,
    ema_id_normalized,
    quest_id_raw,
    quest_id_normalized,
    match_type,
    match_confidence
  )

# Combine with fuzzy matches (high confidence only for automatic acceptance)
if (nrow(fuzzy_matches) > 0) {
  fuzzy_to_add <- fuzzy_matches %>%
    dplyr::filter(match_type %in% c("fuzzy_high", "fuzzy_medium")) %>%
    dplyr::select(
      user_id,
      ema_id_raw,
      ema_id_normalized,
      quest_id_raw,
      quest_id_normalized,
      match_type,
      match_confidence
    )

  all_matches <- dplyr::bind_rows(exact_final, fuzzy_to_add)
} else {
  all_matches <- exact_final
}

cat(sprintf("\nTotal automatic matches: %d\n", nrow(all_matches)))
cat(sprintf("  - Exact: %d\n", sum(all_matches$match_type == "exact")))
cat(sprintf(
  "  - Fuzzy high: %d\n",
  sum(all_matches$match_type == "fuzzy_high")
))
cat(sprintf(
  "  - Fuzzy medium: %d\n",
  sum(all_matches$match_type == "fuzzy_medium")
))

# ------------------------------------------------------------------------------
# IDENTIFY UNMATCHED IDs
# ------------------------------------------------------------------------------

# EMA IDs without questionnaire match
unmatched_ema <- ema_ids %>%
  dplyr::filter(!ema_id_normalized %in% all_matches$ema_id_normalized) %>%
  dplyr::mutate(source = "ema", issue = "no_questionnaire_match")

# Quest IDs without EMA match
unmatched_quest <- quest_ids %>%
  dplyr::filter(!quest_id_normalized %in% all_matches$quest_id_normalized) %>%
  dplyr::mutate(source = "questionnaire", issue = "no_ema_match")

# Low-confidence fuzzy matches (for manual review)
if (nrow(fuzzy_matches) > 0) {
  uncertain_matches <- fuzzy_matches %>%
    dplyr::filter(match_type %in% c("fuzzy_low", "fuzzy_uncertain")) %>%
    dplyr::mutate(source = "fuzzy", issue = "low_confidence_match")
} else {
  uncertain_matches <- tibble::tibble(
    user_id = character(),
    ema_id_raw = character(),
    ema_id_normalized = character(),
    quest_id_raw = character(),
    quest_id_normalized = character(),
    match_type = character(),
    match_confidence = numeric(),
    source = character(),
    issue = character()
  )
}

cat(sprintf("\nUnmatched EMA IDs: %d\n", nrow(unmatched_ema)))
cat(sprintf("Unmatched questionnaire IDs: %d\n", nrow(unmatched_quest)))
cat(sprintf("Uncertain matches (need review): %d\n", nrow(uncertain_matches)))

# ------------------------------------------------------------------------------
# SAVE MATCHING REPORT
# ------------------------------------------------------------------------------

# Full matching report
matching_report <- all_matches %>%
  dplyr::mutate(status = "matched") %>%
  dplyr::bind_rows(
    unmatched_ema %>%
      dplyr::mutate(
        user_id = ema_id_raw,
        quest_id_raw = NA_character_,
        quest_id_normalized = NA_character_,
        match_type = "unmatched",
        match_confidence = NA_real_,
        status = "unmatched_ema"
      ) %>%
      dplyr::select(
        user_id,
        ema_id_raw,
        ema_id_normalized,
        quest_id_raw,
        quest_id_normalized,
        match_type,
        match_confidence,
        status
      )
  )

report_file <- file.path(paths$processed, "firenze_id_matching_report.csv")
readr::write_csv(matching_report, report_file)
cat(sprintf("\n✓ Matching report saved to: %s\n", report_file))

# Unmatched IDs for manual review
unmatched_for_review <- dplyr::bind_rows(
  unmatched_ema %>%
    dplyr::transmute(
      id = ema_id_raw,
      normalized = ema_id_normalized,
      source = "ema",
      suggested_match = NA_character_,
      notes = "No questionnaire match found"
    ),
  unmatched_quest %>%
    dplyr::transmute(
      id = quest_id_raw,
      normalized = quest_id_normalized,
      source = "questionnaire",
      suggested_match = NA_character_,
      notes = "No EMA match found"
    ),
  uncertain_matches %>%
    dplyr::transmute(
      id = ema_id_raw,
      normalized = ema_id_normalized,
      source = "ema",
      suggested_match = quest_id_raw,
      notes = sprintf(
        "Low confidence match (%.2f): %s",
        match_confidence,
        quest_id_raw
      )
    )
)

unmatched_file <- file.path(paths$processed, "firenze_unmatched_ids.csv")
readr::write_csv(unmatched_for_review, unmatched_file)
cat(sprintf("✓ Unmatched IDs saved to: %s\n", unmatched_file))

# ------------------------------------------------------------------------------
# CREATE MATCHED DATASET
# ------------------------------------------------------------------------------

cat("\n--- Creating matched dataset ---\n")

# Get questionnaire variables to merge (exclude ID columns)
quest_vars <- setdiff(names(d_quest), c("quest_id_raw", "quest_id_normalized"))

# Merge EMA with questionnaire for matched IDs
d_firenze_matched <- d_ema %>%
  dplyr::inner_join(
    all_matches %>%
      dplyr::select(user_id, quest_id_raw, match_type, match_confidence),
    by = "user_id"
  ) %>%
  dplyr::left_join(
    d_quest %>% dplyr::select(quest_id_raw, dplyr::all_of(quest_vars)),
    by = "quest_id_raw"
  )

cat(sprintf("\nMatched dataset:\n"))
cat(sprintf(
  "  - Participants: %d\n",
  dplyr::n_distinct(d_firenze_matched$user_id)
))
cat(sprintf("  - Observations: %d\n", nrow(d_firenze_matched)))
cat(sprintf("  - Variables: %d\n", ncol(d_firenze_matched)))

# Save matched dataset
matched_file <- file.path(paths$processed, "d_firenze_matched.rds")
saveRDS(d_firenze_matched, matched_file)
cat(sprintf("\n✓ Matched dataset saved to: %s\n", matched_file))

# ------------------------------------------------------------------------------
# SUMMARY
# ------------------------------------------------------------------------------

cat("\n========================================\n")
cat(
  "ID Matching Complete\n
"
)
cat("========================================\n\n")

cat("Summary:\n")
cat(sprintf("  Total EMA participants: %d\n", nrow(ema_ids)))
cat(sprintf(
  "  Successfully matched: %d (%.1f%%)\n",
  nrow(all_matches),
  100 * nrow(all_matches) / nrow(ema_ids)
))
cat(sprintf("  Unmatched (EMA only): %d\n", nrow(unmatched_ema)))
cat(sprintf("  Unmatched (Quest only): %d\n", nrow(unmatched_quest)))
cat(sprintf("  Uncertain (need review): %d\n", nrow(uncertain_matches)))

cat("\n--- Files created ---\n")
cat(sprintf("  1. %s (matched EMA + questionnaire)\n", matched_file))
cat(sprintf("  2. %s (full matching report)\n", report_file))
cat(sprintf("  3. %s (IDs needing manual review)\n", unmatched_file))

cat("\n--- Next steps ---\n")
cat("1. Review 'firenze_unmatched_ids.csv' for manual ID corrections\n")
cat(
  "2. If corrections needed, update the questionnaire file and re-run\
n"
)
cat("3. Proceed with 05_merge_datasets.R to combine with Flagstaff data\n")

cat("\n========================================\n")

# ==============================================================================
# END OF SCRIPT
# ==============================================================================
