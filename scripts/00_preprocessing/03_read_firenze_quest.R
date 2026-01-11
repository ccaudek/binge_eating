# ==============================================================================
# 03_read_firenze_quest.R
# ==============================================================================
# Project: Binge Eating and Self-Compassion EMA Study
# Purpose: Read and clean questionnaire data from Florence (control group)
# Author:  Corrado Caudek
# Date:    Sat Jan 10 08:37:37 2026
#
# INPUT:
#   - data/raw/firenze/quest/EMA_SCS_NAT23 (Risposte).xlsx
#   - Contains baseline questionnaire responses for Florence participants
#
# OUTPUT:
#   - data/processed/d_firenze_quest.rds
#   - data/dictionaries/data_dictionary_firenze_quest.csv
#
# NOTES:
#   - Questionnaire data is at the PERSON level (one row per participant)
#   - Contains demographic info and baseline measures
#   - ID matching with EMA data may require fuzzy matching (see 04_match_firenze_ids.R)
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
# READ QUESTIONNAIRE DATA
# ------------------------------------------------------------------------------

cat("\n========================================\n")
cat("Reading Florence Questionnaire Data\n")
cat("========================================\n\n")

# Path to questionnaire file
quest_file <- file.path(
  paths$raw_firenze_quest,
  "EMA_SCS_NAT23 (Risposte).xlsx"
)

# Check file exists
if (!file.exists(quest_file)) {
  stop(sprintf("Questionnaire file not found: %s", quest_file))
}

cat(sprintf("Reading: %s\n\n", quest_file))

# Read Excel file
# First, let's see what sheets are available
sheets <- readxl::excel_sheets(quest_file)
cat(sprintf("Available sheets: %s\n", paste(sheets, collapse = ", ")))

# Read the first (or main) sheet
d_quest_raw <- readxl::read_excel(quest_file, sheet = 1)

cat(sprintf(
  "Raw data: %d rows, %d columns\n",
  nrow(d_quest_raw),
  ncol(d_quest_raw)
))

# ------------------------------------------------------------------------------
# EXPLORE COLUMN NAMES
# ------------------------------------------------------------------------------

cat("\n--- Column names in questionnaire file ---\n")
col_names <- names(d_quest_raw)
print(col_names)

# ------------------------------------------------------------------------------
# IDENTIFY ID COLUMN
# ------------------------------------------------------------------------------

# The ID column might have various names - try to find it
potential_id_cols <- c(
  "Inserisci il suo codice anonimo (esempio: ma_ro_1997_05_04_174_m)",
  "ID",
  "id",
  "Id",
  "Codice",
  "codice",
  "CODICE",
  "Participant",
  "participant",
  "Partecipante",
  "partecipante",
  "Nome",
  "nome",
  "Codice partecipante",
  "Codice Partecipante",
  "codice_partecipante"
)

# Find which potential ID column exists
id_col <- NULL
for (col in potential_id_cols) {
  if (col %in% col_names) {
    id_col <- col
    break
  }
}

# If not found, check first column or columns containing "id" or "codice"
if (is.null(id_col)) {
  # Try to find columns containing relevant keywords
  id_candidates <- col_names[grepl(
    "id|codice|partecip|nome",
    col_names,
    ignore.case = TRUE
  )]

  if (length(id_candidates) > 0) {
    id_col <- id_candidates[1]
    cat(sprintf("\nFound potential ID column: '%s'\n", id_col))
  } else {
    # Use first column as fallback
    id_col <- col_names[1]
    cat(sprintf("\nUsing first column as ID: '%s'\n", id_col))
  }
}

cat(sprintf("\nUsing ID column: '%s'\n", id_col))

# ------------------------------------------------------------------------------
# CLEAN AND STANDARDIZE
# ------------------------------------------------------------------------------

cat("\nCleaning and standardizing...\n")

d_quest <- d_quest_raw %>%
  # Rename ID column to standard name
  dplyr::rename(quest_id_raw = !!rlang::sym(id_col)) %>%
  # Create normalized ID for matching
  dplyr::mutate(
    quest_id_raw = as.character(quest_id_raw),
    quest_id_normalized = normalize_id(quest_id_raw)
  ) %>%
  # Remove completely empty rows
  dplyr::filter(!is.na(quest_id_raw) & quest_id_raw != "")

cat(sprintf("After cleaning: %d participants\n", nrow(d_quest)))

# ------------------------------------------------------------------------------
# IDENTIFY AND RENAME KEY VARIABLES
# ------------------------------------------------------------------------------

# Try to identify common questionnaire variables by pattern matching
# This is flexible to handle different column naming conventions

# Function to find column by pattern
find_col <- function(df, patterns) {
  col_names <- names(df)
  for (pattern in patterns) {
    matches <- grep(pattern, col_names, ignore.case = TRUE, value = TRUE)
    if (length(matches) > 0) return(matches[1])
  }
  return(NULL)
}

# Try to identify key variables
potential_vars <- list(
  # Demographics
  age = c("età", "eta", "age", "anni"),
  gender = c("sesso", "genere", "gender", "sex"),
  education = c("istruzione", "education", "titolo", "scuola"),

  # BMI related
  weight = c("peso", "weight", "kg"),
  height = c("altezza", "height", "cm", "statura"),
  bmi = c("bmi", "imc"),

  # Self-Compassion Scale (full)
  scs_total = c("scs.*total", "self.compassion.*total", "scs_tot"),

  # Other scales that might be present
  bdi = c("bdi", "beck.*depress"),
  bai = c("bai", "beck.*anx"),
  edeq = c("ede.?q", "eating.*disorder.*exam")
)

# Create rename map for found variables
found_vars <- list()
for (var_name in names(potential_vars)) {
  col <- find_col(d_quest, potential_vars[[var_name]])
  if (!is.null(col)) {
    found_vars[[var_name]] <- col
    cat(sprintf("  Found '%s' → '%s'\n", var_name, col))
  }
}

# ------------------------------------------------------------------------------
# DISPLAY SAMPLE DATA
# ------------------------------------------------------------------------------

cat("\n--- Sample of ID values ---\n")
print(head(d_quest %>% dplyr::select(quest_id_raw, quest_id_normalized), 20))

# Check for potential issues
cat("\n--- ID diagnostics ---\n")
cat(sprintf("Total IDs: %d\n", nrow(d_quest)))
cat(sprintf("Unique raw IDs: %d\n", dplyr::n_distinct(d_quest$quest_id_raw)))
cat(sprintf(
  "Unique normalized IDs: %d\n",
  dplyr::n_distinct(d_quest$quest_id_normalized)
))

# Check for duplicates
dup_ids <- d_quest %>%
  dplyr::count(quest_id_normalized) %>%
  dplyr::filter(n > 1)

if (nrow(dup_ids) > 0) {
  cat(sprintf("\n⚠ Found %d duplicate normalized IDs:\n", nrow(dup_ids)))
  print(dup_ids)
}

# ------------------------------------------------------------------------------
# SAVE PROCESSED DATA
# ------------------------------------------------------------------------------

output_file <- file.path(paths$processed, "d_firenze_quest.rds")

saveRDS(d_quest, output_file)

cat(sprintf("\n✓ Saved processed questionnaire data to: %s\n", output_file))
cat(sprintf("  - %d participants\n", nrow(d_quest)))
cat(sprintf("  - %d variables\n", ncol(d_quest)))

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
    example_values = paste(head(unique(na.omit(x)), 3), collapse = "; ")
  )
}

data_dictionary <- purrr::map2_dfr(
  d_quest,
  names(d_quest),
  summarize_column
)

# Save dictionary
dict_file <- file.path(paths$dictionaries, "data_dictionary_firenze_quest.csv")
readr::write_csv(data_dictionary, dict_file)

cat(sprintf("✓ Saved data dictionary to: %s\n", dict_file))

# ------------------------------------------------------------------------------
# SUMMARY
# ------------------------------------------------------------------------------

cat("\n========================================\n")
cat("Florence Questionnaire Processing Complete\n")
cat("========================================\n\n")

cat("Variables in questionnaire:\n")
print(names(d_quest))

cat("\n--- Next steps ---\n")
cat("1. Review the data dictionary to understand available variables\n")
cat("2. Run 04_match_firenze_ids.R to match questionnaire IDs with EMA IDs\n")
cat("3. Some manual ID corrections may be needed for fuzzy matches\n")

cat("\n========================================\n")

# ==============================================================================
# END OF SCRIPT
# ==============================================================================
