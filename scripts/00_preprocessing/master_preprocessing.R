# ==============================================================================
# master_preprocessing.R
# ==============================================================================
# Project: Binge Eating and Self-Compassion EMA Study
# Purpose: Master script to execute the full preprocessing pipeline
# Author:  Corrado Caudek
# Date:    Sat Jan 10 08:36:00 2026

# 1. Prima i dati EMA
source("scripts/00_preprocessing/01_read_flagstaff.R")
source("scripts/00_preprocessing/02_read_firenze_ema.R")

# 2. Se servono i questionari (opzionale)
source("scripts/00_preprocessing/03_read_firenze_quest.R")
source("scripts/00_preprocessing/04_match_firenze_ids.R")

# 3. Merge e imputation
source("scripts/00_preprocessing/05_merge_datasets.R")
source("scripts/00_preprocessing/06_imputation.R")
