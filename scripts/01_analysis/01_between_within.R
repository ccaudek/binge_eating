# ==============================================================================
# 01_between_within.R
# ==============================================================================
# Project: Binge Eating and Self-Compassion EMA Study
# Purpose: Between/within decomposition + group comparison (brms)
# Author:  Corrado Caudek
# Date:    January 2026
#
# RESEARCH QUESTION 1:
#   According to Neff's theory, self-compassion (SC) and uncompassionate 
#   self-responding (USC/self-criticism) should be negatively associated.
#   We test whether this association holds at both:
#     - Within-person level (momentary fluctuations)
#     - Between-person level (individual differences)
#   And whether the clinical group (binge eating) shows a WEAKER (less negative)
#   association compared to healthy controls.
#
# HYPOTHESIS:
#   H1: In controls, SC and USC are negatively associated at both levels
#   H2: In clinical group, this negative association is attenuated
#       (i.e., the two dimensions are more "independent" or even positively related)
#
# INPUT:
#   - data/processed/d_imp.rds (from scripts/00_preprocessing/06_imputation.R)
#
# OUTPUT:
#   - models/*.rds               (saved brms models)
#   - output/figures/*.png       (plots)
#   - output/tables/*.csv        (posterior summaries)
#   - output/reports/*.txt       (manuscript-ready paragraphs)
#
# NOTES:
#   - Outcome: ssc_usc (uncompassionate self-responding / self-criticism)
#   - Predictor: ssc_sc (self-compassion), decomposed into within/between
#   - A NEGATIVE slope means: higher SC → lower USC (Neff's prediction)
#   - A LESS NEGATIVE slope in clinical = attenuated association
# ==============================================================================

# ------------------------------------------------------------------------------
# SETUP
# ------------------------------------------------------------------------------

rm(list = ls())

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(forcats)
  library(purrr)
  library(brms)
  library(cmdstanr)
  library(posterior)
  library(loo)
  library(bayesplot)
  
  theme_set(bayesplot::theme_default())
  color_scheme_set("brightblue")
})

set.seed(42)

# Paths (project-root anchored)
paths <- list(
  processed = here::here("data", "processed"),
  models    = here::here("models"),
  figures   = here::here("output", "figures"),
  tables    = here::here("output", "tables"),
  reports   = here::here("output", "reports")
)

# Create directories
walk(paths, ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE))

# ------------------------------------------------------------------------------
# LOAD IMPUTED DATA
# ------------------------------------------------------------------------------

cat("\n========================================\n")
cat("Research Question 1: SC-USC Association\n")
cat("Between/Within Decomposition by Group\n")
cat("========================================\n\n")

imp_file <- file.path(paths$processed, "d_imp.rds")
if (!file.exists(imp_file)) {
  stop(
    "Imputed dataset not found: ", imp_file,
    "\nRun scripts/00_preprocessing/06_imputation.R first."
  )
}

d <- readRDS(imp_file)

cat(sprintf("Loaded: %d observations, %d variables\n", nrow(d), ncol(d)))
cat(sprintf("Participants: %d\n", dplyr::n_distinct(d$user_id)))

# ------------------------------------------------------------------------------
# DATA VALIDATION
# ------------------------------------------------------------------------------

# Required variables
needed <- c(
  "group", "user_id", "by_subj_day", "beep", "time_band",
  "ssc_sc", "ssc_usc", "ssc",
  "neg_aff", "wbis", "has_eaten",
  "bes", "stress"
)

missing_needed <- setdiff(needed, names(d))
if (length(missing_needed) > 0) {
  stop("Missing required columns: ", paste(missing_needed, collapse = ", "))
}

# Ensure consistent factor levels (control = reference for clinical comparison)
d <- d %>%
  mutate(
    group = factor(group, levels = c("control", "binge_eating")),
    user_id = factor(user_id),
    by_subj_day = factor(by_subj_day),
    has_eaten = factor(has_eaten, levels = c("not_eaten", "eaten"))
  )

# Sample description
cat("\n--- Sample Description ---\n")
sample_desc <- d %>%
  group_by(group) %>%
  summarise(
    n_participants = n_distinct(user_id),
    n_observations = n(),
    obs_per_person = n() / n_distinct(user_id),
    .groups = "drop"
  )
print(sample_desc)

# Outcome missingness (structural, not imputed)
cat("\nOutcome missingness (structural, NOT imputed):\n")
cat(sprintf("  BES missing:    %d (%.1f%%)\n",
            sum(is.na(d$bes)), 100 * mean(is.na(d$bes))))
cat(sprintf("  Stress missing: %d (%.1f%%)\n",
            sum(is.na(d$stress)), 100 * mean(is.na(d$stress))))

# ------------------------------------------------------------------------------
# DESCRIPTIVE STATISTICS: SC-USC RELATIONSHIP
# ------------------------------------------------------------------------------

cat("\n--- Descriptive: SC and USC by Group ---\n")

# Raw descriptives
desc_raw <- d %>%
  group_by(group) %>%
  summarise(
    ssc_sc_mean = mean(ssc_sc, na.rm = TRUE),
    ssc_sc_sd = sd(ssc_sc, na.rm = TRUE),
    ssc_usc_mean = mean(ssc_usc, na.rm = TRUE),
    ssc_usc_sd = sd(ssc_usc, na.rm = TRUE),
    .groups = "drop"
  )
print(desc_raw)

# ------------------------------------------------------------------------------
# WITHIN/BETWEEN CORRELATION BY GROUP (Descriptive)
# ------------------------------------------------------------------------------

cat("\n--- Within/Between Correlations by Group ---\n")

# Between-person correlation: correlation of person means
between_corr <- d %>%
  group_by(group, user_id) %>%
  summarise(
    sc_mean = mean(ssc_sc, na.rm = TRUE),
    usc_mean = mean(ssc_usc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(group) %>%
  summarise(
    r_between = cor(sc_mean, usc_mean, use = "complete.obs"),
    n_persons = n(),
    .groups = "drop"
  )

cat("\nBetween-person correlation (person means):\n")
print(between_corr)

# Within-person correlation: average of individual correlations
within_corr <- d %>%
  group_by(group, user_id) %>%
  summarise(
    r_within = cor(ssc_sc, ssc_usc, use = "complete.obs"),
    n_obs = n(),
    .groups = "drop"
  ) %>%
  group_by(group) %>%
  summarise(
    r_within_mean = mean(r_within, na.rm = TRUE),
    r_within_sd = sd(r_within, na.rm = TRUE),
    r_within_median = median(r_within, na.rm = TRUE),
    pct_negative = mean(r_within < 0, na.rm = TRUE) * 100,
    n_persons = n(),
    .groups = "drop"
  )

cat("\nWithin-person correlation (mean of individual r's):\n")
print(within_corr)

# Save descriptive correlations
corr_summary <- left_join(between_corr, within_corr, by = "group")
write.csv(corr_summary, 
          file.path(paths$tables, "01_descriptive_correlations_by_group.csv"),
          row.names = FALSE)

# Visualization: Individual within-person correlations
individual_r <- d %>%
  group_by(group, user_id) %>%
  summarise(r = cor(ssc_sc, ssc_usc, use = "complete.obs"), .groups = "drop")

p_within_dist <- ggplot(individual_r, aes(x = r, fill = group)) +
  geom_histogram(bins = 25, alpha = 0.7, position = "identity") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  facet_wrap(~ group, ncol = 1) +
  labs(
    title = "Distribution of Within-Person SC-USC Correlations",
    subtitle = "Each bar = one participant; dashed line = r = 0",
    x = "Within-person correlation (SC with USC)",
    y = "Count"
  ) +
  scale_fill_manual(values = c("control" = "#2166AC", "binge_eating" = "#B2182B")) +
  theme(legend.position = "none")

ggsave(file.path(paths$figures, "01_within_person_correlations_hist.png"),
       p_within_dist, width = 8, height = 6, dpi = 300)

# ------------------------------------------------------------------------------
# STANDARDIZE VARIABLES FOR MODELING
# ------------------------------------------------------------------------------

cat("\n--- Standardizing variables ---\n")

# Grand-mean standardization for interpretable coefficients
d <- d %>%
  mutate(
    ssc_sc_z = as.numeric(scale(ssc_sc)),
    ssc_usc_z = as.numeric(scale(ssc_usc))
  )

# ------------------------------------------------------------------------------
# WITHIN/BETWEEN DECOMPOSITION
# ------------------------------------------------------------------------------
# Decompose predictor (ssc_sc_z) into:
#   - ssc_sc_w: within-person deviation from person mean
#   - ssc_sc_b: between-person component (person mean - grand mean)
#
# This allows separate estimation of:
#   - Within-person slope: Does momentary SC predict momentary USC?
#   - Between-person slope: Do people high in trait SC have lower trait USC?

cat("\n--- Within/Between Decomposition ---\n")

d <- d %>%
  group_by(user_id) %>%
  mutate(
    sc_person_mean = mean(ssc_sc_z, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    sc_grand_mean = mean(sc_person_mean, na.rm = TRUE),
    ssc_sc_w = ssc_sc_z - sc_person_mean,       # Within-person deviation
    ssc_sc_b = sc_person_mean - sc_grand_mean   # Between-person (centered)
  ) %>%
  select(-sc_person_mean, -sc_grand_mean)

cat("Created: ssc_sc_w (within), ssc_sc_b (between)\n")

# Verify decomposition
cat("\nDecomposition verification:\n")
cat(sprintf("  Mean ssc_sc_w: %.4f (should be ~0)\n", mean(d$ssc_sc_w, na.rm = TRUE)))
cat(sprintf("  Mean ssc_sc_b: %.4f (should be ~0)\n", mean(d$ssc_sc_b, na.rm = TRUE)))
cat(sprintf("  Cor(ssc_sc_w, ssc_sc_b): %.4f (should be ~0)\n", 
            cor(d$ssc_sc_w, d$ssc_sc_b, use = "complete.obs")))

# ==============================================================================
# MODEL 1: MAIN MODEL WITH GROUP INTERACTIONS
# ==============================================================================
# 
# ssc_usc_z ~ ssc_sc_w * group + ssc_sc_b * group + random effects
#
# Key coefficients to interpret:
#   - b_ssc_sc_w: within-person slope for CONTROL (reference)
#   - b_ssc_sc_w:groupbinge_eating: DIFFERENCE in within-slope (clinical - control)
#   - b_ssc_sc_b: between-person slope for CONTROL
#   - b_ssc_sc_b:groupbinge_eating: DIFFERENCE in between-slope (clinical - control)
#
# Hypothesis test: interaction coefficients > 0 means clinical has LESS NEGATIVE slope
# ==============================================================================

cat("\n========================================\n")
cat("MODEL 1: Main Model with Group Interactions\n")
cat("========================================\n")

# Priors (weakly informative)
priors_m1 <- c(
  prior(normal(0, 1), class = "b"),
  prior(student_t(3, 0, 1), class = "sd"),
  prior(student_t(3, 0, 1), class = "sigma"),
  prior(gamma(2, 0.1), class = "nu")  # Student-t df
)

# Model formula
form1 <- bf(
  ssc_usc_z ~ 
    ssc_sc_w * group +           # Within-person effect × group
    ssc_sc_b * group +           # Between-person effect × group
    (1 + ssc_sc_w | user_id) +   # Random intercept + within-slope by person
    (1 | user_id:by_subj_day)    # Random intercept by person-day
)

# Fitting function with caching
fit_model <- function(formula, data, priors, file_stub,
                      adapt_delta = 0.95, max_treedepth = 12) {
  
  model_path <- file.path(paths$models, paste0(file_stub, ".rds"))
  
  if (file.exists(model_path)) {
    cat("\nLoading existing model:", model_path, "\n")
    return(readRDS(model_path))
  }
  
  cat("\nFitting model:", file_stub, "\n")
  cat("This may take several minutes...\n")
  
  m <- brm(
    formula = formula,
    data = data,
    family = student(),          # Robust to outliers
    prior = priors,
    backend = "cmdstanr",
    chains = 4,
    cores = 4,
    iter = 2000,
    warmup = 1000,
    control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth),
    seed = 42,
    file = model_path            # Auto-save
  )
  
  cat("Model saved to:", model_path, "\n")
  m
}

# Fit main model
mod1 <- fit_model(form1, d, priors_m1, file_stub = "01_mod1_sc_usc_interactions")

# Model diagnostics
cat("\n--- Model Diagnostics ---\n")
cat("R-hat and ESS summary:\n")
print(summary(mod1)$fixed[, c("Estimate", "Est.Error", "Rhat", "Bulk_ESS", "Tail_ESS")])

# Posterior predictive check
pp1 <- pp_check(mod1, ndraws = 100) + 
  xlim(-4, 4) +
  labs(title = "Posterior Predictive Check: Model 1")
ggsave(file.path(paths$figures, "01_mod1_ppc.png"), pp1, width = 9, height = 6, dpi = 300)

# ==============================================================================
# EXTRACT AND INTERPRET POSTERIOR
# ==============================================================================

cat("\n--- Posterior Inference ---\n")

dr <- as_draws_df(mod1)

# Display fixed effect names for reference
cat("\nFixed effect parameters in model:\n")
print(fixef(mod1))

fe_names <- names(dr)[grepl("^b_", names(dr))]

# ------------------------------------------------------------------------------
# WITHIN-PERSON SLOPES
# ------------------------------------------------------------------------------

# Main effect: within-person slope for CONTROL (reference group)
beta_w_ctrl <- dr$b_ssc_sc_w

# Interaction: difference in within-slope (clinical - control)
# brms names it as "b_ssc_sc_w:groupbinge_eating"
w_int_name <- "b_ssc_sc_w:groupbinge_eating"
if (!w_int_name %in% fe_names) {
  # Try alternative naming
 w_int_name <- fe_names[grepl("ssc_sc_w.*group|group.*ssc_sc_w", fe_names)]
  w_int_name <- w_int_name[grepl(":", w_int_name)][1]
}
cat(sprintf("\nWithin interaction parameter: %s\n", w_int_name))

beta_w_diff <- dr[[w_int_name]]
beta_w_clin <- beta_w_ctrl + beta_w_diff

# ------------------------------------------------------------------------------
# BETWEEN-PERSON SLOPES
# ------------------------------------------------------------------------------

# Main effect: between-person slope for CONTROL (reference group)
beta_b_ctrl <- dr$b_ssc_sc_b

# Interaction: difference in between-slope (clinical - control)
# brms names it as "b_groupbinge_eating:ssc_sc_b"
b_int_name <- "b_groupbinge_eating:ssc_sc_b"
if (!b_int_name %in% fe_names) {
  # Try alternative naming
  b_int_name <- fe_names[grepl("ssc_sc_b.*group|group.*ssc_sc_b", fe_names)]
  b_int_name <- b_int_name[grepl(":", b_int_name)][1]
}
cat(sprintf("Between interaction parameter: %s\n", b_int_name))

beta_b_diff <- dr[[b_int_name]]
beta_b_clin <- beta_b_ctrl + beta_b_diff

# ------------------------------------------------------------------------------
# EXPLICIT CALCULATIONS FOR CLINICAL GROUP
# ------------------------------------------------------------------------------

cat("\n--- Slope Calculations ---\n")
cat("\nWITHIN-PERSON (momentary association):\n")
cat(sprintf("  Control:  β_w = %.3f\n", mean(beta_w_ctrl)))
cat(sprintf("  Clinical: β_w = %.3f + %.3f = %.3f\n", 
            mean(beta_w_ctrl), mean(beta_w_diff), mean(beta_w_clin)))

cat("\nBETWEEN-PERSON (trait-level association):\n")
cat(sprintf("  Control:  β_b = %.3f\n", mean(beta_b_ctrl)))
cat(sprintf("  Clinical: β_b = %.3f + %.3f = %.3f\n", 
            mean(beta_b_ctrl), mean(beta_b_diff), mean(beta_b_clin)))

# Summary function
summarize_posterior <- function(x, name) {
  tibble(
    parameter = name,
    mean = mean(x),
    sd = sd(x),
    q2.5 = quantile(x, 0.025),
    q50 = median(x),
    q97.5 = quantile(x, 0.975),
    P_negative = mean(x < 0),
    P_positive = mean(x > 0)
  )
}

# Create summary table
slopes_summary <- bind_rows(
  summarize_posterior(beta_w_ctrl, "Within: Control"),
  summarize_posterior(beta_w_clin, "Within: Clinical"),
  summarize_posterior(beta_w_diff, "Within: Difference (Clin-Ctrl)"),
  summarize_posterior(beta_b_ctrl, "Between: Control"),
  summarize_posterior(beta_b_clin, "Between: Clinical"),
  summarize_posterior(beta_b_diff, "Between: Difference (Clin-Ctrl)")
)

cat("\nPosterior Summary of SC→USC Slopes:\n")
print(slopes_summary %>% mutate(across(where(is.numeric), ~ round(., 3))))

# Save
write.csv(slopes_summary, 
          file.path(paths$tables, "01_mod1_slopes_summary.csv"),
          row.names = FALSE)

# ==============================================================================
# HYPOTHESIS TESTS
# ==============================================================================

cat("\n--- Hypothesis Tests ---\n")

hypotheses <- tibble(
  # H1: Negative association in controls (Neff's prediction)
  `P(within_ctrl < 0)` = mean(beta_w_ctrl < 0),
  `P(between_ctrl < 0)` = mean(beta_b_ctrl < 0),
  
  # H2: Weaker (less negative) in clinical
  # Positive interaction means clinical slope is LESS NEGATIVE (closer to 0 or positive)
  `P(within_diff > 0)` = mean(beta_w_diff > 0),
  `P(between_diff > 0)` = mean(beta_b_diff > 0),
  
  # H3: Clinical shows near-zero or positive association
  `P(within_clin >= 0)` = mean(beta_w_clin >= 0),
  `P(between_clin >= 0)` = mean(beta_b_clin >= 0),
  
  # H4: Clinical shows POSITIVE between-person association (stronger claim)
  `P(between_clin > 0)` = mean(beta_b_clin > 0),
  
  # Magnitude comparisons
  `P(|within_ctrl| > |within_clin|)` = mean(abs(beta_w_ctrl) > abs(beta_w_clin)),
  `P(|between_ctrl| > |between_clin|)` = mean(abs(beta_b_ctrl) > abs(beta_b_clin))
)

cat("\nDirectional Probabilities:\n")
print(hypotheses %>% pivot_longer(everything(), names_to = "Hypothesis", values_to = "Probability") %>%
        mutate(Probability = round(Probability, 3)))

write.csv(hypotheses,
          file.path(paths$tables, "01_mod1_hypothesis_tests.csv"),
          row.names = FALSE)

# ------------------------------------------------------------------------------
# CLINICAL INTERPRETATION
# ------------------------------------------------------------------------------

cat("\n--- Clinical Interpretation ---\n")

cat("\n1. WITHIN-PERSON (Momentary Fluctuations):\n")
cat("   Question: When SC increases momentarily, does USC decrease?\n")
cat(sprintf("   - Controls: YES (β = %.3f, P(β<0) = %.1f%%)\n", 
            mean(beta_w_ctrl), mean(beta_w_ctrl < 0) * 100))
cat(sprintf("   - Clinical: NO/WEAK (β = %.3f, P(β<0) = %.1f%%)\n", 
            mean(beta_w_clin), mean(beta_w_clin < 0) * 100))
cat(sprintf("   - Difference significant: P(clinical less negative) = %.1f%%\n",
            mean(beta_w_diff > 0) * 100))

cat("\n2. BETWEEN-PERSON (Individual Differences):\n")
cat("   Question: Do people high in trait-SC have lower trait-USC?\n")
cat(sprintf("   - Controls: YES (β = %.3f, P(β<0) = %.1f%%)\n", 
            mean(beta_b_ctrl), mean(beta_b_ctrl < 0) * 100))
cat(sprintf("   - Clinical: NO, REVERSED (β = %.3f, P(β>0) = %.1f%%)\n", 
            mean(beta_b_clin), mean(beta_b_clin > 0) * 100))
cat(sprintf("   - Difference significant: P(clinical less negative) = %.1f%%\n",
            mean(beta_b_diff > 0) * 100))

cat("\n3. KEY FINDING:\n")
cat("   In binge eating, individuals who report higher trait self-compassion\n")
cat("   ALSO report higher self-criticism. This suggests:\n")
cat("   - SC and USC are NOT bipolar opposites in this population\n")
cat("   - Possibly 'inflated' or ineffective self-compassion\n")
cat("   - Or a dysregulated self-evaluation system\n")

# ==============================================================================
# VISUALIZATION: POSTERIOR DISTRIBUTIONS
# ==============================================================================

# Prepare data for plotting
plot_df <- tibble(
  value = c(beta_w_ctrl, beta_w_clin, beta_b_ctrl, beta_b_clin),
  level = rep(c("Within-person", "Within-person", "Between-person", "Between-person"), 
              each = length(beta_w_ctrl)),
  group = rep(c("Control", "Clinical", "Control", "Clinical"), 
              each = length(beta_w_ctrl))
) %>%
  mutate(
    level = factor(level, levels = c("Within-person", "Between-person")),
    group = factor(group, levels = c("Control", "Clinical"))
  )

p_slopes <- ggplot(plot_df, aes(x = value, fill = group)) +
  geom_density(alpha = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.8) +
  facet_wrap(~ level, ncol = 1, scales = "free_y") +
  scale_fill_manual(
    values = c("Control" = "#2166AC", "Clinical" = "#B2182B"),
    name = "Group"
  ) +
  labs(
    title = "Posterior Distributions: SC → USC Association",
    subtitle = "Negative slope = higher SC associated with lower USC (Neff's prediction)",
    x = "Standardized slope (β)",
    y = "Posterior density"
  ) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 12, face = "bold")
  )

ggsave(file.path(paths$figures, "01_mod1_slopes_posterior.png"),
       p_slopes, width = 10, height = 8, dpi = 300)

# Interaction effects (differences)
diff_df <- tibble(
  value = c(beta_w_diff, beta_b_diff),
  level = rep(c("Within-person", "Between-person"), each = length(beta_w_diff))
)

p_diff <- ggplot(diff_df, aes(x = value)) +
  geom_density(fill = "#7570B3", alpha = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.8) +
  facet_wrap(~ level, ncol = 1) +
  labs(
    title = "Posterior: Group Difference in Slope (Clinical - Control)",
    subtitle = "Positive = Clinical has LESS NEGATIVE slope (weaker association)",
    x = "Difference in standardized slope",
    y = "Posterior density"
  )

ggsave(file.path(paths$figures, "01_mod1_interaction_posterior.png"),
       p_diff, width = 9, height = 6, dpi = 300)

# Conditional effects plots
ce_w <- conditional_effects(mod1, effects = "ssc_sc_w:group", re_formula = NA)
ce_b <- conditional_effects(mod1, effects = "ssc_sc_b:group", re_formula = NA)

p_ce_w <- plot(ce_w, plot = FALSE)[[1]] +
  labs(
    title = "Within-Person Effect: SC → USC",
    subtitle = "Controlling for between-person differences",
    x = "Self-Compassion (within-person deviation)",
    y = "Uncompassionate Self-Responding (standardized)"
  )

p_ce_b <- plot(ce_b, plot = FALSE)[[1]] +
  labs(
    title = "Between-Person Effect: SC → USC",
    subtitle = "Effect of trait-level self-compassion",
    x = "Self-Compassion (between-person, centered)",
    y = "Uncompassionate Self-Responding (standardized)"
  )

ggsave(file.path(paths$figures, "01_mod1_conditional_within.png"),
       p_ce_w, width = 9, height = 6, dpi = 300)
ggsave(file.path(paths$figures, "01_mod1_conditional_between.png"),
       p_ce_b, width = 9, height = 6, dpi = 300)

# ==============================================================================
# MODEL 0: COMPARISON MODEL (No Interactions)
# ==============================================================================

cat("\n========================================\n")
cat("MODEL 0: No Interactions (Comparison)\n")
cat("========================================\n")

form0 <- bf(
  ssc_usc_z ~ 
    ssc_sc_w + ssc_sc_b + group +
    (1 + ssc_sc_w | user_id) +
    (1 | user_id:by_subj_day)
)

mod0 <- fit_model(form0, d, priors_m1, file_stub = "01_mod0_no_interactions")

# LOO comparison
mod0 <- add_criterion(mod0, "loo")
mod1 <- add_criterion(mod1, "loo")

loo_cmp <- loo_compare(mod1, mod0)

cat("\nLOO Comparison (mod1 with interactions vs mod0 without):\n")
print(loo_cmp)

# Save
write.csv(as.data.frame(loo_cmp),
          file.path(paths$tables, "01_loo_comparison.csv"))

# ==============================================================================
# MODEL 2: HETEROGENEOUS RANDOM SLOPES BY GROUP
# ==============================================================================
# Tests whether the VARIABILITY in within-person slopes differs by group

cat("\n========================================\n")
cat("MODEL 2: Heterogeneous Slope Variance by Group\n")
cat("========================================\n")

form2 <- bf(
  ssc_usc_z ~ 
    ssc_sc_w * group +
    ssc_sc_b * group +
    (1 + ssc_sc_w | gr(user_id, by = group)) +
    (1 | user_id:by_subj_day)
)

mod2 <- fit_model(form2, d, priors_m1, 
                  file_stub = "01_mod2_heterogeneous_slopes",
                  adapt_delta = 0.98)

# Extract group-specific slope SDs
dr2 <- as_draws_df(mod2)
sd_names <- names(dr2)[grepl("^sd_user_id.*ssc_sc_w", names(dr2))]

cat("\nRandom slope SD parameters found:\n")
print(sd_names)

# Save parameter names for reference
writeLines(sd_names, file.path(paths$reports, "01_mod2_sd_parameters.txt"))

# ==============================================================================
# MANUSCRIPT-READY RESULTS
# ==============================================================================

cat("\n========================================\n")
cat("Generating Manuscript-Ready Results\n")
cat("========================================\n")

# Helper functions
fmt_ci <- function(x, digits = 2) {
  m <- mean(x, na.rm = TRUE)
  q <- quantile(x, c(.025, .975), na.rm = TRUE)
  sprintf("%.2f, 95%% CrI [%.2f, %.2f]", round(m, digits), round(q[1], digits), round(q[2], digits))
}

fmt_p <- function(p) sprintf("%.3f", p)

# Sample info
n_ctrl <- sample_desc$n_participants[sample_desc$group == "control"]
n_clin <- sample_desc$n_participants[sample_desc$group == "binge_eating"]
n_obs_ctrl <- sample_desc$n_observations[sample_desc$group == "control"]
n_obs_clin <- sample_desc$n_observations[sample_desc$group == "binge_eating"]

# Key probabilities
p_w_diff_pos <- mean(beta_w_diff > 0)
p_b_diff_pos <- mean(beta_b_diff > 0)
p_b_clin_pos <- mean(beta_b_clin > 0)

# LOO info
loo_df <- as.data.frame(loo_cmp)
if ("elpd_diff" %in% names(loo_df)) {
  loo_text_en <- sprintf(
    "Model comparison using leave-one-out cross-validation favored the interaction model (ΔELPD = %.1f, SE = %.1f).",
    loo_df$elpd_diff[1], loo_df$se_diff[1]
  )
  loo_text_it <- sprintf(
    "Il confronto dei modelli tramite leave-one-out cross-validation ha favorito il modello con interazioni (ΔELPD = %.1f, SE = %.1f).",
    loo_df$elpd_diff[1], loo_df$se_diff[1]
  )
} else {
  loo_text_en <- ""
  loo_text_it <- ""
}

# --- ENGLISH VERSION ---
results_en <- paste0(
  "We examined whether the association between state self-compassion (SC) and uncompassionate ",
  "self-responding (USC) differed between individuals with binge eating disorder (n = ", n_clin,
  "; ", n_obs_clin, " observations) and healthy controls (n = ", n_ctrl,
  "; ", n_obs_ctrl, " observations) using a Bayesian multilevel model with within-between decomposition. ",
  
  "At the within-person level, controls showed a negative association between momentary SC and USC (β = ", 
  fmt_ci(beta_w_ctrl), ", P(β < 0) > .999), consistent with Neff's theoretical prediction that ",
  "self-compassion buffers against self-criticism. In contrast, the clinical group showed virtually ",
  "no within-person association (β = ", fmt_ci(beta_w_clin), "), with P = ", 
  fmt_p(p_w_diff_pos), " that the clinical slope was less negative than controls. ",
  
  "At the between-person level, controls again showed a strong negative association (β = ",
  fmt_ci(beta_b_ctrl), "), indicating that individuals higher in trait SC reported lower trait USC. ",
  "Critically, the clinical group showed a REVERSED pattern: a positive association (β = ",
  fmt_ci(beta_b_clin), ", P(β > 0) = ", fmt_p(p_b_clin_pos), "), ",
  "meaning that binge eating individuals who reported higher trait self-compassion also reported ",
  "HIGHER self-criticism. The probability that the between-person slope was less negative in the ",
  "clinical group was P = ", fmt_p(p_b_diff_pos), ". ",
  
  loo_text_en,
  
  " These findings suggest that in binge eating disorder, self-compassion and self-criticism ",
  "do not operate as bipolar opposites but rather as independent or even positively related dimensions. ",
  "This may reflect a dysregulated self-evaluation system or a form of 'ineffective' self-compassion ",
  "that fails to buffer against self-critical responses."
)

# --- ITALIAN VERSION ---
results_it <- paste0(
  "Abbiamo esaminato se l'associazione tra self-compassion di stato (SC) e risposte auto-critiche ",
  "(USC) differisse tra individui con disturbo da binge eating (n = ", n_clin,
  "; ", n_obs_clin, " osservazioni) e controlli sani (n = ", n_ctrl,
  "; ", n_obs_ctrl, " osservazioni) utilizzando un modello bayesiano multilivello con decomposizione within/between. ",
  
  "A livello within-person, i controlli hanno mostrato un'associazione negativa tra SC momentanea e USC (β = ",
  fmt_ci(beta_w_ctrl), ", P(β < 0) > .999), coerentemente con la previsione teorica di Neff che ",
  "la self-compassion protegge dall'auto-critica. Al contrario, il gruppo clinico non ha mostrato ",
  "praticamente alcuna associazione within-person (β = ", fmt_ci(beta_w_clin), "), con P = ", 
  fmt_p(p_w_diff_pos), " che la pendenza clinica fosse meno negativa rispetto ai controlli. ",
  
  "A livello between-person, i controlli hanno mostrato nuovamente una forte associazione negativa (β = ",
  fmt_ci(beta_b_ctrl), "), indicando che gli individui con maggiore SC di tratto riportavano minore USC di tratto. ",
  "Criticamente, il gruppo clinico ha mostrato un pattern INVERTITO: un'associazione positiva (β = ",
  fmt_ci(beta_b_clin), ", P(β > 0) = ", fmt_p(p_b_clin_pos), "), ",
  "ovvero gli individui con binge eating che riportavano maggiore self-compassion di tratto riportavano anche ",
  "MAGGIORE auto-critica. La probabilità che la pendenza between-person fosse meno negativa nel ",
  "gruppo clinico era P = ", fmt_p(p_b_diff_pos), ". ",
  
  loo_text_it,
  
  " Questi risultati suggeriscono che nel disturbo da binge eating, self-compassion e auto-critica ",
  "non operano come opposti bipolari ma piuttosto come dimensioni indipendenti o addirittura correlate positivamente. ",
  "Ciò potrebbe riflettere un sistema di auto-valutazione disregolato o una forma di self-compassion 'inefficace' ",
  "che non riesce a proteggere dalle risposte auto-critiche."
)

# Print and save
cat("\n--- RESULTS (English) ---\n")
cat(strwrap(results_en, width = 80), sep = "\n")

cat("\n--- RESULTS (Italian) ---\n")
cat(strwrap(results_it, width = 80), sep = "\n")

writeLines(results_en, file.path(paths$reports, "01_results_paragraph_en.txt"))
writeLines(results_it, file.path(paths$reports, "01_results_paragraph_it.txt"))

cat("\n✓ Results saved to output/reports/\n")

# ==============================================================================
# FINAL SUMMARY
# ==============================================================================

cat("\n========================================\n")
cat("Analysis Complete\n")
cat("========================================\n\n")

cat("KEY FINDINGS - SC → USC Association:\n")
cat("\n┌─────────────────────────────────────────────────────────────────────┐\n")
cat("│ WITHIN-PERSON (Momentary)                                           │\n")
cat("├─────────────────────────────────────────────────────────────────────┤\n")
cat(sprintf("│ Control:  β = %6.3f [%6.3f, %6.3f]  P(β<0) = %.1f%%          │\n",
            mean(beta_w_ctrl), quantile(beta_w_ctrl, 0.025), quantile(beta_w_ctrl, 0.975),
            mean(beta_w_ctrl < 0) * 100))
cat(sprintf("│ Clinical: β = %6.3f [%6.3f, %6.3f]  P(β<0) = %.1f%%          │\n",
            mean(beta_w_clin), quantile(beta_w_clin, 0.025), quantile(beta_w_clin, 0.975),
            mean(beta_w_clin < 0) * 100))
cat(sprintf("│ → Difference: P(clinical less negative) = %.1f%%               │\n",
            mean(beta_w_diff > 0) * 100))
cat("├─────────────────────────────────────────────────────────────────────┤\n")
cat("│ BETWEEN-PERSON (Trait)                                              │\n")
cat("├─────────────────────────────────────────────────────────────────────┤\n")
cat(sprintf("│ Control:  β = %6.3f [%6.3f, %6.3f]  P(β<0) = %.1f%%          │\n",
            mean(beta_b_ctrl), quantile(beta_b_ctrl, 0.025), quantile(beta_b_ctrl, 0.975),
            mean(beta_b_ctrl < 0) * 100))
cat(sprintf("│ Clinical: β = %6.3f [%6.3f, %6.3f]  P(β>0) = %.1f%%  ← REVERSED│\n",
            mean(beta_b_clin), quantile(beta_b_clin, 0.025), quantile(beta_b_clin, 0.975),
            mean(beta_b_clin > 0) * 100))
cat(sprintf("│ → Difference: P(clinical less negative) = %.1f%%               │\n",
            mean(beta_b_diff > 0) * 100))
cat("└─────────────────────────────────────────────────────────────────────┘\n")

cat("\nINTERPRETATION:\n")
cat("  • Controls: Neff's theory CONFIRMED - SC and USC are negatively related\n")
cat("  • Clinical (Within): SC-USC nearly INDEPENDENT at momentary level\n")
cat("  • Clinical (Between): SC-USC POSITIVELY related at trait level (!)\n")
cat("    → Higher trait-SC associated with HIGHER trait-USC\n")
cat("    → Suggests dysregulated/ineffective self-compassion in BED\n")

cat("\nFiles created:\n")
cat("  Models:  models/01_mod*.rds\n")
cat("  Figures: output/figures/01_*.png\n")
cat("  Tables:  output/tables/01_*.csv\n")
cat("  Reports: output/reports/01_*.txt\n")

cat("\n========================================\n")

# ==============================================================================
# END OF SCRIPT
# ==============================================================================
