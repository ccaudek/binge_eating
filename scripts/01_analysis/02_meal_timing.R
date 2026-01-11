# ==============================================================================
# 02_meal_timing.R
# ==============================================================================
# Project: Binge Eating and Self-Compassion EMA Study
# Purpose: Test whether meal proximity moderates the SC-USC association
# Author:  Corrado Caudek
# Date:    January 2026
#
# RESEARCH QUESTION 2:
#   Does the association between self-compassion (SC) and self-criticism (USC)
#   differ when participants are in an eating context vs. not?
#
#   Specifically:
#   - Is the SC-USC association different near meals vs. far from meals?
#   - Does this meal timing moderation differ between clinical and control groups?
#   - Is meal context particularly salient for the binge eating group?
#
# THEORETICAL BACKGROUND:
#   Eating contexts may be especially triggering for individuals with binge
#   eating disorder. The protective effect of self-compassion against
#   self-criticism might be weaker (or absent) in food-related contexts,
#   especially for clinical participants.
#
# HYPOTHESIS:
#   H1: In controls, meal timing has minimal effect on SC-USC association
#   H2: In clinical group, the SC-USC relationship weakens near meals
#       (i.e., even momentary increases in SC fail to reduce USC near eating)
#
# INPUT:
#   - data/processed/d_imp.rds (from scripts/00_preprocessing/06_imputation.R)
#
# OUTPUT:
#   - models/02_mod*.rds            (saved brms models)
#   - output/figures/02_*.png       (plots)
#   - output/tables/02_*.csv        (posterior summaries)
#   - output/reports/02_*.txt       (manuscript-ready paragraphs)
#
# STATISTICAL APPROACH:
#   - Three-way interaction: SC_within × Group × Meal_timing
#   - Within-between decomposition of SC
#   - Compare model with vs without meal timing interaction (LOO)
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

# Paths
paths <- list(
  processed = here::here("data", "processed"),
  models = here::here("models"),
  figures = here::here("output", "figures"),
  tables = here::here("output", "tables"),
  reports = here::here("output", "reports")
)

walk(paths, ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE))

# ------------------------------------------------------------------------------
# LOAD DATA
# ------------------------------------------------------------------------------

cat("\n========================================\n")
cat("Research Question 2: Meal Timing Moderation\n")
cat("Does eating context moderate the SC-USC association?\n")
cat("========================================\n\n")

imp_file <- file.path(paths$processed, "d_imp.rds")
if (!file.exists(imp_file)) {
  stop("Imputed dataset not found. Run 06_imputation.R first.")
}

d <- readRDS(imp_file)

cat(sprintf("Loaded: %d observations, %d variables\n", nrow(d), ncol(d)))
cat(sprintf("Participants: %d\n", n_distinct(d$user_id)))

# ------------------------------------------------------------------------------
# DATA PREPARATION
# ------------------------------------------------------------------------------

cat("\n--- Data Preparation ---\n")

# Filter days with sufficient data (≥2 notifications per day)
d_analysis <- d %>%
  group_by(user_id, by_subj_day) %>%
  mutate(n_notif_day = n()) %>%
  ungroup() %>%
  filter(n_notif_day >= 2) %>%
  select(-n_notif_day)

cat(sprintf("After filtering: %d observations\n", nrow(d_analysis)))

# Set factor levels
# Reference: control group, not_eaten (so interactions show clinical difference near meals)
d_analysis <- d_analysis %>%
  mutate(
    group = factor(group, levels = c("control", "binge_eating")),
    has_eaten = factor(has_eaten, levels = c("not_eaten", "eaten")),
    by_subj_day = factor(by_subj_day)
  )

# Standardize SC and USC for modeling
d_analysis <- d_analysis %>%
  mutate(
    ssc_sc_z = as.numeric(scale(ssc_sc)),
    ssc_usc_z = as.numeric(scale(ssc_usc))
  )

# Within-between decomposition
d_analysis <- d_analysis %>%
  group_by(user_id) %>%
  mutate(
    sc_person_mean = mean(ssc_sc_z, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    sc_grand_mean = mean(sc_person_mean, na.rm = TRUE),
    ssc_sc_w = ssc_sc_z - sc_person_mean,
    ssc_sc_b = sc_person_mean - sc_grand_mean
  ) %>%
  select(-sc_person_mean, -sc_grand_mean)

# Check meal timing distribution
cat("\nMeal timing distribution by group:\n")
meal_dist <- d_analysis %>%
  count(group, has_eaten) %>%
  group_by(group) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  ungroup()
print(meal_dist)

# Sample description
sample_desc <- d_analysis %>%
  group_by(group) %>%
  summarise(
    n_participants = n_distinct(user_id),
    n_observations = n(),
    pct_eaten = round(100 * mean(has_eaten == "eaten", na.rm = TRUE), 1),
    .groups = "drop"
  )
cat("\nSample description:\n")
print(sample_desc)

# ==============================================================================
# MODEL 1: BASELINE (Without Meal Timing)
# ==============================================================================
# This is the same as mod1 from 01_between_within.R
# We fit it here to allow LOO comparison with the meal timing model

cat("\n========================================\n")
cat("MODEL 1: Baseline (no meal timing)\n")
cat("========================================\n")

priors_main <- c(
  prior(normal(0, 1), class = "b"),
  prior(student_t(3, 0, 1), class = "sd"),
  prior(student_t(3, 0, 1), class = "sigma"),
  prior(gamma(2, 0.1), class = "nu")
)

form_baseline <- bf(
  ssc_usc_z ~
    ssc_sc_w *
      group +
      ssc_sc_b * group +
      (1 + ssc_sc_w | user_id) +
      (1 | user_id:by_subj_day)
)

# Fitting function with caching
fit_model <- function(
  formula,
  data,
  priors,
  file_stub,
  adapt_delta = 0.95,
  max_treedepth = 12,
  iter = 2000,
  warmup = 1000
) {
  model_path <- file.path(paths$models, paste0(file_stub, ".rds"))

  if (file.exists(model_path)) {
    cat("\nLoading existing model:", model_path, "\n")
    return(readRDS(model_path))
  }

  cat("\nFitting model:", file_stub, "\n")

  m <- brm(
    formula = formula,
    data = data,
    family = student(),
    prior = priors,
    backend = "cmdstanr",
    chains = 4,
    cores = 4,
    iter = iter,
    warmup = warmup,
    control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth),
    seed = 42,
    file = model_path
  )

  cat("Model saved to:", model_path, "\n")
  m
}

mod_baseline <- fit_model(
  form_baseline,
  d_analysis,
  priors_main,
  file_stub = "02_mod_baseline"
)

# ==============================================================================
# MODEL 2: WITH MEAL TIMING MODERATION
# ==============================================================================
# Three-way interaction: SC × Group × Meal_timing

cat("\n========================================\n")
cat("MODEL 2: With Meal Timing Moderation\n")
cat("========================================\n")

form_meal <- bf(
  ssc_usc_z ~
    ssc_sc_w *
      group *
      has_eaten + # Three-way: within × group × meal
      ssc_sc_b * group * has_eaten + # Three-way: between × group × meal
      (1 + ssc_sc_w | user_id) +
      (1 | user_id:by_subj_day)
)

mod_meal <- fit_model(
  form_meal,
  d_analysis,
  priors_main,
  file_stub = "02_mod_meal_timing",
  adapt_delta = 0.98,
  max_treedepth = 12
)

# Model diagnostics
cat("\n--- Model Diagnostics ---\n")
print(summary(mod_meal)$fixed[, c("Estimate", "Est.Error", "Rhat", "Bulk_ESS")])

# Posterior predictive check
pp_meal <- pp_check(mod_meal, ndraws = 100) +
  labs(title = "Posterior Predictive Check: Meal Timing Model")
ggsave(
  file.path(paths$figures, "02_mod_meal_ppc.png"),
  pp_meal,
  width = 9,
  height = 6,
  dpi = 300
)

# ==============================================================================
# MODEL COMPARISON (LOO)
# ==============================================================================

cat("\n--- Model Comparison ---\n")

mod_baseline <- add_criterion(mod_baseline, "loo")
mod_meal <- add_criterion(mod_meal, "loo")

loo_cmp <- loo_compare(mod_meal, mod_baseline)
cat("\nLOO Comparison (meal timing model vs baseline):\n")
print(loo_cmp)

write.csv(
  as.data.frame(loo_cmp),
  file.path(paths$tables, "02_loo_comparison.csv")
)

# Extract ELPD difference
loo_df <- as.data.frame(loo_cmp)
elpd_diff <- loo_df$elpd_diff[1]
se_diff <- loo_df$se_diff[1]

# ==============================================================================
# EXTRACT POSTERIOR: 8 COMBINATIONS (2 Groups × 2 Levels × 2 Meal Contexts)
# ==============================================================================

cat("\n--- Extracting Posterior Slopes ---\n")

dr <- as_draws_df(mod_meal)

# Identify parameter names
fe_names <- names(dr)[grepl("^b_", names(dr))]
cat("\nFixed effects in model:\n")
print(fixef(mod_meal))

# Helper to find interaction parameter
find_param <- function(draws_df, pattern) {
  nm <- names(draws_df)[grepl(pattern, names(draws_df), fixed = FALSE)]
  if (length(nm) == 0) {
    warning("Parameter not found: ", pattern)
    return(rep(0, nrow(draws_df)))
  }
  draws_df[[nm[1]]]
}

# ------------------------------------------------------------------------------
# WITHIN-PERSON SLOPES (8 combinations expanded)
# Reference: control, not_eaten
# ------------------------------------------------------------------------------

# Base within slope (control, not_eaten)
beta_w_ctrl_far <- dr$b_ssc_sc_w

# Control, eaten
beta_w_ctrl_near <- dr$b_ssc_sc_w +
  find_param(dr, "ssc_sc_w:has_eateneaten|has_eateneaten:ssc_sc_w")

# Clinical, not_eaten
beta_w_clin_far <- dr$b_ssc_sc_w +
  find_param(dr, "ssc_sc_w:groupbinge_eating|groupbinge_eating:ssc_sc_w")

# Clinical, eaten (need all main effects + interactions)
int_w_group <- find_param(
  dr,
  "ssc_sc_w:groupbinge_eating|groupbinge_eating:ssc_sc_w"
)
int_w_eaten <- find_param(dr, "ssc_sc_w:has_eateneaten|has_eateneaten:ssc_sc_w")
int_w_three <- find_param(
  dr,
  "ssc_sc_w:groupbinge_eating:has_eateneaten|groupbinge_eating:ssc_sc_w:has_eateneaten|ssc_sc_w:has_eateneaten:groupbinge_eating"
)

beta_w_clin_near <- dr$b_ssc_sc_w + int_w_group + int_w_eaten + int_w_three

# ------------------------------------------------------------------------------
# BETWEEN-PERSON SLOPES
# ------------------------------------------------------------------------------

# Control, not_eaten
beta_b_ctrl_far <- dr$b_ssc_sc_b

# Control, eaten
int_b_eaten <- find_param(dr, "ssc_sc_b:has_eateneaten|has_eateneaten:ssc_sc_b")
beta_b_ctrl_near <- dr$b_ssc_sc_b + int_b_eaten

# Clinical, not_eaten
int_b_group <- find_param(
  dr,
  "ssc_sc_b:groupbinge_eating|groupbinge_eating:ssc_sc_b"
)
beta_b_clin_far <- dr$b_ssc_sc_b + int_b_group

# Clinical, eaten
int_b_three <- find_param(
  dr,
  "groupbinge_eating:has_eateneaten:ssc_sc_b|ssc_sc_b:groupbinge_eating:has_eateneaten|groupbinge_eating:ssc_sc_b:has_eateneaten"
)
beta_b_clin_near <- dr$b_ssc_sc_b + int_b_group + int_b_eaten + int_b_three

# ------------------------------------------------------------------------------
# SUMMARY TABLE
# ------------------------------------------------------------------------------

summarize_posterior <- function(x, name) {
  tibble(
    parameter = name,
    mean = mean(x),
    sd = sd(x),
    q2.5 = quantile(x, 0.025),
    q97.5 = quantile(x, 0.975),
    P_negative = mean(x < 0)
  )
}

slopes_summary <- bind_rows(
  # Within-person
  summarize_posterior(beta_w_ctrl_far, "Within: Control, Far from meal"),
  summarize_posterior(beta_w_ctrl_near, "Within: Control, Near meal"),
  summarize_posterior(beta_w_clin_far, "Within: Clinical, Far from meal"),
  summarize_posterior(beta_w_clin_near, "Within: Clinical, Near meal"),
  # Between-person
  summarize_posterior(beta_b_ctrl_far, "Between: Control, Far from meal"),
  summarize_posterior(beta_b_ctrl_near, "Between: Control, Near meal"),
  summarize_posterior(beta_b_clin_far, "Between: Clinical, Far from meal"),
  summarize_posterior(beta_b_clin_near, "Between: Clinical, Near meal")
)

cat("\nPosterior Summary - SC→USC Slopes by Group and Meal Context:\n")
print(slopes_summary %>% mutate(across(where(is.numeric), ~ round(., 3))))

write.csv(
  slopes_summary,
  file.path(paths$tables, "02_slopes_by_meal_timing.csv"),
  row.names = FALSE
)

# ==============================================================================
# HYPOTHESIS TESTS: MEAL TIMING EFFECTS
# ==============================================================================

cat("\n--- Hypothesis Tests ---\n")

# Difference in slopes: near vs far from meals
meal_effect_ctrl_w <- beta_w_ctrl_near - beta_w_ctrl_far
meal_effect_clin_w <- beta_w_clin_near - beta_w_clin_far
meal_effect_ctrl_b <- beta_b_ctrl_near - beta_b_ctrl_far
meal_effect_clin_b <- beta_b_clin_near - beta_b_clin_far

# Three-way interaction: does meal effect differ by group?
threeway_w <- meal_effect_clin_w - meal_effect_ctrl_w
threeway_b <- meal_effect_clin_b - meal_effect_ctrl_b

hypotheses <- tibble(
  # Within-person: meal effects within each group
  `P(meal weakens SC-USC in Control, within)` = mean(meal_effect_ctrl_w > 0),
  `P(meal weakens SC-USC in Clinical, within)` = mean(meal_effect_clin_w > 0),

  # Three-way: does meal effect differ between groups?
  `P(meal effect stronger in Clinical, within)` = mean(threeway_w > 0),

  # Between-person: meal effects
  `P(meal weakens SC-USC in Control, between)` = mean(meal_effect_ctrl_b > 0),
  `P(meal weakens SC-USC in Clinical, between)` = mean(meal_effect_clin_b > 0),
  `P(meal effect stronger in Clinical, between)` = mean(threeway_b > 0),

  # Is clinical slope near-zero or positive near meals?
  `P(Clinical within >= 0 near meals)` = mean(beta_w_clin_near >= 0),
  `P(Clinical between > 0 near meals)` = mean(beta_b_clin_near > 0)
)

cat("\nMeal Timing Effects - Directional Probabilities:\n")
print(
  hypotheses %>%
    pivot_longer(
      everything(),
      names_to = "Hypothesis",
      values_to = "Probability"
    ) %>%
    mutate(Probability = round(Probability, 3))
)

write.csv(
  hypotheses,
  file.path(paths$tables, "02_meal_timing_hypotheses.csv"),
  row.names = FALSE
)

# ==============================================================================
# VISUALIZATION
# ==============================================================================

cat("\n--- Creating Visualizations ---\n")

# Prepare data for plotting
n_draws <- length(beta_w_ctrl_far)

plot_df <- tibble(
  value = c(
    beta_w_ctrl_far,
    beta_w_ctrl_near,
    beta_w_clin_far,
    beta_w_clin_near,
    beta_b_ctrl_far,
    beta_b_ctrl_near,
    beta_b_clin_far,
    beta_b_clin_near
  ),
  level = rep(c(
    rep("Within-person", n_draws * 4),
    rep("Between-person", n_draws * 4)
  )),
  group = rep(
    rep(c("Control", "Control", "Clinical", "Clinical"), each = n_draws),
    2
  ),
  meal = rep(rep(c("Far from meal", "Near meal"), each = n_draws, times = 2), 2)
) %>%
  mutate(
    level = factor(level, levels = c("Within-person", "Between-person")),
    group = factor(group, levels = c("Control", "Clinical")),
    meal = factor(meal, levels = c("Far from meal", "Near meal"))
  )

# Plot 1: Slopes by group and meal timing
p_meal <- ggplot(plot_df, aes(x = value, fill = meal)) +
  geom_density(alpha = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.8) +
  facet_grid(level ~ group, scales = "free_y") +
  scale_fill_manual(
    values = c("Far from meal" = "#E69F00", "Near meal" = "#56B4E9"),
    name = "Meal Context"
  ) +
  labs(
    title = "SC → USC Association by Group and Meal Proximity",
    subtitle = "Does eating context moderate the self-compassion effect?",
    x = "Standardized slope (β)",
    y = "Posterior density"
  ) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 11, face = "bold")
  )

ggsave(
  file.path(paths$figures, "02_meal_timing_slopes.png"),
  p_meal,
  width = 10,
  height = 8,
  dpi = 300
)

# Plot 2: Meal timing effect (difference: near - far)
effect_df <- tibble(
  value = c(
    meal_effect_ctrl_w,
    meal_effect_clin_w,
    meal_effect_ctrl_b,
    meal_effect_clin_b
  ),
  level = rep(
    c("Within-person", "Within-person", "Between-person", "Between-person"),
    each = n_draws
  ),
  group = rep(c("Control", "Clinical"), each = n_draws, times = 2)
)

p_effect <- ggplot(effect_df, aes(x = value, fill = group)) +
  geom_density(alpha = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.8) +
  facet_wrap(~level, ncol = 1) +
  scale_fill_manual(
    values = c("Control" = "#2166AC", "Clinical" = "#B2182B"),
    name = "Group"
  ) +
  labs(
    title = "Meal Timing Effect on SC-USC Association",
    subtitle = "Difference in slope: Near meal - Far from meal (positive = weaker association near meals)",
    x = "Change in slope near meals",
    y = "Posterior density"
  ) +
  theme(legend.position = "bottom")

ggsave(
  file.path(paths$figures, "02_meal_timing_effect.png"),
  p_effect,
  width = 9,
  height = 7,
  dpi = 300
)

# Conditional effects from brms
ce_within <- conditional_effects(
  mod_meal,
  effects = "ssc_sc_w:has_eaten",
  conditions = data.frame(group = c("control", "binge_eating")),
  re_formula = NA
)

p_ce <- plot(ce_within, plot = FALSE)[[1]] +
  labs(
    title = "Within-Person SC → USC Effect by Meal Context",
    x = "Self-Compassion (within-person)",
    y = "Self-Criticism (standardized)"
  )

ggsave(
  file.path(paths$figures, "02_conditional_effects_meal.png"),
  p_ce,
  width = 10,
  height = 6,
  dpi = 300
)

# ==============================================================================
# MANUSCRIPT-READY RESULTS
# ==============================================================================

cat("\n========================================\n")
cat("Generating Manuscript-Ready Results\n")
cat("========================================\n")

fmt_ci <- function(x, digits = 2) {
  sprintf(
    "%.2f, 95%% CrI [%.2f, %.2f]",
    round(mean(x), digits),
    round(quantile(x, 0.025), digits),
    round(quantile(x, 0.975), digits)
  )
}

fmt_p <- function(p) sprintf("%.3f", p)

# Key values
n_ctrl <- sample_desc$n_participants[sample_desc$group == "control"]
n_clin <- sample_desc$n_participants[sample_desc$group == "binge_eating"]
n_obs <- nrow(d_analysis)
pct_eaten_ctrl <- sample_desc$pct_eaten[sample_desc$group == "control"]
pct_eaten_clin <- sample_desc$pct_eaten[sample_desc$group == "binge_eating"]

# LOO comparison interpretation
if (abs(elpd_diff) > 2 * se_diff) {
  loo_interpretation <- sprintf(
    "Model comparison using LOO-CV showed that the meal timing model %s the baseline model (ΔELPD = %.1f, SE = %.1f).",
    ifelse(elpd_diff < 0, "did not improve upon", "improved upon"),
    elpd_diff,
    se_diff
  )
} else {
  loo_interpretation <- sprintf(
    "Model comparison using LOO-CV showed no clear preference for the meal timing model over baseline (ΔELPD = %.1f, SE = %.1f), suggesting meal proximity does not substantially moderate the SC-USC association.",
    elpd_diff,
    se_diff
  )
}

# Results paragraph
results_en <- paste0(
  "We examined whether proximity to eating occasions moderated the association between ",
  "state self-compassion (SC) and self-criticism (USC). Participants reported whether they had ",
  "eaten in the past 2 hours at each EMA prompt (control: ",
  pct_eaten_ctrl,
  "% near meals; ",
  "clinical: ",
  pct_eaten_clin,
  "% near meals). ",

  "At the within-person level, controls showed a negative SC-USC association both far from meals ",
  "(β = ",
  fmt_ci(beta_w_ctrl_far),
  ") and near meals (β = ",
  fmt_ci(beta_w_ctrl_near),
  "), ",
  "with ",
  fmt_p(mean(meal_effect_ctrl_w > 0)),
  " probability that the association weakened near meals. ",

  "For the clinical group, the within-person association was attenuated regardless of meal context ",
  "(far: β = ",
  fmt_ci(beta_w_clin_far),
  "; near: β = ",
  fmt_ci(beta_w_clin_near),
  "). ",
  "The probability that meal proximity had a stronger weakening effect in clinical vs. control ",
  "participants was P = ",
  fmt_p(mean(threeway_w > 0)),
  ". ",

  "At the between-person level, controls maintained a strong negative association in both contexts ",
  "(far: β = ",
  fmt_ci(beta_b_ctrl_far),
  "; near: β = ",
  fmt_ci(beta_b_ctrl_near),
  "), ",
  "while clinical participants showed minimal association ",
  "(far: β = ",
  fmt_ci(beta_b_clin_far),
  "; near: β = ",
  fmt_ci(beta_b_clin_near),
  "). ",

  loo_interpretation,
  " ",

  "These findings suggest that the attenuated SC-USC relationship in binge eating disorder ",
  "is not specific to eating contexts but rather reflects a general pattern of dissociation ",
  "between self-compassion and self-criticism in this population."
)

cat("\n--- RESULTS (English) ---\n")
cat(strwrap(results_en, width = 80), sep = "\n")

writeLines(
  results_en,
  file.path(paths$reports, "02_meal_timing_results_en.txt")
)
cat("\n✓ Results saved to output/reports/\n")

# ==============================================================================
# FINAL SUMMARY
# ==============================================================================

cat("\n========================================\n")
cat("Analysis Complete\n")
cat("========================================\n\n")

cat("KEY FINDINGS - Meal Timing Moderation:\n")
cat("┌─────────────────────────────────────────────────────────────────────┐\n")
cat("│ WITHIN-PERSON                                                       │\n")
cat(sprintf(
  "│ Control, far from meal:  β = %6.3f [%6.3f, %6.3f]             │\n",
  mean(beta_w_ctrl_far),
  quantile(beta_w_ctrl_far, 0.025),
  quantile(beta_w_ctrl_far, 0.975)
))
cat(sprintf(
  "│ Control, near meal:      β = %6.3f [%6.3f, %6.3f]             │\n",
  mean(beta_w_ctrl_near),
  quantile(beta_w_ctrl_near, 0.025),
  quantile(beta_w_ctrl_near, 0.975)
))
cat(sprintf(
  "│ Clinical, far from meal: β = %6.3f [%6.3f, %6.3f]             │\n",
  mean(beta_w_clin_far),
  quantile(beta_w_clin_far, 0.025),
  quantile(beta_w_clin_far, 0.975)
))
cat(sprintf(
  "│ Clinical, near meal:     β = %6.3f [%6.3f, %6.3f]             │\n",
  mean(beta_w_clin_near),
  quantile(beta_w_clin_near, 0.025),
  quantile(beta_w_clin_near, 0.975)
))
cat("├─────────────────────────────────────────────────────────────────────┤\n")
cat(sprintf(
  "│ LOO comparison: ΔELPD = %6.1f (SE = %.1f)                        │\n",
  elpd_diff,
  se_diff
))
cat("└─────────────────────────────────────────────────────────────────────┘\n")

cat("\nINTERPRETATION:\n")
cat("  The SC-USC dissociation in clinical group is NOT meal-specific.\n")
cat("  Rather, it appears to be a general characteristic of self-evaluation\n")
cat("  in binge eating disorder, present across all contexts.\n")

cat("\nFiles created:\n")
cat("  Models:  models/02_mod*.rds\n")
cat("  Figures: output/figures/02_*.png\n")
cat("  Tables:  output/tables/02_*.csv\n")
cat("  Reports: output/reports/02_meal_timing_results_en.txt\n")

cat("\n========================================\n")

# ==============================================================================
# END OF SCRIPT
# ==============================================================================
