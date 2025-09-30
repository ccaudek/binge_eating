# ------------------------------------------------
# Project binge-eating and self-compassion.
# Two groups: binge-eating and controls.
#
# Date: Fri Oct 11 07:14:16 2024
# ------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(here)
  library(haven)
  library("readxl")
  library(lubridate)
  library(psych)
  library(ggplot2)
  library(sjPlot)
  library(rio)
  library(purrr)
  library(brms)
  library(cmdstanr)
  library(missForest)
  library(missRanger)
  library(visdat)
  library(dlookr)  
})


# Import Natalie data -------------------------------

# Read Natalie's data
d <- read_sav(
  here::here("data", "raw", "flagstaff", "10.25.23_EMA_redcap.sav")
) |> 
  rename(
    "user_id" = "Participant_ID",
    "WBIS_2" = "WBI_2" 
  )

d <- d %>%
  mutate(by_subj_day = case_when(
    Notification_No >= 1 & Notification_No <= 5 ~ 1,
    Notification_No >= 6 & Notification_No <= 10 ~ 2,
    Notification_No >= 11 & Notification_No <= 15 ~ 3,
    Notification_No >= 16 & Notification_No <= 20 ~ 4,
    Notification_No >= 21 & Notification_No <= 25 ~ 5,
    Notification_No >= 26 & Notification_No <= 30 ~ 6,
    Notification_No >= 31 & Notification_No <= 35 ~ 7,
    TRUE ~ NA_integer_  # For values outside 1-35, if any
  ))

# Verify the result
# table(d$by_subj_day)


# Import control data -------------------------------

# All individual EMA files are stored in this directory.
dir <- here::here("data", "raw", "firenze", "ema", "controls")

file_names <- list.files(path = dir, full.names = TRUE)
file_names <- as.character(list.files(path = dir))
n_files <- length(file_names)

d_list <- list()

for (index_file in 1:n_files) {
  
  d1 <-
    read_excel(
      here("data", "raw", "firenze", "ema", "controls", file_names[index_file]),
      skip = 1  # This will skip the first row
    ) |> 
    dplyr::rename(
      "dat_time" = `Date and time`,
      "SC_1" = `scs_pos_1 (multipleChoice)`,
      "SC_2" = `scs_neg_2 (multipleChoice)`,
      "SC_3" = `scs_pos_3 (multipleChoice)`,
      "SC_4" = `scs_neg_4 (multipleChoice)`,
      "SC_5" = `scs_pos_5 (multipleChoice)`,
      "SC_6" = `scs_neg_6 (multipleChoice)`,
      "SC_7" = `scs_neg_7_new (multipleChoice)`,
      "SC_8" = `scs_pos_8_new (multipleChoice)`,
      "WBIS_1" = `weight_quest_1 (multipleChoice)`,
      "WBIS_2" = `weight_quest_2 (multipleChoice)`,
      "WBIS_3" = `weight_quest_3 (multipleChoice)`,
      "affect_Frustrat_Ang" = `emotion_quest_1 (multipleChoice)`,
      "affect_Sad_Depressed" = `emotion_quest_2 (multipleChoice)`,
      "affect_stress" = `emotion_quest_3 (multipleChoice)`,
      "Did_you_eat" = `yesno_eating (yesno)`,
      "PerceivedStress_1" = `quest_1 (multipleChoice)`,
      "PerceivedStress_2" = `quest_2 (multipleChoice)`,
      "BES_1" = `eating_1 (multipleChoice)`,
      "BES_2" = `eating_2 (multipleChoice)`,
      "BES_3" = `eating_3 (multipleChoice)`,
      "BES_4" = `eating_4 (multipleChoice)`,
      "BES_5" = `eating_5 (multipleChoice)`,
      "BES_6" = `eating_6 (multipleChoice)`
    )
  
  d1$`intro_phrase1 (basic)` <- NULL
  d1$`intro_phrase2 (basic)` <- NULL
  
  d1$user_id <- sub("\\.xlsx$", "", file_names[index_file])
  
  date_times <- parse_date_time(d1$dat_time, orders = "a, d b Y H:M:S")
  
  # Extract the date in ISO format
  d1$dates <- format(date_times, "%Y-%m-%d")
  
  # Extract the hour
  d1$hours <- format(date_times, "%H:%M:%S")
  
  # Successive day per participant
  d1$date_times <- parse_date_time(d1$dat_time, orders = "a, d b Y H:M:S")
  # Extract just the dates
  d1$dates <- as.Date(date_times)
  # Convert to factor and then to integer
  d1$by_subj_day <- as.integer(factor(d1$dates))
  
  # Check the maximum value of day_numbers
  max_day <- max(d1$by_subj_day)
  
  if (max_day == 8) {
    # Remove rows where day_number is 1
    d1 <- d1[d1$by_subj_day != 1, ]
    # Subtract 1 from all day_numbers
    d1$by_subj_day <- d1$by_subj_day - 1
  } else if (max_day == 9) {
    # Remove rows where day_number is 1 or 9
    d1 <- d1[!(d1$by_subj_day == 1 | d1$by_subj_day == 9), ]
    # Subtract 1 from all day_numbers
    d1$by_subj_day <- d1$by_subj_day - 1
  }
  # If the maximum is 7, do nothing
  
  # Define a function to convert modalities
  convert_modalities_SC <- function(x) {
    case_when(
      x == "Totalmente falso" ~ 1,
      x == "Un po' falso" ~ 2,
      x == "Né vero né falso" ~ 3,
      x == "Un po' vero" ~ 4,
      x == "Totalmente vero" ~ 5,
      TRUE ~ NA_integer_  # For unexpected values, if any
    )
  }
  
  d1 <- d1 %>%
    mutate(
      SC_1 = convert_modalities_SC(SC_1),
      SC_2 = convert_modalities_SC(SC_2),
      SC_3 = convert_modalities_SC(SC_3),
      SC_4 = convert_modalities_SC(SC_4),
      SC_5 = convert_modalities_SC(SC_5),
      SC_6 = convert_modalities_SC(SC_6),
      SC_7 = convert_modalities_SC(SC_7),
      SC_8 = convert_modalities_SC(SC_8)
    )
  
  d1$SC_2R <- abs(d1$SC_2 - 6)
  d1$SC_4R <- abs(d1$SC_4 - 6)
  d1$SC_6R <- abs(d1$SC_6 - 6)
  d1$SC_7R <- abs(d1$SC_7 - 6)
  
  convert_modalities_WBIS <- function(x) {
    case_when(
      x == "Fortemente in disaccordo" ~ 1,
      x == "Moderatamente in disaccordo" ~ 2,
      x == "Lievemente in disaccordo" ~ 3,
      x == "Né in disaccordo né in accordo" ~ 4,
      x == "Lievemente in accordo" ~ 5,
      x == "Moderatamente in accordo" ~ 6,
      x == "Fortemente in accordo" ~ 7,
      TRUE ~ NA_integer_  # For unexpected values, if any
    )
  }
  
  d1 <- d1 %>%
    mutate(
      WBIS_1 = convert_modalities_WBIS(WBIS_1),
      WBIS_2 = convert_modalities_WBIS(WBIS_2),
      WBIS_3 = convert_modalities_WBIS(WBIS_3)
    )
  
  convert_modalities_affect <- function(x) {
    case_when(
      x == "Per niente" ~ 1,
      x == "Un po'" ~ 2,
      x == "Abbastanza" ~ 3,
      x == "Estremamente" ~ 4,
      TRUE ~ NA_integer_  # For unexpected values, if any
    )
  }
  
  d1 <- d1 %>%
    mutate(
      affect_Frustrat_Ang = convert_modalities_affect(affect_Frustrat_Ang),
      affect_Sad_Depressed = convert_modalities_affect(affect_Sad_Depressed),
      affect_stress = convert_modalities_affect(affect_stress)
    )
  
  d1$Did_you_eat <- ifelse(d1$Did_you_eat == "yes", 1, 0)
  
  convert_modalities_BES <- function(x) {
    case_when(
      x == "Per niente" ~ 1,
      x == "Un poco" ~ 2,
      x == "Abbastanza" ~ 3,
      x == "Molto" ~ 4,
      x == "Estremamente" ~ 5,
      TRUE ~ NA_integer_  # For unexpected values, if any
    )
  }
  
  d1 <- d1 %>%
    mutate(
      BES_1 = convert_modalities_BES(BES_1),
      BES_2 = convert_modalities_BES(BES_2),
      BES_3 = convert_modalities_BES(BES_3),
      BES_4 = convert_modalities_BES(BES_4),
      BES_5 = convert_modalities_BES(BES_5),
      BES_6 = convert_modalities_BES(BES_6)
    )
  
  convert_modalities_PerceivedStress <- function(x) {
    case_when(
      x == "Per niente" ~ 1,
      x == "Un poco" ~ 2,
      x == "Abbastanza" ~ 3,
      x == "Estremamente" ~ 4,
      TRUE ~ NA_integer_  # For unexpected values, if any
    )
  }
  
  d1 <- d1 %>%
    mutate(
      PerceivedStress_1 = convert_modalities_PerceivedStress(PerceivedStress_1),
      PerceivedStress_2 = convert_modalities_PerceivedStress(PerceivedStress_2)
    )
  
  d1$dates <- NULL
  d1$hours <- NULL
  d1$date_times <- NULL
  d1$dat_time <- NULL
  
  d1$Responded <- 1
  
  d_list[[index_file]] <- d1
}

mydat <- do.call(rbind.data.frame, d_list)

# Save complete raw data
# here::here("data", "prep", "ema", "ema_data_1.RDS")
# saveRDS(mydat, snakemake@output[["rds"]])

temp <- colnames(mydat)

column_names <- setdiff(temp, c("SC_7", "SC_8", "SC_7R"))

subset_natalie <- d |> 
  dplyr::select(all_of(column_names))
subset_natalie$group <- "binge_eating"

subset_firenze <- mydat |> 
  dplyr::select(all_of(column_names))
subset_firenze$group <- "control"

subset_natalie$user_id <- as.character(subset_natalie$user_id)


# Combine both data sets -----------------------------

both_df <- rbind(subset_natalie, subset_firenze) |> 
  dplyr::select(-Responded)


# Data wrangling ------------------------------------

both_df <- both_df |> 
  mutate(
    ssc = SC_1 + SC_3 + SC_5 + SC_2R + SC_4R + SC_6R,
    ssc_sc = SC_1 + SC_3 + SC_5,
    ssc_usc = SC_2 + SC_4 + SC_6,
    wbis = WBIS_1 + WBIS_2 + WBIS_3,
    neg_aff = affect_Frustrat_Ang + affect_Sad_Depressed + affect_stress,
    bes = BES_1 + BES_2 + BES_3 + BES_4 + BES_5 + BES_6,
    stress = PerceivedStress_1 + PerceivedStress_2
    )

d <- both_df |> 
  dplyr::select(
    group, user_id, by_subj_day, Did_you_eat, ssc, ssc_sc, ssc_usc, wbis, 
    neg_aff, bes, stress
  ) |> 
  dplyr::rename(
    has_eaten = Did_you_eat
  ) |> 
  arrange(group, user_id, by_subj_day)

# BES was measured only is the notification was provided after a meal 
# (has_eaten == 1).

d$group <- factor(d$group)
d$user_id <- factor(d$user_id)
d$has_eaten <- factor(d$has_eaten)


# Missing data -----------------------------------------

vis_miss(d)

plot_na_pareto(d, only_na = TRUE)

summary(d)

# Impute d
d_imp <- missRanger(d, pmm.k = 5, num.trees = 100, verbose = 0)

vis_miss(d_imp)
glimpse(d_imp)


# Data summary -----------------------------------------

d_imp |> 
  group_by(group, has_eaten) |> 
  summarize(
    n = n(),
    avg_ssc = mean(ssc, na.rm=TRUE),
    avg_ssc_usc = mean(ssc_usc, na.rm=TRUE),
    avg_ssc_sc = mean(ssc_sc, na.rm=TRUE),
    avg_neg_aff = mean(neg_aff, na.rm=TRUE),
    avg_stress = mean(stress, na.rm=TRUE),
    avg_wbis = mean(wbis, na.rm=TRUE),
    avg_bes = mean(bes, na.rm=TRUE)
  ) |> 
  as.data.frame()




# Standardize variables
d_imp$neg_aff <- as.vector(scale(d_imp$neg_aff)) 
d_imp$bes <- as.vector(scale(d_imp$bes))
d_imp$ssc <- as.vector(scale(d_imp$ssc))
d_imp$wbis <- as.vector(scale(d_imp$wbis))

hist(d_imp$ssc)

m <- lmer(
  ssc_usc ~ ssc_sc * group + 
    (1 + ssc_sc | user_id/by_subj_day),
  data = d_imp
)
summary(m)


d_imp$ssc_usc <- as.vector(scale(d_imp$ssc_usc))
d_imp$ssc_sc <- as.vector(scale(d_imp$ssc_sc))

mod <- brm(
  ssc_usc ~ ssc_sc * group + 
    (1 + ssc_sc | user_id/by_subj_day),
  family = student(),
  backend = "cmdstanr",
  data = d_imp
)
pp_check(mod)

conditional_effects(mod, "ssc_sc:group")

summary(mod)
# Multilevel Hyperparameters:
#   ~user_id (Number of levels: 133) 
#                      Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)             0.79      0.05     0.70     0.90 1.01      282      638
# sd(ssc_sc)                0.32      0.03     0.27     0.37 1.00     1097     1638
# cor(Intercept,ssc_sc)     0.25      0.09     0.07     0.43 1.00      867     1571
# 
# ~user_id:by_subj_day (Number of levels: 901) 
#                      Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)             0.19      0.01     0.17     0.22 1.00      592     1510
# sd(ssc_sc)                0.18      0.02     0.14     0.21 1.01      557      903
# cor(Intercept,ssc_sc)     0.05      0.11    -0.16     0.26 1.02      470      922
# 
# Regression Coefficients:
#                     Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept               0.08      0.08    -0.08     0.24 1.03      199      328
# ssc_sc                 -0.08      0.04    -0.15    -0.00 1.01      514     1416
# groupcontrol           -0.33      0.15    -0.60    -0.04 1.03      196      429
# ssc_sc:groupcontrol    -0.20      0.07    -0.34    -0.08 1.01      659     1513
# 
# Further Distributional Parameters:
#       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sigma     0.28      0.01     0.26     0.30 1.00      918     1894
# nu        2.52      0.19     2.17     2.90 1.00     1351     2457
# 
# Draws were sampled using sample(hmc). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).

performance::r2_bayes(mod)
# Conditional R2: 0.793 (95% CI [0.786, 0.799])
# Marginal R2: 0.048 (95% CI [0.015, 0.094])




after_eating_df <- both_df |> 
  dplyr::filter(Did_you_eat == "1")

after_eating_df <- after_eating_df |> 
  mutate(
    bes = BES_1 + BES_2 + BES_3 + BES_4 + BES_5 + BES_6
  )

after_eating_df |> 
  group_by(group) |> 
  summarize(
    n = n(),
    avg_bes = mean(bes, na.rm = TRUE),
    stderr = sqrt(var(bes, na.rm = TRUE) / n),
    avg_ssc = mean(ssc, na.rm = TRUE),
    se_ssc = sqrt(var(ssc, na.rm = TRUE) / n)
  )



both_df |> 
  group_by(group, Did_you_eat) |> 
  summarize(
    n = n(),
    # avg_bes = mean(bes, na.rm = TRUE),
    # stderr = sqrt(var(bes, na.rm = TRUE) / n),
    avg_ssc = mean(ssc, na.rm = TRUE),
    se_ssc = sqrt(var(ssc, na.rm = TRUE) / n),
    avg_wbis = mean(wbis, na.rm = TRUE),
    avg_stress = mean(stress, na.rm = TRUE)
  )




# mod <- brm(
#   ssc ~ group * bes + (1 + bes | user_id/by_subj_day), 
#   family = student(),
#   backend = "cmdstanr",
#   data = both_df
# )

# fm <- lmer(affect ~ group * (bes + ssc) + wbis +
#              (bes + ssc + wbis | user_id), 
#            data = after_eating_df)
# summary(fm)
# 
# plot_model(fm, type = "pred", terms = c("bes", "group"))
# plot_model(fm, type = "pred", terms = c("ssc", "group"))
# plot_model(fm, type = "pred", terms = c("wbis"))

hist(after_eating_df$affect)



mod <- brm(
  affect ~ group * (bes + ssc) +
    (bes + ssc | user_id), 
  family = asym_laplace(),
  backend = "cmdstanr",
  iter = 3000,
  chains = 2,
  threads = threading(4),
  data = after_eating_df
  # silent = 2
)
pp_check(mod) 

me <- conditional_effects(mod, "bes:group")
g1 <- plot(me, plot = FALSE)[[1]] + 
  labs(x = "BES", y = "Negative Affect") 
g1

me <- conditional_effects(mod, "ssc:group")
g2 <- plot(me, plot = FALSE)[[1]] + 
  labs(x = "State Self-Compassion", y = "Negative Affect") 
g2

summary(mod)
#                  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept           -0.19      0.07    -0.32    -0.05 1.01      370      752
# groupcontrol        -0.53      0.16    -0.84    -0.22 1.00      695     1144
# bes                  0.25      0.04     0.17     0.33 1.00      654     1048
# ssc                 -0.21      0.04    -0.28    -0.14 1.00     1085     1571
# groupcontrol:bes    -0.33      0.11    -0.56    -0.11 1.00     1112     1609
# groupcontrol:ssc    -0.15      0.08    -0.31    -0.00 1.00     1055     1636

performance::r2_bayes(mod)

tapply(both_df$wbis, both_df$group, mean, na.rm=TRUE)

both_df <- both_df |> 
  mutate(
    bes = BES_1 + BES_2 + BES_3 + BES_4 + BES_5 + BES_6
  )

mod <- brm(
  wbis ~ group * Did_you_eat * (bes + ssc) +
    (Did_you_eat * (bes + ssc) | user_id), 
  family = asym_laplace(),
  backend = "cmdstanr",
  algorithm = "meanfield",
  data = both_df
  # silent = 2
)

eat0_df <- both_df |> 
  dplyr::filter(Did_you_eat == 0)

eat1_df <- both_df |> 
  dplyr::filter(Did_you_eat == 1)


hist(sqrt(eat1_df$wbis))

mod0 <- brm(
  scale(wbis) ~ group * scale(ssc) +
    (1 + scale(ssc) | user_id), 
  family = asym_laplace(),
  backend = "cmdstanr",
  iter = 1000,
  chains = 2,
  threads = threading(2),
  data = eat0_df
)
pp_check(mod0) 

summary(mod0)

conditional_effects(mod0, "ssc:group")


hist(both_df$affect)



mod1 <- brm(
  scale(wbis) ~ group * scale(ssc_sc) +
    (1 +  scale(ssc_sc) | user_id ), 
  family = asym_laplace(),
  backend = "cmdstanr",
  algorithm = "meanfield",
  # iter = 1000,
  # chains = 2,
  # threads = threading(2),
  data = eat1_df
)
pp_check(mod1)
summary(mod1)

conditional_effects(mod1, "ssc_sc:group")

