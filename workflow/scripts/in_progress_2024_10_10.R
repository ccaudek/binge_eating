suppressPackageStartupMessages({
  library(dplyr)
  library(here)
  library(haven)
  library("readxl")
  library(lubridate)
  library(psych)
  library(ggplot2)
  library(sjPlot)
  library(dplyr)
  library(rio)
  library(here)
  library(purrr)
})


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

d_control <- readRDS(
  here::here(
    "data", "prep", "ema_data_controls_48subj.RDS"
  )
)

var_names <- names(d_control)

d1 <- d |> 
  dplyr::select(var_names)


# All individual EMA files are stored in this directory.
dir <- here::here("data", "raw", "firenze", "ema", "controls")

file_names <- list.files(path = dir, full.names = TRUE)
file_names <- as.character(list.files(path = dir))
n_files <- length(file_names)

d_list <- list()

for (index_file in 1:n_files) {
  
  d1 <-
    read_excel(
      here("data", "raw", "firenze", "ema", "controls", file_names[index_file])
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
  
  # Successive day per partecipant
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

both_df <- rbind(subset_natalie, subset_firenze)

both_df <- both_df |> 
  mutate(
    ssc = SC_1 + SC_3 + SC_5 + SC_2R + SC_4R + SC_6R,
    ssc_sc = SC_1 + SC_3 + SC_5,
    ssc_usc = SC_2 + SC_4 + SC_6,
    wbis = WBIS_1 + WBIS_2 + WBIS_3,
    affect = affect_Frustrat_Ang + affect_Sad_Depressed + affect_stress
    )

both_df$Did_you_eat <- factor(both_df$Did_you_eat)

# plot_model(fm, type = "pred", terms = c("affect", "group"))

# mod <- brm(
#   ssc ~ group * wbis + (1 + wbis | user_id/by_subj_day), 
#   family = student(),
#   backend = "cmdstanr",
#   data = both_df
# )
# conditional_effects(mod, "wbis:group")

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

both_df$stress <- both_df$PerceivedStress_1 + both_df$PerceivedStress_2

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

# Standardize variables
after_eating_df$affect <- as.vector(scale(after_eating_df$affect)) 
after_eating_df$bes <- as.vector(scale(after_eating_df$bes))
after_eating_df$ssc <- as.vector(scale(after_eating_df$ssc))
after_eating_df$wbis <- as.vector(scale(after_eating_df$wbis))

mod <- brm(
  affect ~ group * (bes + ssc) +
    (bes + ssc | user_id), 
  family = asym_laplace(),
  backend = "cmdstanr",
  iter = 3000,
  chains = 2,
  threads = threading(4),
  data = after_eating_df,
  silent = 2
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



