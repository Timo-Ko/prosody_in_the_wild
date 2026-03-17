# Install and load required packages 

packages <- c( "dplyr", "tidyr", "ggplot2", "stringr")
#install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

# read in benchmark results
bmr_ema <- readRDS("results/bmr_affect.rds")

# load data
affect_voice  <- readRDS("data/affect_voice.rds")

####  CONTENT SENTIMENT EFFECTS ON VOICE PREDICTION PERFORMANCE FOR AFFECT ####

bmr_results_folds <- readRDS("results/results/bmr_results_folds.rds")

# Fold-wise MAE table
mae_long <- bmr_results_folds %>%
  filter(
    learner_id == "regr.ranger",
    str_detect(task_id, "^egemaps_(valence|arousal)")
  ) %>%
  mutate(
    target = ifelse(str_detect(task_id, "arousal"), "Arousal", "Valence"),
    condition = case_when(
      str_detect(task_id, "_pos$") ~ "Positive",
      str_detect(task_id, "_neu$") ~ "Neutral",
      str_detect(task_id, "_neg$") ~ "Negative",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(condition)) %>%
  select(target, iteration, condition, regr.mae) %>%
  rename(mae = regr.mae)

# One omnibus p-value per target
friedman_res <- mae_long %>%
  group_by(target) %>%
  summarise(
    p_friedman = friedman.test(mae ~ condition | iteration, data = cur_data())$p.value,
    .groups = "drop"
  )

friedman_res

# finish