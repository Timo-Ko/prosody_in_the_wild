# Install and load required packages 

packages <- c( "dplyr", "tidyr", "ggplot2")
#install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

# read in benchmark results
bmr_ema <- readRDS("results/bmr_ema.rds")

# load data
affect_voice <- readRDS("data/study1/affect_voice_cleaned.rds")


####  CONTENT SENTIMENT EFFECTS ON VOICE PREDICTION PERFORMANCE ####

library(dplyr)
library(stringr)
library(tidyr)

bmr_results_folds <- readRDS("results/bmr_results_folds.rds")

# extract fold-level SRHO for RF
srho_df <- bmr_results_folds %>%
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
      TRUE ~ "All"
    )
  ) %>%
  select(
    target, condition, iteration, regr.srho
  ) %>%
  pivot_wider(
    names_from = condition,
    values_from = regr.srho
  )


# wilcox test

conditions <- c("Positive", "Neutral", "Negative")

for (tgt in c("Valence", "Arousal")) {
  cat("\nTarget:", tgt, "\n")
  for (cond in conditions) {
    res <- wilcox.test(
      srho_df[[cond]][srho_df$target == tgt],
      srho_df$All[srho_df$target == tgt],
      paired = TRUE
    )
    cat(cond, ": p =", round(res$p.value, 3), "\n")
  }
}

# finish