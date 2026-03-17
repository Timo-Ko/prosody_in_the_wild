#### COMBINE DATA OF SMARTPHONECHANGERS ####

library(dplyr)
library(tidyr)
library(ggplot2)

voice_features_cleaned <- readRDS("data/voice_features_cleaned.rds") # load voice data

### DESCRIPTIVES OF FINAL DATA ####

# number of participants total 
length(unique(voice_features_cleaned$user_id))

# number of es total
length(unique(voice_features_cleaned$e_s_questionnaire_id))

# number of audio logs total
dim(voice_features_cleaned)

# audio logs per condition
table(voice_features_cleaned$condition)

# avg number of es and audio logs per participant 

summary_stats <- voice_features_cleaned %>%
  # Create a summary for each user
  group_by(user_id) %>%
  # Summarize the number of unique es and count of audio logs
  summarise(
    num_es = n_distinct(e_s_questionnaire_id),
    num_audio_logs = n()
  ) %>%
  # Ungroup to perform overall summary calculations
  ungroup() %>%
  # Calculate the overall summaries, including means and standard deviations
  summarise(
    num_participants = n(),  # Total number of participants
    num_es_total = sum(num_es),  # Total number of es
    num_audio_logs_total = sum(num_audio_logs),  # Total number of audio logs
    avg_es_per_participant = mean(num_es),  # Average number of es per participant
    sd_es_per_participant = sd(num_es),  # SD of es per participant
    avg_audio_logs_per_participant = mean(num_audio_logs),  # Average number of audio logs per participant
    sd_audio_logs_per_participant = sd(num_audio_logs)  # SD of audio logs per participant
  )


# append demographics

# load demographic data 
demographics_df <- readRDS("data/demographics_df.RData")

# merge data
voice_features_cleaned_demo <- voice_features_cleaned %>%
  dplyr::left_join(demographics_df, by=c("user_id" = "p_0001")) # join w demographics


# One row per participant in the FINAL sample (i.e., those present in voice_features_cleaned)
demo_final <- voice_features_cleaned_demo %>%
  group_by(user_id) %>%
  summarise(
    # pick first non-missing value per participant (change column names if needed)
    sex = dplyr::first(na.omit(Demo_GE1)),    
    age = dplyr::first(na.omit(Demo_A1)),    
    .groups = "drop"
  )

sex_comp <- demo_final %>%
  filter(!is.na(sex)) %>%                    # keep only non-missing
  mutate(
    sex = as.character(sex),
    sex = dplyr::recode(sex, `1` = "Male", `2` = "Female")  # adjust if your coding differs
  ) %>%
  count(sex, name = "n") %>%
  mutate(pct = 100 * n / sum(n))

sex_comp


age_stats <- demo_final %>%
  summarise(
    n_participants = n(),
    n_age_nonmiss = sum(!is.na(age)),
    age_mean = mean(age, na.rm = TRUE),
    age_sd   = sd(age, na.rm = TRUE),
    age_min  = min(age, na.rm = TRUE),
    age_max  = max(age, na.rm = TRUE)
  )

age_stats

#### FIGURE 2 : ACOUSTIC QUALITY + VARIABILITY (eGeMAPS) ####
# Creates 4 panels:
# (A) F0 mean  (B) F0 pctlrange0-2 (proxy for F0 range)  (C) loudness mean  (D) HNR mean

core_feats <- c(
  "VoicedSegmentsPerSec",
  "F0semitoneFrom27.5Hz_sma3nz_pctlrange0-2",
  "loudness_sma3_amean",
  "HNRdBACF_sma3nz_amean"
)

rename_map <- c(
  "A. Voiced segments (per sec)" = "VoicedSegmentsPerSec",
  "B. F0 variability (2nd-98th percentile range)" = "F0semitoneFrom27.5Hz_sma3nz_pctlrange0-2",
  "C. Loudness (mean)" = "loudness_sma3_amean",
  "D. HNR (dB; mean)" = "HNRdBACF_sma3nz_amean"
)

plot_df <- voice_features_cleaned %>%
  select(all_of(core_feats)) %>%
  rename(any_of(rename_map)) %>%   # safe with hyphens
  pivot_longer(cols = everything(), names_to = "feature", values_to = "value") %>%
  filter(is.finite(value))

winsorize <- function(x, p = 0.005) {
  lo <- stats::quantile(x, probs = p, na.rm = TRUE)
  hi <- stats::quantile(x, probs = 1 - p, na.rm = TRUE)
  pmax(pmin(x, hi), lo)
}

plot_df <- plot_df %>%
  group_by(feature) %>%
  mutate(value_w = winsorize(value, p = 0.005)) %>%
  ungroup()

fig2 <- ggplot(plot_df, aes(x = value_w)) +
  geom_density(adjust = 1.1, na.rm = TRUE) +
  facet_wrap(~ feature, scales = "free_x", ncol = 2) +
  labs(x = NULL, y = "Density") +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )

if (!dir.exists("figures")) dir.create("figures", recursive = TRUE)

ggsave("figures/Figure2_acoustic_quality_distributions.pdf",
       fig2, width = 7, height = 5, dpi = 600)

fig2

#### Variations with sentence conditions #####
packages <- c("dplyr","stringr","lme4","lmerTest","tidyr")
invisible(lapply(packages, library, character.only = TRUE))

dat <- voice_features_cleaned %>%
  mutate(
    user_id = factor(user_id),
    condition = factor(condition, levels = c("neutral","positive","negative"))
  )

metrics <- core_feats
metrics <- metrics[metrics %in% names(dat)]
if (length(metrics) == 0) stop("No metrics found in dat. Check core_feats vs colnames(dat).")

bt <- function(x) paste0("`", x, "`")
zcol <- function(x) as.numeric(scale(x))

# z-standardize outcomes at trial level
dat <- dat %>%
  mutate(across(all_of(metrics), zcol, .names = "z_{.col}"))
z_metrics <- paste0("z_", metrics)

run_mlm_trial_z <- function(yz, data) {
  f <- as.formula(paste0(bt(yz), " ~ condition + (1|user_id)"))
  m <- lmer(f, data = data, REML = TRUE)   # or REML = FALSE if you prefer
  
  fe <- as.data.frame(coef(summary(m)))
  fe$term <- rownames(fe); rownames(fe) <- NULL
  fe$metric <- yz
  
  # ICC_user
  vc <- as.data.frame(VarCorr(m))
  v_user  <- vc$vcov[vc$grp == "user_id"]
  v_resid <- vc$vcov[vc$grp == "Residual"]
  icc_user <- v_user / (v_user + v_resid)
  
  list(model = m, fixed = fe, icc_user = icc_user)
}

res_z <- lapply(z_metrics, run_mlm_trial_z, data = dat)
names(res_z) <- z_metrics

fixed_z <- bind_rows(lapply(res_z, `[[`, "fixed")) %>%
  filter(term %in% c("conditionpositive","conditionnegative")) %>%
  mutate(
    contrast = ifelse(term == "conditionpositive",
                      "Positive vs Neutral",
                      "Negative vs Neutral")
  )

icc_z <- tibble::tibble(
  metric = names(res_z),
  icc_user = sapply(res_z, function(x) x$icc_user)
)

results_table <- fixed_z %>%
  left_join(icc_z, by = "metric") %>%
  transmute(
    metric = sub("^z_", "", metric),
    contrast = contrast,
    beta_z = round(Estimate, 3),
    se = round(`Std. Error`, 3),
    df = if ("df" %in% names(.)) round(df, 1) else NA_real_,
    t = round(`t value`, 2),
    p = signif(`Pr(>|t|)`, 3),
    icc_user = round(icc_user, 3)
  ) %>%
  arrange(metric, contrast)

# correct p vals
results_table$p_fdr <- p.adjust(results_table$p, method = "BH")

results_table

write.csv(results_table, "results/table1_mlm.csv")

# ####  SUPPLEMENTS: DESCRIPTIVE DIFFERENCES IN VOICE FEATURES AMONG THREE SENTENCE CONDITIONS ####
# 
# egemaps_features <- colnames(voice_features_cleaned)[which(colnames(voice_features_cleaned) == "F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(voice_features_cleaned) == "equivalentSoundLevel_dBp")]
# 
# # Calculate the average values for each feature across the three conditions
# voice_by_condition <- voice_features_cleaned %>%
#   select(condition, all_of(egemaps_features)) %>%
#   group_by(condition) %>%
#   summarize(across(everything(), mean, na.rm = TRUE)) %>%
#   pivot_longer(cols = -condition, names_to = "feature", values_to = "value") %>%
#   pivot_wider(names_from = condition, values_from = value) %>%
#   mutate(
#     range_conditions = pmax(negative, neutral, positive, na.rm = TRUE) -
#       pmin(negative, neutral, positive, na.rm = TRUE)
#   )
# 
# 
# # save results 
# write.csv(voice_by_condition, "results/voice_by_condition.csv")
# 
# ## plot 
# 
# # --- Figure: Top 10 eGeMAPS features with largest between-condition differences (normalized) ---
# # Input: voice_by_condition (tibble/data.frame) with columns:
# # feature, negative, neutral, positive, range_conditions
# 
# library(dplyr)
# library(tidyr)
# library(ggplot2)
# library(stringr)
# 
# top_n <- 10
# 
# # 1) Select top-N features by raw between-condition range
# top_df <- voice_by_condition %>%
#   arrange(desc(range_conditions)) %>%
#   slice_head(n = top_n) %>%
#   mutate(
#     feature_label = feature %>%
#       str_replace("_sma3nz_", "_") %>%
#       str_replace("_sma3_", "_") %>%
#       str_replace("_amean", "_mean") %>%
#       str_replace("_percentile50\\.0", "_p50") %>%
#       str_replace("_stddevNorm", "_sdNorm")
#   )
# 
# # 2) Long format and normalize within feature (center + scale)
# plot_df <- top_df %>%
#   select(feature, feature_label, negative, neutral, positive, range_conditions) %>%
#   pivot_longer(cols = c(negative, neutral, positive),
#                names_to = "condition",
#                values_to = "value") %>%
#   mutate(
#     condition = factor(condition, levels = c("negative", "neutral", "positive"))
#   ) %>%
#   group_by(feature_label) %>%
#   mutate(
#     value_z = as.numeric(scale(value)) # z-score within feature across conditions
#   ) %>%
#   ungroup() %>%
#   mutate(
#     # order features by raw range (largest at top)
#     feature_label = factor(feature_label, levels = rev(top_df$feature_label))
#   )
# 
# # 3) Range in normalized units (for the line segment)
# range_df <- plot_df %>%
#   group_by(feature_label) %>%
#   summarise(
#     min_val = min(value_z, na.rm = TRUE),
#     max_val = max(value_z, na.rm = TRUE),
#     .groups = "drop"
#   )
# 
# # 4) Plot
# p <- ggplot() +
#   geom_vline(xintercept = 0, linewidth = 0.4) +
#   geom_segment(
#     data = range_df,
#     aes(x = min_val, xend = max_val, y = feature_label, yend = feature_label),
#     linewidth = 0.6,
#     lineend = "round"
#   ) +
#   geom_point(
#     data = plot_df,
#     aes(x = value_z, y = feature_label, shape = condition),
#     size = 2.2
#   ) +
#   scale_shape_manual(values = c(negative = 16, neutral = 15, positive = 17)) +
#   labs(
#     x = "Normalized feature value",
#     y = NULL,
#     shape = "Sentence condition"
#   ) +
#   theme_minimal(base_size = 11) +
#   theme(
#     panel.grid.major.y = element_blank(),
#     panel.grid.minor = element_blank(),
#     legend.position = "top"
#   )
# 
# # 5) Save
# ggsave("figures/fig_egemaps_top10_conditions_normalized.png", p, width = 7.0, height = 4.6, dpi = 300)
# 
# p




## FINISH