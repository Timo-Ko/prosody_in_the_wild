### PREPARATION ####

# Install and load required packages 

packages <- c( "dplyr", "data.table", "stringr", "ggplot2", "lubridate")
#install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

# load data

affect_es <- readRDS("data/affect_ema.rds")

### CONVERT EMA ANSWERS TO NUMERICS ####

# create new empty columns for numeric responses
affect_es$valence <- NA_real_
affect_es$arousal <- NA_real_
affect_es$stress  <- NA_real_

# convert affect responses to numerics
affect_es <- affect_es %>%
  # normalize answer text (remove punctuation/whitespace, lowercase)
  mutate(
    answer_text_clean = answer_text %>%
      tolower() %>%
      gsub("[^[:alnum:]]", "", .)
  ) %>%
  # valence
  mutate(
    valence = case_when(
      answer_text_clean == "sehrangenehm"     ~ 6,
      answer_text_clean == "angenehm"         ~ 5,
      answer_text_clean == "eherangenehm"     ~ 4,
      answer_text_clean == "eherunangenehm"   ~ 3,
      answer_text_clean == "unangenehm"       ~ 2,
      answer_text_clean == "sehrunangenehm"   ~ 1,
      TRUE                                   ~ valence
    ),
    # arousal
    arousal = case_when(
      answer_text_clean == "sehraktiviert" ~ 6,
      answer_text_clean == "aktiviert"     ~ 5,
      answer_text_clean == "eheraktiviert" ~ 4,
      answer_text_clean == "eherinaktiv"   ~ 3,
      answer_text_clean == "inaktiv"       ~ 2,
      answer_text_clean == "sehrinaktiv"   ~ 1,
      TRUE                                 ~ arousal
    ),
    # stress (0–5 as provided)
    stress = case_when(
      answer_text_clean == "sehrentspannt"   ~ 0,
      answer_text_clean == "entspannt"       ~ 1,
      answer_text_clean == "eherentspannt"   ~ 2,
      answer_text_clean == "ehergestresst"   ~ 3,
      answer_text_clean == "gestresst"       ~ 4,
      answer_text_clean == "sehrgestresst"   ~ 5,
      TRUE                                   ~ stress
    )
  )
# Build wide-ish EMA table (one row per e_s_questionnaire_id) with valence, arousal, stress

affect_df_raw <- data.frame(
  e_s_questionnaire_id = unique(affect_es$e_s_questionnaire_id)
)

# add valence / arousal / stress (some ES instances may have only a subset answered)
affect_df_raw <- merge(
  affect_df_raw,
  na.omit(affect_es[, c("e_s_questionnaire_id", "valence")]),
  by = "e_s_questionnaire_id",
  all.x = TRUE
)

affect_df_raw <- merge(
  affect_df_raw,
  na.omit(affect_es[, c("e_s_questionnaire_id", "arousal")]),
  by = "e_s_questionnaire_id",
  all.x = TRUE
)

affect_df_raw <- merge(
  affect_df_raw,
  na.omit(affect_es[, c("e_s_questionnaire_id", "stress")]),
  by = "e_s_questionnaire_id",
  all.x = TRUE
)

## match with corresponding user ids + timestamp
affect_df_raw_merged <- merge(
  affect_df_raw,
  affect_es[, c("e_s_questionnaire_id", "user_id", "questionnaireStartedTimestamp")],
  by = "e_s_questionnaire_id",
  all.x = TRUE
)

# remove duplicates created by merge (keep first per ES instance)
affect_df_raw_merged <- affect_df_raw_merged[!duplicated(affect_df_raw_merged$e_s_questionnaire_id), ]

# save
saveRDS(affect_df_raw_merged, "data/affect_df.rds")

# finish