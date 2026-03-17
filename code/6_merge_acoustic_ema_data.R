### PREPARATION ####

# load data 

voice_features <- readRDS("data/voice_features_cleaned.rds")

affect_df <- readRDS("data/affect_df.rds")

# remove unneeded cols
affect_df$n_valence_unique <- NULL
affect_df$n_arousal_unique <- NULL

# load demographic data 
demographics_df <- readRDS("data/demographics_df.RData")

### MERGE AFFECT WITH AUDIO FEATURES ####

# merge data
affect_voice <- voice_features %>%
  dplyr::filter(if_any(voicingFinalUnclipped_sma_amean:last_col(), ~ !is.na(.))) %>% # remove rows where voice features are NA (probably logging error)
  dplyr::inner_join(affect_df, by=c("e_s_questionnaire_id" = "e_s_questionnaire_id", "user_id" = "user_id", "questionnaireStartedTimestamp" = "questionnaireStartedTimestamp")) %>%  # join w voice features
  dplyr::left_join(demographics_df, by=c("user_id" = "p_0001")) # join w demographics

# reorder columns
affect_voice <- affect_voice  %>% 
  dplyr::select(c("e_s_questionnaire_id", "questionnaireStartedTimestamp", "id", "user_id" , "Demo_A1", "Demo_GE1", "condition", "valence", "arousal"),everything())

# save dfs 
saveRDS(affect_voice, "data/affect_voice.rds")

### DESCRIPTIVES OF MERGED DATA ####

# number of participants
length(unique(affect_voice$user_id))

# number of es
length(unique(affect_voice$e_s_questionnaire_id))

# number of audio logs
dim(affect_voice)

## FINISH