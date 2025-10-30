### PREPARATION ####

# load data 

voice_features <- readRDS("data/study1/voice_features_study1.rds")

affect_df <- readRDS("data/study1/affect_df.RData")

affect_df$user_id <- as.integer(affect_df$user_id)

# load demographic data 
demographics_df <- readRDS("data/study1/demographics_df.RData")

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
saveRDS(affect_voice, "data/study1/affect_voice_study1.rds")

### DESCRIPTIVES OF MERGED DATA ####

# number of participants
length(unique(affect_voice$user_id))

# number of es
length(unique(affect_voice$e_s_questionnaire_id))

# number of audio logs
dim(affect_voice)

## FINISH