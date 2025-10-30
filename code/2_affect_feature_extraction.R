### PREPARATION ####

# Install and load required packages 

packages <- c( "dplyr", "data.table", "stringr", "ggplot2", "lubridate")
install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

# load data

affect_es <- readRDS("data/study1/affect_ema.RData")

### CONVERT AFFECT ANSWERS TO NUMERICS ####

# create new empty columns for numeric responses
affect_es$valence <- NA
affect_es$arousal <- NA

# convert affect responses to numerics
affect_es = affect_es  %>% 
  dplyr::mutate(answer_text = gsub("[^[:alnum:]]", "", answer_text)) %>% 
  dplyr::mutate(valence = ifelse(answer_text == "sehrangenehm", 6, valence),
         valence = ifelse(answer_text == "angenehm", 5, valence),
         valence = ifelse(answer_text == "eherangenehm", 4, valence), 
         valence = ifelse(answer_text == "eherunangenehm", 3, valence), 
         valence = ifelse(answer_text == "unangenehm", 2, valence), 
         valence = ifelse(answer_text == "sehrunangenehm", 1, valence)) %>% 
  dplyr::mutate(arousal = ifelse(answer_text == "sehraktiviert", 6, arousal),
         arousal = ifelse(answer_text == "aktiviert", 5, arousal),
         arousal = ifelse(answer_text == "eheraktiviert", 4, arousal), 
         arousal = ifelse(answer_text == "eherinaktiv", 3, arousal), 
         arousal = ifelse(answer_text == "inaktiv", 2, arousal), 
         arousal = ifelse(answer_text == "sehrinaktiv", 1, arousal)) 

affect_df_raw <- as.data.frame(matrix(0, ncol = 0, nrow = length(unique(affect_es$e_s_questionnaire_id)) )) # create new empty df
affect_df_raw$e_s_questionnaire_id <-  unique(affect_es$e_s_questionnaire_id) # add column with questionnaire id   

affect_df_raw <- merge(affect_df_raw, na.omit(affect_es[, c("e_s_questionnaire_id", "valence")]), by="e_s_questionnaire_id", all.x = T) # add valence column
affect_df_raw <- merge(affect_df_raw, na.omit(affect_es[, c("e_s_questionnaire_id", "arousal")]), by="e_s_questionnaire_id", all.x = T) # add arousal column

# beware: in some es instances only the valence or the arousal item has been answered!

## match w corresponding user ids

affect_df_raw_merged <- merge(affect_df_raw, affect_es[, c("e_s_questionnaire_id", "user_id", "questionnaireStartedTimestamp")], by ="e_s_questionnaire_id", all.x = T)

affect_df_raw_merged <- affect_df_raw_merged[!duplicated(affect_df_raw_merged$e_s_questionnaire_id),] # remove duplicates created by merge
                                        
# save 
saveRDS(affect_df_raw_merged, "data/study1/affect_df.RData")

# finish