### PREPARATION ####

# Install and load required packages 

packages <- c( "dplyr", "data.table", "stringr", "ggplot2", "lubridate", "jsonlite", "stringr")
install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

# load data

al_es <- readRDS("data/study1/al_ema.RData")

### DETERMINE SENTENCE CONDITION ####

# read in pos/ neg/ neutral sentences

pos_sentences <- c("Ich heirate die Person, die ich liebe",
                   "Ich habe im Lotto gewonnen",
                   "Meine Mannschaft hat gestern gewonnen",
                   "Ich habe ein tolles Geschenk bekommen",
                   "Ich habe eine Auszeichnung erhalten",
                   "Ich fühle mich heute großartig",
                   "Ich habe die Stelle bekommen",
                   "Ich genieße jeden Tag meines Lebens",
                   "Meine Kinder sehen so süß aus",
                   "Ich wurde soeben befördert",
                   "Seine Worte bringen mich zum Lächeln")

# collapse  
pos_sentences <- paste(pos_sentences, collapse = "|")


#neg_sentences <- c()

neutral_sentences <- c("Digitale Uhren sind weit verbreitet",
                       "Der Kunde kauft eine graue Hose",
                       "Der Teller steht auf dem runden Tisch",
                       "Unser Körper besteht großteils aus Wasser",
                       "Rohre bestehen aus Metall",
                       "Ich sehe einen Teppich auf dem Boden",
                       "Da sind Magnete am Kühlschrank",
                       "Meine Tasche liegt im Zimmer",
                       "Es stehen genügend Mülleimer im Raum",
                       "Mein Löffel liegt noch auf dem Tisch",
                       "Paul trägt einen langen Mantel")

# collapse  
neutral_sentences <- paste(neutral_sentences, collapse = "|")

# search questions if they match any of the sentences 

al_es$condition <- "negative"
al_es$condition <- base::ifelse(str_detect(al_es$question, pattern = neutral_sentences), "neutral", al_es$condition  ) 
al_es$condition <- base::ifelse(str_detect(al_es$question, pattern = pos_sentences), "positive", al_es$condition ) 

# number of audio logs across sentence conditions
table(al_es$condition)

# count number of audio loges per es instance
al_per_es <- al_es %>% group_by(e_s_questionnaire_id) %>% count()

table(al_per_es$n) # view counts

### EXTRACT AUDIO FEATURES ####

# parse first audio log instance to get names of acoustic features 

# remove the first and last square bracket

first_al <- str_sub(al_es$answer_text[1],3,-3)

# remove excess backslashes

first_al <- gsub(
  pattern = ('\\'), 
  replacement = '', 
  x = first_al,
  fixed = T
)

# parse

first_al_parsed <- first_al %>% 
  fromJSON(flatten = T) 

egemaps_names <- names(first_al_parsed$eGeMAPSv01a)

compare_names <- names(first_al_parsed$ComParE_2016)

# create empty dfs

# egemaps
egemaps_feature_df <- data.frame(matrix(ncol = (length(egemaps_names) + 2), nrow = nrow(al_es)))
colnames(egemaps_feature_df) <- c("id", "condition", egemaps_names)
egemaps_feature_df$id<- al_es$id
egemaps_feature_df$condition <- al_es$condition

# compare
compare_feature_df <- data.frame(matrix(ncol = (length(compare_names) + 2), nrow = nrow(al_es)))
colnames(compare_feature_df) <- c("id", "condition", compare_names)
compare_feature_df$id <- al_es$id
compare_feature_df$condition <- al_es$condition

for (id in egemaps_feature_df$id) {
    
# get cell with acoustic features

acoustics <- al_es[al_es$id == id, "answer_text"]
  
# remove the first and last square bracket
acoustics <- str_sub(acoustics,3,-3)

# remove excess backslashes
acoustics<- gsub(
  pattern = ('\\'), 
  replacement = '', 
  x = acoustics,
  fixed = T
)

# parse
acoustics_parsed <- acoustics%>% 
  fromJSON(flatten = T) 

egemaps_values <- as.data.frame(acoustics_parsed$eGeMAPSv01a) # save egemaps values

compare_values <- as.data.frame(acoustics_parsed$ComParE_2016) # save compare values

# if the participant did not make a record the df is empty
# replace those cases with NA

if (length(egemaps_values) == 0) {
  
  egemaps_values <- rep(NA, length(egemaps_names))
  compare_values <- rep(NA, length(compare_names))

}

# paste values in data frame 

# paste into df
egemaps_feature_df[egemaps_feature_df$id==id, c(3:(ncol(egemaps_feature_df)))] <- egemaps_values
compare_feature_df[compare_feature_df$id==id, c(3:(ncol(compare_feature_df)))] <- compare_values

rm(egemaps_values)
rm(compare_values)

};

# append questionnaire id and user id
egemaps_feature_df <- merge(egemaps_feature_df, al_es[,c("id", "e_s_questionnaire_id")], by = "id")
compare_feature_df <- merge(compare_feature_df, al_es[,c("id", "e_s_questionnaire_id")], by = "id")

# save feature sets
saveRDS(egemaps_feature_df, "data/study1/egemaps_feature_df.rds")
saveRDS(compare_feature_df, "data/study1/compare_feature_df.rds")

## match w corresponding user ids

egemaps_feature_df_all <- left_join(egemaps_feature_df , al_es[, c("questionnaireStartedTimestamp", "id", "user_id")], by ="id")
compare_feature_df_all <- left_join(compare_feature_df , al_es[, c("questionnaireStartedTimestamp", "id", "user_id")], by ="id")

# remove unneeded and reorder columns
egemaps_feature_df_all  <- egemaps_feature_df_all  %>% 
  dplyr::select(-c("name", "frameTime")) %>% 
  dplyr::select(c("id", "e_s_questionnaire_id", "user_id", "condition", "questionnaireStartedTimestamp"),everything())

compare_feature_df_all  <- compare_feature_df_all  %>% 
  dplyr::select(-c("name", "frameTime")) %>% 
  dplyr::select(c("id", "e_s_questionnaire_id", "user_id", "condition", "questionnaireStartedTimestamp"),everything())

# merge voice features 
voice_features <- merge(egemaps_feature_df_all, compare_feature_df_all, by = c("id", "e_s_questionnaire_id", "user_id", "condition", "questionnaireStartedTimestamp"))

# save
saveRDS(voice_features, "data/study1/voice_features_study1.rds")

# finish