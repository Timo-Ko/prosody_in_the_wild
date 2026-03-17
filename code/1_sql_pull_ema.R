### PREPARATION ####

# install and load required packages 
packages <- c( "RMariaDB", "DBI", "dbplyr", "lubridate", "tidyr")
#install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

# load functions 
source("code/functions/get_ema_beeps.R")

### READ IN EXPERIENCE SAMPLING DATA ####

# connect to phonestudy database

# Sensing database
phonestudy = dbConnect(
  drv = RMariaDB::MariaDB(),
  username = "rstudio",
  password = rstudioapi::askForPassword("Enter your password"),
  host = 'mc-ibt01.unisg.ch',
  port = 3306,
  dbname = "ssps")

# pull es_answer data
ps_esanswer = dbFetch(dbSendQuery(phonestudy , 'select * from ps_esanswer'))

str(ps_esanswer)

# pull es_questionnaire data (this is needed to match questionnaire id and user id)
ps_esquestionnaire = dbFetch(dbSendQuery(phonestudy , 'select * from ps_esquestionnaire'))

str(ps_esquestionnaire)

# ps_esanswer does not have a user id column and timestamp when the questionnaire was started yet - this need to be appended for the function to work
# append user id and questionnaireStartedTimestamp column to ps_esanswer dataframe

ps_esanswer_extended <- merge(ps_esanswer, ps_esquestionnaire[, c("id", "user_id", "questionnaireStartedTimestamp")],  by.x ="e_s_questionnaire_id", by.y = "id")

# reorder columns
ps_esanswer_extended <- ps_esanswer_extended %>% 
                dplyr::relocate(user_id)

# We do not need the ps_esquestionnaire table for further analyses of the RQ at hand

## create separate dfs for affect and acoustic data

# select only page id 1,2 for relevant ema items

affect_es = ps_esanswer_extended  %>% 
  dplyr::filter(page_id == 1 | page_id == 2) 

# ema_df = ps_esanswer_extended  %>% 
#   dplyr::filter(item_id == 61 | item_id == 62) 

# select only page id 20, 21, 22, and 23 because contains audio logging data

al_es = ps_esanswer_extended  %>% 
  dplyr::filter(page_id == 20 | page_id == 21 | page_id == 22 | page_id == 23) 

# audio = ps_esanswer_extended  %>% 
#   dplyr::filter(item_id == 31) 

### CHECK FOR GHOST EVENTS ####

# in these cases the question is empty or NA

table(affect_es$question != "" & !is.na(affect_es$question))
table(al_es$question != "" & !is.na(al_es$question))

# there are no ghost events!

### ACCOUNT FOR CASES WHERE PARTICIPANT HAD USED THE "BACK" BUTTON" ####

## filter cases where participants used the back button (use entry with latest timestamp)
affect_es_filtered <- affect_es %>% 
  dplyr::group_by(questionnaireStartedTimestamp) %>% 
  dplyr::arrange(desc(timestamp)) %>% 
  dplyr::distinct(page_id, .keep_all = T) %>% 
  dplyr::ungroup()

al_es_filtered <- al_es %>% 
  dplyr::group_by(questionnaireStartedTimestamp) %>% 
  dplyr::arrange(desc(timestamp)) %>% 
  dplyr::distinct(page_id, .keep_all = T) %>% 
  dplyr::ungroup()

## check for duplicated entries
affect_es_filtered %>%
  dplyr::select(user_id, e_s_questionnaire_id, page_id) %>%
  duplicated() %>%
  table()

al_es_filtered %>%
  dplyr::select(user_id, e_s_questionnaire_id, page_id) %>%
  duplicated() %>%
  table()

# save
saveRDS(affect_es_filtered, "data/affect_ema.rds")
saveRDS(al_es_filtered, "data/al_ema.rds")

### compute ema / audio logging compliance 

es_q <- getEmaBeeps(ps_esquestionnaire)   

## 1) Audio answers for EMAs --------------------------------------

# # Voice-logging items (pages 21/22/23)
# al_es <- ps_esanswer_extended %>%
#   filter(page_id %in% c(21, 22, 23))

# Define when a voice task counts as "completed", rows are EMA instances
audio_per_beep <- al_es %>%
  group_by(user_id, es_questionnaire_id = e_s_questionnaire_id) %>%
  summarise(
    n_audio_logs    = n(),
    audio_completed = n_audio_logs == 4, 
    .groups = "drop"
  )

table(audio_per_beep_evening$n_audio_logs)
table(audio_per_beep_evening$audio_completed)

# ## 2) Merge beeps + audio info ------------------------------------------
# 
# ema_voice <- es_q %>%
#   left_join(audio_per_beep,
#             by = c("user_id", "es_questionnaire_id")) %>%
#   mutate(
#     audio_completed = replace_na(audio_completed, FALSE)
#   )
#
## 3) Compliance rates --------------------------------------------------
#
# # (A) EMA Compliance among all evening beeps sent
# ema_compliance_all_evening <- ema_voice %>%
#   summarise(
#     n_evening_beeps = n(),
#     n_ema_completed    = sum(beep_answered),
#     compliance_all  = n_ema_completed / n_evening_beeps
#   )
# 
# ema_compliance_all_evening
#
# # (B) Compliance among all evening beeps sent
# voice_compliance <- ema_voice %>%
#   summarise(
#     n_evening_beeps = n(),
#     n_with_audio    = sum(audio_completed),
#     compliance_all  = n_with_audio / n_evening_beeps
#   )
# 
# voice_compliance
# 
# # (C) compliance among evening beeps where the EMA was answered
# voice_compliance_answered_evening <- ema_voice %>%
#   filter(beep_answered == 1) %>%
#   summarise(
#     n_evening_answered = n(),
#     n_with_audio       = sum(audio_completed),
#     compliance_ans     = n_with_audio / n_evening_answered
#   )
# 
# voice_compliance_answered_evening

# finish