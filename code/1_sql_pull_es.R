### PREPARATION ####

# install and load required packages 
packages <- c( "RMariaDB", "DBI", "dbplyr")
install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

# load functions 
source("code/functions/helpers_general.R")
source("code/functions/es_preprocessing.R")
source("code/functions/smartphonechangers.R")

#source("r_code/local_config.R") # load local config to access databases

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

str(ps_esanswer)

# We do not need the ps_esquestionnaire table for further analyses of the RQ at hand

## create separate dfs for affect and acoustic data

# select only page id 1 and 2 because those contain affect values

affect_es = ps_esanswer_extended  %>% 
  dplyr::filter(page_id == 1 | page_id == 2) 

# select only page id 21, 22, and 23 because contains audio logging data

al_es = ps_esanswer_extended  %>% 
  dplyr::filter(page_id == 21 | page_id == 22 | page_id == 23) 

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

# ### CORRECT TIMESTAMPS ####
# 
# # e.g., when participants traveled or there was a change in summer/winter time
# # this is relevant when pairing the es data with other time-stamp corrected sensing data
# 
# affect_es_timecorrected <- preprocessing_general_es(affect_es_filtered, phonestudy )
# al_es_timecorrected <- preprocessing_general_es(al_es_filtered, phonestudy)

### EDIT SMARTPHONE CHANGERS ####

# assign new unique user id (>=2000) to users from es data that had changed their phones during the study period
# 
# affect_es_changed <- apply_smartphonechanges(affect_es_timecorrected , "user_id")
# al_es_changed <- apply_smartphonechanges(al_es_timecorrected , "user_id")

## RS: hast du für duplicates in den emas gecheckt? Es gibt manchmal pro ES Instanz mehere Einträge 
## (das heimtücksische ist auch manchmal, dass die nicht ganz den gleichen timestamp haben, sondern um ein paar Miliseks verzögert auftreten)
# to do: check for duplicates!

# save
saveRDS(affect_es_filtered, "data/study1/affect_ema.RData")
saveRDS(al_es_filtered, "data/study1/al_ema.RData")

# finish