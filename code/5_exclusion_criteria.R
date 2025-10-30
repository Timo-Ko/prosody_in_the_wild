#### COMBINE DATA OF SMARTPHONECHANGERS ####

library(dplyr)
library(tidyr)

affect_voice_raw <- readRDS("data/study1/affect_voice_study1.rds") # load voice data

changers = read.csv2("data/study1/Smartphonewechsel_20220608.csv") # load smartphone changers

## create unified if mapping

id_mapping <- changers %>%
  # Select relevant columns only 
  dplyr::select(NewId, p_0001_2, p_0001_3, p_0001_new) %>%
  # Convert from wide to long format
  pivot_longer(cols = starts_with("p_"),
               values_to = "old_id", 
               names_to = "variable") %>%
  # Drop NAs since these don't map to any old_id
  drop_na() %>%
  # Select only the new ID and old ID columns
  dplyr::select(new_id = NewId, old_id) %>%
  # Ensure all mappings are unique
  dplyr::distinct()

## apply new id mapping

affect_voice_changed <- affect_voice_raw %>%
  # Left join to add new_id where applicable
  dplyr::left_join(id_mapping, by = c("user_id" = "old_id")) %>%
  # Replace old user_id with new_id where available
  dplyr::mutate(user_id = coalesce(new_id, user_id)) %>%
  # Drop the temporary new_id column
  dplyr::select(-new_id)

# save data
saveRDS(affect_voice_changed, "data/study1/affect_voice_changed.rds") # save voice data

### FILTED DATA BASED ON AFFECT DATA (USER LEVEL) ####

# read data
affect_voice_changed <- readRDS("data/study1/affect_voice_changed.rds") # load voice data

## remove participants with less than 10 voice samples (one per condition) and corresponding experience sampling instances

# count how many voice samples with valence and arousal ratings are available per participant
count_voice_user <- affect_voice_changed %>% 
  dplyr::group_by(user_id) %>% 
  dplyr::summarize(count = n()) %>% 
  dplyr::arrange(desc(count))

hist(count_voice_user$count, breaks = 50)

# count participants with less than 10 voice samples (158 participants)
length(which(count_voice_user$count < 10))

# Filter the affect_voice data frame participants with at least 10 voice samples
affect_voice_filtered <- affect_voice_changed %>%
  dplyr::semi_join(count_voice_user %>% dplyr::filter(count >= 10), by = "user_id")

# compute variance in valence and arousal responses per participant
var_es_user <- affect_voice_filtered %>%
  dplyr::group_by(user_id) %>%
  dplyr::select(user_id, valence, arousal) %>%
  dplyr::mutate(var_valence = var(valence, na.rm = T), var_arousal = var(arousal, na.rm = T)) %>%
  dplyr::slice(1) #keep one row per user 

# find participants with zero variance in their valence AND arousal responses across all their es (they were probably straightlining)
length(which(var_es_user$var_valence == 0 & var_es_user$var_arousal == 0)) # 8 participants fall into the straightliner category

# remove those eight straightliners

affect_voice_filtered <- affect_voice_filtered %>%
  group_by(user_id) %>%
  filter(var(valence, na.rm = TRUE) != 0 | var(arousal, na.rm = TRUE) != 0) %>%
  ungroup()

## supplementary analysis: compute affect baseline per participant

# compute median valence and arousal per participant as baseline ("trait") score

median_affect_user <- affect_voice_filtered  %>% 
  dplyr::group_by(user_id) %>%
  dplyr::mutate(md_valence = median(valence, na.rm =T), md_arousal = median(arousal, na.rm =T)) %>%
  dplyr::slice(1) #keep one row per user 

# append median affect column to affect df
affect_voice_filtered <- merge(affect_voice_filtered, median_affect_user[,c("user_id", "md_valence", "md_arousal")], by = "user_id")

# compute deviation of current affect from baseline for each participant
affect_voice_filtered$diff_valence <- as.numeric(affect_voice_filtered$valence - affect_voice_filtered$md_valence)
affect_voice_filtered$diff_arousal <- as.numeric(affect_voice_filtered$arousal - affect_voice_filtered$md_arousal)

# arrange cols
affect_voice_filtered <- affect_voice_filtered  %>% 
  dplyr::select(c("e_s_questionnaire_id", "questionnaireStartedTimestamp", "id", "user_id" , "Demo_A1", "Demo_GE1", "condition", "valence", "md_valence", "diff_valence", "arousal", "md_arousal", "diff_arousal"), everything())

### CLEAN DATA BASED ON VOICE INDICATORS (INSTANCE LEVEL) ####

## find samples where participants did not record voice in their audio samples

hist(affect_voice_filtered$voicingFinalUnclipped_sma_amean, breaks = 1000) #plot distribution 
hist(affect_voice_filtered$VoicedSegmentsPerSec, breaks = 1000) #plot (normal distribution)
hist(affect_voice_filtered$MeanVoicedSegmentLengthSec, breaks = 1000) #plot (normal distribution)

# remove all instances based on those features
affect_voice_cleaned <- affect_voice_filtered %>%
  dplyr::filter(
    voicingFinalUnclipped_sma_amean >= 0.5 |
      VoicedSegmentsPerSec > 0 |
      MeanVoicedSegmentLengthSec > 0
  )

# investigate excluded samples
removed_cases <- anti_join(affect_voice_filtered, affect_voice_cleaned)

nrow(removed_cases) # number of removed cases 
length(unique(removed_cases$user_id)) # number of user_ids of removed cases

# save cleaned df
saveRDS(affect_voice_cleaned, "data/study1/affect_voice_study1_cleaned.rds")

### DESCRIPTIVES OF FINAL DATA ####

# number of participants total 
length(unique(affect_voice_cleaned$user_id))

# number of es total
length(unique(affect_voice_cleaned$e_s_questionnaire_id))

# number of audio logs total
dim(affect_voice_cleaned)

# avg number of es and audio logs per participant 

summary_stats <- affect_voice_cleaned %>%
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


# distribution of raw valence and arousal ratings across es instances
hist(affect_voice_cleaned$valence)
hist(affect_voice_cleaned$arousal)

table(affect_voice_cleaned$valence)
table(affect_voice_cleaned$arousal)

# distribution of valence and arousal differences from participants' baseline across es instances
hist(affect_voice_cleaned$diff_valence)
hist(affect_voice_cleaned$diff_arousal)

table(affect_voice_cleaned$diff_valence)
table(affect_voice_cleaned$diff_arousal)

## FINISH