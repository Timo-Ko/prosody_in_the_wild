#### COMBINE DATA OF SMARTPHONECHANGERS ####

library(dplyr)
library(tidyr)

voice_features <- readRDS("data/voice_features.rds") # load voice data

changers = read.csv2("data/Smartphonewechsel_20220608.csv") # load smartphone changers

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

voice_features_changed <- voice_features %>%
  # Left join to add new_id where applicable
  dplyr::left_join(id_mapping, by = c("user_id" = "old_id")) %>%
  # Replace old user_id with new_id where available
  dplyr::mutate(user_id = coalesce(new_id, user_id)) %>%
  # Drop the temporary new_id column
  dplyr::select(-new_id)

# save data
saveRDS(voice_features_changed, "data/voice_features_changed.rds") # save voice data

### CLEAN DATA BASED ON VOICE INDICATORS (INSTANCE LEVEL) ####

## find samples where participants did not record voice in their audio samples

hist(voice_features_changed$voicingFinalUnclipped_sma_amean, breaks = 1000) #plot distribution 
hist(voice_features_changed$VoicedSegmentsPerSec, breaks = 1000) #plot (normal distribution)
hist(voice_features_changed$MeanVoicedSegmentLengthSec, breaks = 1000) #plot (normal distribution)
hist(voice_features_changed$HNRdBACF_sma3nz_amean, breaks = 1000) #plot (normal distribution)

# remove all instances based on those features
voice_features_cleaned <- voice_features_changed %>%
  dplyr::filter(
    voicingFinalUnclipped_sma_amean >= 0.5 &
    VoicedSegmentsPerSec > 0 &
    MeanVoicedSegmentLengthSec > 0 &
    HNRdBACF_sma3nz_amean > 0
  )

# investigate excluded samples
removed_cases <- anti_join(voice_features_changed, voice_features_cleaned)

nrow(removed_cases) # number of removed cases 
length(unique(removed_cases$user_id)) # number of user_ids of removed cases

# save cleaned df
saveRDS(voice_features_cleaned, "data/voice_features_cleaned.rds")

## FINISH