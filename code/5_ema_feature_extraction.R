# preparation
packages <- c("dplyr", "stringr", "lubridate")
invisible(lapply(packages, library, character.only = TRUE))

affect_es <- readRDS("data/affect_ema.rds")

# 1) Clean + code (still label-based; best practice is to also filter by item/question)
affect_es_coded <- affect_es %>%
  dplyr::mutate(
    answer_text_clean = tolower(answer_text),
    answer_text_clean = gsub("[^[:alnum:]]", "", answer_text_clean),
    valence = dplyr::case_when(
      answer_text_clean == "sehrangenehm"   ~ 6,
      answer_text_clean == "angenehm"       ~ 5,
      answer_text_clean == "eherangenehm"   ~ 4,
      answer_text_clean == "eherunangenehm" ~ 3,
      answer_text_clean == "unangenehm"     ~ 2,
      answer_text_clean == "sehrunangenehm" ~ 1,
      TRUE ~ NA_real_
    ),
    arousal = dplyr::case_when(
      answer_text_clean == "sehraktiviert" ~ 6,
      answer_text_clean == "aktiviert"     ~ 5,
      answer_text_clean == "eheraktiviert" ~ 4,
      answer_text_clean == "eherinaktiv"   ~ 3,
      answer_text_clean == "inaktiv"       ~ 2,
      answer_text_clean == "sehrinaktiv"   ~ 1,
      TRUE ~ NA_real_
    )
  )

# Helper: first non-NA value
first_non_na <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_real_ else x[1]
}

# 2) Aggregate to one row per EMA instance, with conflict diagnostics
affect_df <- affect_es_coded %>%
  dplyr::group_by(e_s_questionnaire_id) %>%
  dplyr::summarise(
    user_id = dplyr::first(user_id),
    questionnaireStartedTimestamp = dplyr::first(questionnaireStartedTimestamp),
    
    valence = first_non_na(valence),
    arousal = first_non_na(arousal),
    # stress = first_non_na(stress)  # add once you code it
    
    n_valence_unique = dplyr::n_distinct(valence[!is.na(valence)]),
    n_arousal_unique = dplyr::n_distinct(arousal[!is.na(arousal)]),
    .groups = "drop"
  )

# 3) Flag potential problems (should generally be 0)
conflicts_val <- sum(affect_df$n_valence_unique > 1, na.rm = TRUE)
conflicts_aro <- sum(affect_df$n_arousal_unique > 1, na.rm = TRUE)
message("Conflicting multiple valence values within EMA instance: ", conflicts_val)
message("Conflicting multiple arousal values within EMA instance: ", conflicts_aro)


## account for smartphone changes in the affect data

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

affect_df_changed <- affect_df %>%
  # Left join to add new_id where applicable
  dplyr::left_join(id_mapping, by = c("user_id" = "old_id")) %>%
  # Replace old user_id with new_id where available
  dplyr::mutate(user_id = coalesce(new_id, user_id)) %>%
  # Drop the temporary new_id column
  dplyr::select(-new_id)

# 4) Save
saveRDS(affect_df_changed, "data/affect_df.rds")

# finish