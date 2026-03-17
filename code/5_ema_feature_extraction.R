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


# 4) Save
saveRDS(affect_df, "data/affect_df.rds")