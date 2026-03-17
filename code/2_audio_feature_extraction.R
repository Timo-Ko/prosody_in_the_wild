### PREPARATION ####

# Install and load required packages 

packages <- c("dplyr","stringr","jsonlite","stringi","tibble")
#install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

# load data

al_ema <- readRDS("data/al_ema.rds")

# ---- Load helper sentence table ----
sent_tbl <- readRDS("data/sentence_table_54.rds")

# ---- Normalization helper (must match helper-table creation) ----
normalize_txt <- function(x) {
  x %>%
    stringi::stri_trans_general("NFKC") %>%
    stringr::str_replace_all("\\s+", " ") %>%
    stringr::str_squish() %>%
    stringr::str_to_lower()
}


# 1) Recording-level sentence mapping (

# Split question into 3 sentences for each recording row
q_clean <- stringr::str_replace_all(al_ema$question, "\\s*\\.$", "")
parts_list <- stringr::str_split(q_clean, "\\.\\s+")
parts_list <- lapply(parts_list, function(x) { length(x) <- 3; x })  # pad to 3

# Long map: 3 rows per recording id (for QC only)
rec_sent_long <- tibble::tibble(
  id = rep(al_ema$id, each = 3),
  sentence_pos_in_prompt = rep(1:3, times = nrow(al_ema)),
  sentence_text_logged = unlist(parts_list, use.names = FALSE)
) %>%
  dplyr::mutate(
    sentence_text_logged = stringr::str_squish(sentence_text_logged),
    sentence_no_punct    = stringr::str_remove(sentence_text_logged, "\\s*[.!?]+\\s*$"),
    sentence_norm_logged = normalize_txt(sentence_no_punct)
  ) %>%
  dplyr::left_join(
    sent_tbl %>% dplyr::select(sentence_id, category, condition_3, sentence_norm),
    by = c("sentence_norm_logged" = "sentence_norm")
  )

# Diagnostics: unmatched sentences
unmatched <- rec_sent_long %>% dplyr::filter(is.na(sentence_id))
if (nrow(unmatched) > 0) {
  message("WARNING: Unmatched logged sentences found (distinct up to 100):")
  print(unmatched %>% dplyr::distinct(sentence_text_logged) %>% head(100))
}

saveRDS(rec_sent_long, "data/al_ema_recording_sentence_map_long.rds")

# Collapse to recording-level metadata (1 row per id)
rec_meta <- rec_sent_long %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(
    # condition should be homogeneous within a recording (all 3 sentences same valence set)
    condition = dplyr::first(condition_3[!is.na(condition_3)]),
    n_unique_conditions = dplyr::n_distinct(condition_3[!is.na(condition_3)]),
    # store the 3 sentence ids and categories for later reference if needed
    sentence_ids = paste(sentence_id, collapse = ";"),
    sentence_categories = paste(category, collapse = ";"),
    .groups = "drop"
  )

# QC: check that almost all recordings have exactly one condition
message("QC: n_unique_conditions distribution:")
print(table(rec_meta$n_unique_conditions, useNA = "ifany"))

saveRDS(rec_meta, "data/al_ema_recording_meta.rds")

# Attach recording-level metadata back to al_ema 
al_ema <- al_ema %>%
  dplyr::left_join(rec_meta, by = "id")

# Optional: drop recordings where prompt sentences did not map cleanly to a single condition
# (Recommended for analysis clarity.)
al_ema <- al_ema %>% dplyr::filter(!is.na(condition) & n_unique_conditions == 1)

# 2) Robust openSMILE feature extraction 

parse_acoustics <- function(x) {
  # remove leading/trailing square brackets if present: "[{...}]"
  x <- stringr::str_sub(x, 3, -3)
  # remove escaped backslashes
  x <- gsub("\\\\", "", x, fixed = FALSE)
  jsonlite::fromJSON(x, flatten = TRUE)
}

extract_named_vec <- function(obj, feature_names) {
  out <- rep(NA_real_, length(feature_names))
  names(out) <- feature_names
  
  if (is.null(obj)) return(out)
  
  v <- NULL
  if (is.data.frame(obj)) {
    if (nrow(obj) >= 1 && ncol(obj) >= 1) {
      v <- unlist(as.list(obj[1, , drop = FALSE]), use.names = TRUE)
    } else {
      return(out)
    }
  } else if (is.list(obj)) {
    if (length(obj) == 0) return(out)
    v <- unlist(obj, use.names = TRUE)
  } else {
    v <- obj
    if (is.null(names(v))) return(out)
  }
  
  if (is.null(v) || length(v) == 0 || is.null(names(v))) return(out)
  
  common <- intersect(names(v), feature_names)
  if (length(common) > 0) out[common] <- as.numeric(v[common])
  out
}

# Determine feature names from first parseable entry
first_idx <- which(!is.na(al_ema$answer_text) & nchar(al_ema$answer_text) > 5)[1]
if (is.na(first_idx) || length(first_idx) == 0) stop("No parseable answer_text entries found.")
first_parsed <- parse_acoustics(al_ema$answer_text[first_idx])

egemaps_names <- names(first_parsed$eGeMAPSv01a)
compare_names <- names(first_parsed$ComParE_2016)

if (length(egemaps_names) == 0 || length(compare_names) == 0) {
  stop("Could not detect eGeMAPS/ComParE names from first parsed row. Check answer_text format.")
}

# Keys for output tables (recording-level)
keys <- al_ema %>%
  dplyr::select(
    id, e_s_questionnaire_id, user_id, questionnaireStartedTimestamp,
    page_id, item_id,
    condition, sentence_ids, sentence_categories
  )

# Preallocate matrices
eg_mat <- matrix(NA_real_, nrow = nrow(al_ema), ncol = length(egemaps_names))
colnames(eg_mat) <- egemaps_names

cp_mat <- matrix(NA_real_, nrow = nrow(al_ema), ncol = length(compare_names))
colnames(cp_mat) <- compare_names

# Extraction loop
for (i in seq_len(nrow(al_ema))) {
  acoustics <- al_ema$answer_text[i]
  if (is.na(acoustics) || nchar(acoustics) < 5) next
  
  parsed <- tryCatch(parse_acoustics(acoustics), error = function(e) NULL)
  if (is.null(parsed)) next
  
  eg_mat[i, ] <- extract_named_vec(parsed$eGeMAPSv01a, egemaps_names)
  cp_mat[i, ] <- extract_named_vec(parsed$ComParE_2016, compare_names)
}

# Assemble feature dfs (one row per recording)
egemaps_feature_df <- dplyr::bind_cols(keys, as.data.frame(eg_mat))
compare_feature_df <- dplyr::bind_cols(keys, as.data.frame(cp_mat))

# drop unneeded cols
egemaps_feature_df$name <- NULL
egemaps_feature_df$frameTime <- NULL

compare_feature_df$name <- NULL
compare_feature_df$frameTime <- NULL

# Drop non-feature columns if present
compare_names <- setdiff(compare_names, c("name", "frameTime"))

# Merge to single table
voice_features <- dplyr::left_join(
  egemaps_feature_df,
  compare_feature_df %>% dplyr::select(id, dplyr::all_of(compare_names)),
  by = "id"
)

# 3) Save outputs
saveRDS(egemaps_feature_df, "data/egemaps_feature_df.rds")
saveRDS(compare_feature_df, "data/compare_feature_df.rds")
saveRDS(voice_features, "data/voice_features.rds")

# finish