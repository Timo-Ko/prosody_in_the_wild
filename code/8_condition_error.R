packages <- c("dplyr", "stringr", "data.table", "mlr3", "mlr3learners", "ranger")
invisible(lapply(packages, library, character.only = TRUE))

set.seed(123, kind = "L'Ecuyer")

source("code/functions/bmr_results.R")

# load analysis data
affect_voice <- readRDS("data/affect_voice.rds")
colnames(affect_voice) <- make.names(colnames(affect_voice), unique = TRUE)

# keep only rows relevant for the sentence-condition comparison
cmp_dat <- affect_voice %>%
  filter(condition %in% c("positive", "neutral", "negative")) %>%
  filter(!is.na(valence), !is.na(arousal))

# keep only participants who have retained data in all 3 conditions
eligible_users <- cmp_dat %>%
  distinct(user_id, condition) %>%
  count(user_id, name = "n_conditions") %>%
  filter(n_conditions == 3L) %>%
  pull(user_id)

cmp_dat <- cmp_dat %>%
  filter(user_id %in% eligible_users)

# create one shared participant-level fold map
n_folds <- 10L
users <- sort(unique(cmp_dat$user_id))

if (length(users) < n_folds) {
  stop("Fewer than 10 eligible participants available for synchronized CV.")
}

fold_map <- tibble::tibble(
  user_id = sample(users),
  cv_fold = rep(seq_len(n_folds), length.out = length(users))
)

cmp_dat <- cmp_dat %>%
  left_join(fold_map, by = "user_id") %>%
  mutate(cv_fold = factor(cv_fold, levels = seq_len(n_folds)))

# sanity check: every fold must contain participants for every condition
fold_check <- cmp_dat %>%
  distinct(user_id, condition, cv_fold) %>%
  count(cv_fold, condition, name = "n_users")

if (any(fold_check$n_users == 0L)) {
  stop("At least one fold has no users in one of the three conditions.")
}

# locate eGeMAPS feature block
egemaps_start <- match("F0semitoneFrom27.5Hz_sma3nz_amean", names(cmp_dat))
egemaps_end   <- match("equivalentSoundLevel_dBp", names(cmp_dat))

if (is.na(egemaps_start) || is.na(egemaps_end) || egemaps_start > egemaps_end) {
  stop("Could not locate the eGeMAPS feature block.")
}

egemaps_cols <- names(cmp_dat)[egemaps_start:egemaps_end]

suffix_map <- c(
  positive = "pos",
  neutral  = "neu",
  negative = "neg"
)

make_task <- function(data, target, condition_name) {
  task_dat <- data %>%
    filter(condition == condition_name) %>%
    select(user_id, cv_fold, all_of(target), all_of(egemaps_cols))
  
  task_id <- paste("egemaps", target, suffix_map[[condition_name]], sep = "_")
  
  task <- TaskRegr$new(
    id = task_id,
    backend = task_dat,
    target = target
  )
  
  task$col_roles$group <- "user_id"
  task$col_roles$feature <- setdiff(task$col_roles$feature, c("user_id", "cv_fold"))
  task
}

make_resampling <- function(task) {
  r <- rsmp("custom_cv")
  r$instantiate(task, col = "cv_fold")
  r
}

tasks <- list(
  make_task(cmp_dat, "valence", "positive"),
  make_task(cmp_dat, "valence", "neutral"),
  make_task(cmp_dat, "valence", "negative"),
  make_task(cmp_dat, "arousal", "positive"),
  make_task(cmp_dat, "arousal", "neutral"),
  make_task(cmp_dat, "arousal", "negative")
)

resamplings <- lapply(tasks, make_resampling)

# use one learner specification consistently across all tasks
lrn_rf <- lrn(
  "regr.ranger",
  num.trees = 1000
)

lgr::get_logger("bbotk")$set_threshold("warn")

design <- benchmark_grid(
  tasks = tasks,
  learners = list(lrn_rf),
  resamplings = resamplings,
  paired = TRUE
)

bmr_cond <- benchmark(
  design,
  store_models = FALSE,
  store_backends = FALSE
)

saveRDS(bmr_cond, "data/bmr_affect_condition_synced.rds")

# fold-wise MAE from the synchronized benchmarks
bmr_results_folds_cond <- extract_bmr_results(
  bmr_cond,
  list(msr("regr.mae"))
)

mae_long <- bmr_results_folds_cond %>%
  as_tibble() %>%
  filter(str_detect(task_id, "^egemaps_(valence|arousal)_(pos|neu|neg)$")) %>%
  mutate(
    target = if_else(str_detect(task_id, "arousal"), "Arousal", "Valence"),
    condition = case_when(
      str_detect(task_id, "_pos$") ~ "Positive",
      str_detect(task_id, "_neu$") ~ "Neutral",
      str_detect(task_id, "_neg$") ~ "Negative",
      TRUE ~ NA_character_
    ),
    condition = factor(condition, levels = c("Positive", "Neutral", "Negative"))
  ) %>%
  select(target, iteration, condition, regr.mae) %>%
  rename(mae = regr.mae) %>%
  arrange(target, iteration, condition)

friedman_res <- mae_long %>%
  group_by(target) %>%
  summarise(
    p_friedman = friedman.test(mae ~ condition | iteration, data = cur_data())$p.value,
    .groups = "drop"
  )

friedman_res

# finish