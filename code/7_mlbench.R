### PREPARATION ####

# Install and load required packages 

packages <- c( "dplyr", "parallel", "data.table", "ggplot2", "mlr3", "mlr3learners", "ranger", "glmnet", "future", "remotes", "stringr")
#install.packages(setdiff(packages, rownames(installed.packages())))  
lapply(packages, library, character.only = TRUE)

set.seed(123, kind = "L'Ecuyer") # set seed to make sure all results are reproducible

### READ IN DATA ####

# read in data frames
affect_voice  <- readRDS("data/affect_voice.rds")

# load required functions
source("code/functions/bmr_results.R")
source("code/functions/sign_test_folds.R")
source("code/functions/ccc_measure.R")

# make sure all feature names are valid for ml
colnames(affect_voice) <- make.names(colnames(affect_voice), unique = TRUE)

affect_voice$audSpec_Rfilt_sma.12._quartile3 <-
  as.numeric(affect_voice$audSpec_Rfilt_sma.12._quartile3)

#### CREATE TASKS ####

## sex classification

affect_voice_gender <- affect_voice[!is.na(affect_voice$Demo_GE1),] # create new df with no missing data for gender
affect_voice_gender$Demo_GE1 <- as.factor(affect_voice_gender$Demo_GE1) # convert gender to factor

# egemaps
egemaps_sex = TaskClassif$new(id = "egemaps_sex", 
                                          backend = affect_voice_gender[,c(which(colnames(affect_voice_gender)=="user_id"),
                                                                             which(colnames(affect_voice_gender)=="Demo_GE1"),  
                                                                             which(colnames(affect_voice_gender)=="F0semitoneFrom27.5Hz_sma3nz_amean"):which(colnames(affect_voice_gender)=="equivalentSoundLevel_dBp"))], 
                                          target = "Demo_GE1")


# compare
compare_sex = TaskClassif$new(id = "compare_sex", 
                              backend = affect_voice_gender[,c(which(colnames(affect_voice_gender)=="user_id"),
                                                               which(colnames(affect_voice_gender)=="Demo_GE1"),  
                                                               which(colnames(affect_voice_gender)=="audspec_lengthL1norm_sma_range"):which(colnames(affect_voice_gender)=="mfcc_sma_de.14._stddevFallingSlope"))], 
                              target = "Demo_GE1")

## affective states

# egemaps

# all sentences

# valence
egemaps_valence = TaskRegr$new(
  id = "egemaps_valence",
  backend = affect_voice[, c(which(colnames(affect_voice) == "user_id"),
                             which(colnames(affect_voice) == "valence"),
                             which(colnames(affect_voice) == "F0semitoneFrom27.5Hz_sma3nz_amean"):
                               which(colnames(affect_voice) == "equivalentSoundLevel_dBp"))],
  target = "valence"
)

# arousal
egemaps_arousal = TaskRegr$new(
  id = "egemaps_arousal",
  backend = affect_voice[, c(which(colnames(affect_voice) == "user_id"),
                             which(colnames(affect_voice) == "arousal"),
                             which(colnames(affect_voice) == "F0semitoneFrom27.5Hz_sma3nz_amean"):
                               which(colnames(affect_voice) == "equivalentSoundLevel_dBp"))],
  target = "arousal"
)

# positive sentences only
egemaps_valence_pos = TaskRegr$new(
  id = "egemaps_valence_pos",
  backend = affect_voice[affect_voice$condition == "positive",
                         c(which(colnames(affect_voice) == "user_id"),
                           which(colnames(affect_voice) == "valence"),
                           which(colnames(affect_voice) == "F0semitoneFrom27.5Hz_sma3nz_amean"):
                             which(colnames(affect_voice) == "equivalentSoundLevel_dBp"))],
  target = "valence"
)

egemaps_arousal_pos = TaskRegr$new(
  id = "egemaps_arousal_pos",
  backend = affect_voice[affect_voice$condition == "positive",
                         c(which(colnames(affect_voice) == "user_id"),
                           which(colnames(affect_voice) == "arousal"),
                           which(colnames(affect_voice) == "F0semitoneFrom27.5Hz_sma3nz_amean"):
                             which(colnames(affect_voice) == "equivalentSoundLevel_dBp"))],
  target = "arousal"
)


# negative sentences only
egemaps_valence_neg = TaskRegr$new(
  id = "egemaps_valence_neg",
  backend = affect_voice[affect_voice$condition == "negative",
                         c(which(colnames(affect_voice) == "user_id"),
                           which(colnames(affect_voice) == "valence"),
                           which(colnames(affect_voice) == "F0semitoneFrom27.5Hz_sma3nz_amean"):
                             which(colnames(affect_voice) == "equivalentSoundLevel_dBp"))],
  target = "valence"
)

egemaps_arousal_neg = TaskRegr$new(
  id = "egemaps_arousal_neg",
  backend = affect_voice[affect_voice$condition == "negative",
                         c(which(colnames(affect_voice) == "user_id"),
                           which(colnames(affect_voice) == "arousal"),
                           which(colnames(affect_voice) == "F0semitoneFrom27.5Hz_sma3nz_amean"):
                             which(colnames(affect_voice) == "equivalentSoundLevel_dBp"))],
  target = "arousal"
)

# neutral sentences only
egemaps_valence_neu = TaskRegr$new(
  id = "egemaps_valence_neu",
  backend = affect_voice[affect_voice$condition == "neutral",
                         c(which(colnames(affect_voice) == "user_id"),
                           which(colnames(affect_voice) == "valence"),
                           which(colnames(affect_voice) == "F0semitoneFrom27.5Hz_sma3nz_amean"):
                             which(colnames(affect_voice) == "equivalentSoundLevel_dBp"))],
  target = "valence"
)

egemaps_arousal_neu = TaskRegr$new(
  id = "egemaps_arousal_neu",
  backend = affect_voice[affect_voice$condition == "neutral",
                         c(which(colnames(affect_voice) == "user_id"),
                           which(colnames(affect_voice) == "arousal"),
                           which(colnames(affect_voice) == "F0semitoneFrom27.5Hz_sma3nz_amean"):
                             which(colnames(affect_voice) == "equivalentSoundLevel_dBp"))],
  target = "arousal"
)


# interspeech 2016

# valence
compare_valence = TaskRegr$new(
  id = "compare_valence",
  backend = affect_voice[, c(which(colnames(affect_voice) == "user_id"),
                             which(colnames(affect_voice) == "valence"),
                             which(colnames(affect_voice) == "audspec_lengthL1norm_sma_range"):
                               which(colnames(affect_voice) == "mfcc_sma_de.14._stddevFallingSlope"))],
  target = "valence"
)

# arousal
compare_arousal = TaskRegr$new(
  id = "compare_arousal",
  backend = affect_voice[, c(which(colnames(affect_voice) == "user_id"),
                             which(colnames(affect_voice) == "arousal"),
                             which(colnames(affect_voice) == "audspec_lengthL1norm_sma_range"):
                               which(colnames(affect_voice) == "mfcc_sma_de.14._stddevFallingSlope"))],
  target = "arousal"
)


## add blocking

# sex
egemaps_sex$col_roles$group = "user_id"
egemaps_sex$col_roles$feature = setdiff(egemaps_sex$col_roles$feature, "user_id")

compare_sex$col_roles$group = "user_id"
compare_sex$col_roles$feature = setdiff(compare_sex$col_roles$feature, "user_id")

# affective states

egemaps_valence$col_roles$group = "user_id"
egemaps_valence$col_roles$feature = setdiff(egemaps_valence$col_roles$feature, "user_id")

egemaps_arousal$col_roles$group = "user_id"
egemaps_arousal$col_roles$feature = setdiff(egemaps_arousal$col_roles$feature, "user_id")

# sentence conditions

egemaps_valence_pos$col_roles$group = "user_id"
egemaps_valence_pos$col_roles$feature = setdiff(egemaps_valence_pos$col_roles$feature, "user_id")

egemaps_arousal_pos$col_roles$group = "user_id"
egemaps_arousal_pos$col_roles$feature = setdiff(egemaps_arousal_pos$col_roles$feature, "user_id")

egemaps_valence_neg$col_roles$group = "user_id"
egemaps_valence_neg$col_roles$feature = setdiff(egemaps_valence_neg$col_roles$feature, "user_id")

egemaps_arousal_neg$col_roles$group = "user_id"
egemaps_arousal_neg$col_roles$feature = setdiff(egemaps_arousal_neg$col_roles$feature, "user_id")

egemaps_valence_neu$col_roles$group = "user_id"
egemaps_valence_neu$col_roles$feature = setdiff(egemaps_valence_neu$col_roles$feature, "user_id")

egemaps_arousal_neu$col_roles$group = "user_id"
egemaps_arousal_neu$col_roles$feature = setdiff(egemaps_arousal_neu$col_roles$feature, "user_id")

# compare2016 features
compare_valence$col_roles$group = "user_id"
compare_valence$col_roles$feature = setdiff(compare_valence$col_roles$feature, "user_id")

compare_arousal$col_roles$group = "user_id"
compare_arousal$col_roles$feature = setdiff(compare_arousal$col_roles$feature, "user_id")


#### CREATE LEARNERS ####

lrn_fl = lrn("regr.featureless")

# rf learner
lrn_rf = lrn(
  "regr.ranger",
  num.trees  = 1000
)

#### RESAMPLING ####

resampling = rsmp("cv", folds = 10L)

#### BENCHMARK ####

# avoid console output
logger = lgr::get_logger("bbotk")
logger$set_threshold("warn")

# show progress
progressr::handlers(global = TRUE)
progressr::handlers("progress")

## sex classification

bmgrid_sex = benchmark_grid(
  task = c(egemaps_sex, compare_sex),
  learner = list(lrn("classif.featureless", predict_type = "prob"), lrn("classif.ranger", num.trees =1000, predict_type = "prob")),
  resampling = resampling
)

future::plan("multisession", workers = 15) # enable parallelization

bmr_sex = benchmark(bmgrid_sex, store_models = F, store_backends = F) # execute the benchmark

saveRDS(bmr_sex, "results/bmr_sex.rds") # save results

## main benchmark

bmgrid_affect = benchmark_grid(
  task = c(egemaps_valence,
           egemaps_arousal,
           egemaps_valence_neg,
           egemaps_valence_pos,
           egemaps_valence_neu,
           egemaps_arousal_neg,
           egemaps_arousal_pos,
           egemaps_arousal_neu,
           compare_valence,
           compare_arousal
           ),
  learner = list(lrn_fl, lrn_rf),
  resampling = resampling
)

future::plan("multisession", workers = 15) # enable parallelization

bmr_affect = benchmark(bmgrid_affect, store_models = F, store_backends = F) # execute the benchmark

saveRDS(bmr_affect, "data/bmr_affect.rds") # save results

#### BENCHMARK RESULTS ####

# read in benchmark results
bmr_sex <- readRDS("data/bmr_sex.rds")
bmr_affect <- readRDS("data/bmr_affect.rds")

## view aggregated performance
bmr_sex$aggregate(msrs(c("classif.acc", "classif.bacc", "classif.auc")))

mes = c(msr_ccc, msrs(c("regr.srho", "regr.rsq", "regr.mae", "regr.rmse"))) # set performance measures

bmr_affect$aggregate(mes)

# set measures for table

## deep dive: retrieve benchmark results across tasks and learners for single cv folds

bmr_results_folds <- extract_bmr_results(bmr_affect, mes)

saveRDS(bmr_results_folds, "data/bmr_results_folds.rds")

## create combined overview table of performance

pred_table <- results_table(affect_voice, bmr_results_folds)

# save prediction tables
write.csv(pred_table, "results/pred_table.csv")

# finish