#' @param bmr a mlr3 benchmark object
#' @param mes performance measures
#' @return df_results - a data frame with the perfromance measures per fold

extract_bmr_results <- function(bmr, measures) {
  library(data.table)
  
  # convert aggregated benchmark object to a data.table
  agg <- as.data.table(bmr$aggregate(measures))
  
  # prepare results collector
  all_res <- list()
  
  for (i in seq_len(nrow(agg))) {
    task_id    <- agg$task_id[i]
    learner_id <- agg$learner_id[i]
    rr         <- agg$resample_result[[i]]  # the ResampleResult object
    
    # extract *fold-level* performance
    sc <- as.data.table(rr$score(measures))
    sc[, task_id := task_id]
    sc[, learner_id := learner_id]
    
    all_res[[length(all_res) + 1]] <- sc
  }
  
  rbindlist(all_res, use.names = TRUE, fill = TRUE)
}


#' @param bmr_results a data frame with performance measures per fold
#' @return results.table - a summary table with median and SD across folds

results_table <- function(bmr_results) {
  library(dplyr)
  
  out <- bmr_results %>%
    group_by(task_id, learner_id) %>%
    summarise(
      # Spearman's rho
      Md_srho = median(regr.srho, na.rm = TRUE),
      SD_srho = sd(regr.srho, na.rm = TRUE),
      # CCC
      Md_ccc = median(regr.ccc, na.rm = TRUE),
      SD_ccc = sd(regr.ccc, na.rm = TRUE),
      # R^2
      Md_rsq = median(regr.rsq, na.rm = TRUE),
      SD_rsq = sd(regr.rsq, na.rm = TRUE),
      # MAE
      Md_mae = median(regr.mae, na.rm = TRUE),
      SD_mae = sd(regr.mae, na.rm = TRUE),
      # RMSE
      Md_rmse = median(regr.rmse, na.rm = TRUE),
      SD_rmse = sd(regr.rmse, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(task_id, learner_id) %>%
    mutate(
      srho = sprintf("%.3f (SD = %.3f)", Md_srho, SD_srho),
      ccc  = sprintf("%.3f (SD = %.3f)", Md_ccc,  SD_ccc),
      rsq  = sprintf("%.3f (SD = %.3f)", Md_rsq,  SD_rsq),
      mae  = sprintf("%.3f (SD = %.3f)", Md_mae,  SD_mae),
      rmse = sprintf("%.3f (SD = %.3f)", Md_rmse, SD_rmse)
    ) %>%
    select(task_id, learner_id, srho, ccc, rsq, mae, rmse)
  
  out
}


