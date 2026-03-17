# Extract Information which beeps were answered out of all beeps that were sent (w/o the actual answers)

getEmaBeeps <- function(ps_esquestionnaire){
  
  # read data set1 for ema (read timestamps as character, see #7)
  es_q <-  ps_esquestionnaire %>% 
    dplyr::mutate(notificationTimestamp = as.character(notificationTimestamp), questionnaireStartedTimestamp = as.character(questionnaireStartedTimestamp), 
                  questionnaireEndedTimestamp = as.character(questionnaireEndedTimestamp)) %>% data.frame()
  colnames(es_q)[which(colnames(es_q) == "id")] <- "es_questionnaire_id" 
  
  
  # include information whether Beep was answered
  es_q$beep_started <- ifelse(!is.na(es_q$questionnaireStartedTimestamp), 1,0)
  es_q$beep_answered <- ifelse(is.na(es_q$questionnaireEndedTimestamp), 0,1)
  
  return(es_q)
}
