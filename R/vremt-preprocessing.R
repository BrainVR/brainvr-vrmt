preprocess_vremt <- function(obj){
  obj <- vremt_preprocess_experiment_log(obj)
  obj <- vremt_preprocess_actions_log(obj)
  return(obj)
}

vremt_preprocess_experiment_log <- function(obj){
  df_exp <- get_experiment_log(obj)
  df_exp$timestamp <- df_exp$Time
  # Deletes first pickup whihc is there as an error due to logging
  df_exp <- df_exp[-which(df_exp$Message == "pickup")[1], ]
  df_exp <- add_indices_experiment_log(df_exp)
  obj$data$experiment_log$data <- df_exp
  return(obj)
}

vremt_preprocess_actions_log <- function(obj){
  df_actions <- obj$data$actions_log
  colnames(df_actions) <- tolower(colnames(df_actions))
  df_actions[, ncol(df_actions)] <- NULL
  obj$data$actions_log <-  df_actions
  return(obj) 
}

add_indices_experiment_log <- function(df_test){
#' This hack assigns Index based on the number of occurances of hte same 
#' Event and message 
#' .e.g. the first recall start gets 1, seconds recall start gets 2
  df_test$Index <- ave(rep(1, nrow(df_test)),
                       list(df_test$Event, df_test$Message),
                       FUN = cumsum)
  return(df_test)
}
