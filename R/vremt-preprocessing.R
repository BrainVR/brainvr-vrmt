#' Preprocesses vremt object
#'
#' @param obj object of class vremt. loaded with load_vremt_experiments
#'
#' @return
#' @export
#'
#' @examples
preprocess_vremt <- function(obj){
  obj <- vremt_preprocess_experiment_log(obj)
  obj <- vremt_preprocess_actions_log(obj)
  obj <- vremt_preprocess_experiment_info(obj)
  obj$data$position <- add_area_boundaries(obj$data$position, AREA_SIZE)
  return(obj)
}

vremt_preprocess_experiment_info <- function(obj){
  # decompose_task
  task <- obj$data$experiment_info$Experiment$Task
  obj$data$experiment_info$Experiment$Task <- decompose_task(task)
  # unnest settings
  obj$data$experiment_info$Experiment$Settings <-
    obj$data$experiment_info$Experiment$Settings$settings
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
  df_actions <- obj$data$actions_log$data
  colnames(df_actions) <- tolower(colnames(df_actions))
  df_actions[, ncol(df_actions)] <- NULL
  obj$data$actions_log$data <- df_actions
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

#' Decomposes task as listed in the experiment info into data.frame
#' @param task character with task settings
#' @return data.frame (island, locations, )
decompose_task <- function(task){
  task <- gsub("\\]", "", task)
  task <- strsplit(task, "\\[")[[1]]
  task <- task[2:length(task)] # removes first always empty task
  out <- sapply(task, strsplit, ",", USE.NAMES = FALSE)
  res <- data.frame(
    island = sapply(out, `[`, 1),
    daytime = sapply(out, `[`, 7),
    weather = sapply(out, `[`, 8)
  )
  res$items <- sapply(out, function(x){x[2:6]}, simplify = FALSE)
  return(res)
}
