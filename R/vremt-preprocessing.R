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
  obj$data$position <- add_area_boundaries(obj$data$position, AREA_BOUNDARIES)
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
  df_actions$item_name <- convert_czech_to_en(df_actions$item_name)
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
  res$locations <- sapply(out, function(x){x[2:6]}, simplify = FALSE)
  res$items <- sapply(res$locations, convert_location_to_item, simplify = FALSE)
  return(res)
}


#' Converts location codes to item codes
#'
#' @description Each item only appears in a single location. This function then
#' conversts a vector of locations to a vector of items using the
#' \link{ITEM_CODES} data.
#'
#' @param locations english locations
#'
#' @return vector of characters with english codes of location names
#' @export
#'
#' @examples
convert_location_to_item <- function(locations){
  items <- sapply(locations, function(x) {
    LOCATION_ITEM$item[LOCATION_ITEM$location == x]
  }, simplify = TRUE, USE.NAMES = FALSE)
  return(items)
}

#' Converts czech names into english
#'
#' @param items
#'
#' @return
#' @export
#'
#' @examples
convert_czech_to_en <- function(items) {
  en <- sapply(items, function(x) {
    ITEM_CODES$name_en[ITEM_CODES$name_cz == x]
  }, simplify = TRUE, USE.NAMES = FALSE)
  return(en)
}
