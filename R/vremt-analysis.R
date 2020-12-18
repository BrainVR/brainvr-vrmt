vremt_collection_performance <- function(obj, index){
  phase_obj <- get_recallItems_data(obj, index)
  if(is.null(phase_obj)){
    warning("There is not a recall at index ", index)
    return(NULL)
  }
  phase_task_index <- get_phase_task_index(phase_obj)
  task <- get_task_settings(phase_obj, phase_task_index)
  collected_items <- get_collected_items(phase_obj)
  res <- list()
  res$item_performance <- item_performance(task$items[[1]], collected_items)
  # Time
  # Number of drops
  # Path?
  return(res)
}

vremt_placement_performance <- function(obj, index){
  # Correct order
  phase_obj <- get_recallPlacement_data(obj, index)
  if(is.null(phase_obj)){
    warning("There is not a recall at index ", index)
    return(NULL)
  }
  phase_task_index <- get_phase_task_index(phase_obj)
  task <- get_task_settings(phase_obj, phase_task_index)

  df_actions <- get_actions_log(phase_obj)
  # participants can pick the object back and then drop it elsewhere, need to only
  # count the last drop
  drop <- select_last_drops(df_actions)

  res <- list()
  # order
  correct_order <- task$items[[1]]
  dropped_order <- drop$item_name
  res$drop_order <- sapply(correct_order, function(x){
    which(x == correct_order) - which(x == dropped_order)
  })
  # location

  # path? Thats weird, as path should not mean anything
  return(res)
}

select_last_drops <- function(df_actions){
  drop <- df_actions[df_actions$action == "dropped", ]
  drop <- drop[order(drop$timestamp, decreasing = TRUE), ]
  ## remove duplicated user_id
  drop <- drop[!duplicated(drop[, c("action", "item_name")]), ]
  drop <- drop[order(drop$timestamp, decreasing = FALSE), ]
  return(drop)
}

item_performance <- function(wanted_items, collected_items) {
  res <- list()
  res$correct_items <- intersect_unique(wanted_items, collected_items)
  res$missing_items <- setdiff_unique(wanted_items, collected_items,
                                     nomatch = wanted_items)
  res$extra_items <- setdiff_unique(collected_items, wanted_items,
                                   nomatch = collected_items)
  res <- add_field_lengths(res, c("missing_items", "correct_items", "extra_items"))
  res <- collapse_fields(res, c("missing_items", "correct_items", "extra_items"))
  return(res)
}

# Calculates lengths of vectors and saves to new fields
add_field_lengths <- function(res, fields) {
  for (field in fields) {
    field_name <- paste("n", field, sep = "_")
    res[[field_name]] <- length(res[[field]])
  }
  return(res)
}

# collapses collected items or categories into a single vector
collapse_fields <- function(res, fields) {
  for (field in fields) {
    res[[field]] <- paste0(res[[field]], collapse = ",")
  }
  return(res)
}
