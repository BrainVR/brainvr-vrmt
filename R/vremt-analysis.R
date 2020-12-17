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
  return(res)
}

vremt_placemnt_performance <- function(obj, index){

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
