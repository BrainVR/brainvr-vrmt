#' Analyses the recall item collection performance
#'
#' @param obj vremt object
#' @param index order of the phase to analyse
#'
#' @return list with resutls
#' @export
#'
#' @examples
vremt_collection_performance <- function(obj, index){
  phase_obj <- get_recallItems_data(obj, index)
  if(is.null(phase_obj)){
    warning("There is not a recall at index ", index)
    return(NULL)
  }
  phase_task_index <- get_phase_task_index(phase_obj)
  task <- get_task_settings(phase_obj, phase_task_index)
  collected_items <- get_collected_items(phase_obj)
  res <- item_performance(task$items[[1]], collected_items)
  # Time
  # Number of drops
  # Path?
  return(as.data.frame(res))
}

#' Analyses the recall placemnent phase
#'
#' @param obj vremt object
#' @param index order of the phase to analyse
#'
#' @return list with results
#' @export
#'
#' @examples
vremt_placement_performance <- function(obj, index){
  # Correct order
  phase_obj <- get_recallPlacement_data(obj, index)
  if(is.null(phase_obj)){
    warning("There is not a recall at index ", index)
    return(NULL)
  }
  phase_task_index <- get_phase_task_index(phase_obj)
  task <- get_task_settings(phase_obj, phase_task_index)
  df_actions <- get_actions_log(recall)

  # participants can pick the object back and then drop it elsewhere, need to only
  # count the last drop
  drop <- select_last_drops(df_actions)

  # placemnt distance error
  res <- as.data.frame(unity_vector_to_numeric(drop$position),
                       row.names = drop$item_name)
  colnames(res) <- c("position_x", "position_z", "position_y")
  correct_position <- get_item_position(task$items[[1]])

  target_distance <- sapply(row.names(res), function(x){
    euclid_distance(as.numeric(res[x,]), as.numeric(correct_position[x,]))
    }
  )
  res <-  merge(res, as.data.frame(target_distance), by = "row.names")

  # order error
  df_order <- drop[order(drop$timestamp), c("item_name", "location")]
  df_order$order <- 1:nrow(df_order) #needs to be ordered (see line above)
  df_order$correct_order <- match(drop$item_name, task$items[[1]])
  df_order$order_error <- df_order$order - df_order$correct_order
  df_order <- merge(df_order, LOCATION_ITEM[, c("location", "arm")], by = "location")

  res <- merge(df_order, res, by.x = "item_name", by.y = "Row.names")

  ## Adds the correct solution
  res <- merge(res, LOCATION_ITEM[, c("location", "arm", "item")],
               by.x = "item_name", by.y = "item", all.x = TRUE, suffixes = c("", "_correct"))
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

# Wrapper to help the collection performacne analyse
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
