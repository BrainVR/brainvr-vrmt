#' Analyses the recall item collection performance
#'
#' @param recallItems vremt recallItem object receoved with
#' \code{\link{get_recallItems_data}}
#'
#' @return list with resutls
#' @export
#'
#' @examples
vremt_collection_performance <- function(recallItems) {
  # TODO
  phase_task_index <- get_phase_task_index(recallItems)
  if (is.null(phase_task_index)) {
    warning("No task index found")
    return(NULL)
  }
  task <- get_task_settings(recallItems, phase_task_index)
  res <- list()

  res$collected_items <- get_collected_items(recallItems)
  res$wanted_items <- task$items[[1]]
  res$summary <- item_performance(res$wanted_items, res$collected_items)
  res$summary <- as.data.frame(res$summary)
  # Time
  # Number of drops
  # Path?
  return(res)
}

#' Analyses the recall placement phase
#'
#' @param recallPlacement vremt placement object got with
#' \code{\link{get_recallPlacement_data}}
#' @param index order of the phase to analyse
#'
#' @return list with results
#' @export
#'
#' @examples
vremt_placement_performance <- function(recallPlacement){
  phase_task_index <- get_phase_task_index(recallPlacement)
  task <- get_task_settings(recallPlacement, phase_task_index)
  df_actions <- get_actions_log(recallPlacement)

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
  res <- merge(res, as.data.frame(target_distance), by = "row.names")

  # order error
  df_order <- drop[order(drop$timestamp), c("item_name", "location")]
  df_order$order <- 1:nrow(df_order) #needs to be ordered (see line above)
  df_order$correct_order <- match(drop$item_name, task$items[[1]])
  df_order$order_error <- df_order$order - df_order$correct_order
  df_order <- merge(df_order, LOCATION_ITEM[, c("location", "arm")],
                    by = "location", all.x = TRUE)

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

#' Returns which arm is closest
#'
#' @description this is still better than get last get_last_bridge_entered
#' due to complications with that function. See its manual for better description
#'
#' @param location Location is set as a numeric(3)
#'
#' @return
#' @export
#'
#' @examples
get_closest_arm <- function(location){
  distances <- get_arm_distances(location)
  min <- which(min(distances) == distances)
  return(min)
}

#' Return distances to all arms
#'
#' @param location numeric(3) with position
#'
#' @return vector 5 with distances to each arm
#' @export
#'
#' @examples
get_arm_distances <- function(location){
  arms <- get_arm_position()
  distances <- stats::dist(rbind(arms, location))
  distances <- as.numeric(as.matrix(distances)[6, 1:5])
  return(distances)
}
