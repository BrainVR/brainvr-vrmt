#' Returns portion of the object relevant to thegiven phase
#'
#' @param obj vremt object
#' @param phase Name of the phase as appears in the experiment log in the
#' Message parameter (e.g. exploration, recall, recallPlacement). Some phases
#' (recall) have multiple "subphases"
#' @param index Index of the phase (one based) in case there are multiple phases
#' of the same name
#'
#' @return vremt object with only data for a given phase
#' @export
#'
#' @examples
get_phase_data <- function(obj, phase, index = NA){
  phase_time <- get_phase_time(obj, phase, index)
  if(is.null(phase_time)) return(NULL)
  phase_obj <- filter_times(obj, phase_time)
  return(phase_obj)
}

#' Filters the VREMT object for particular timestamps Keeps all the list
#' information, such as settings etc.
#'
#' @param obj vremt object
#' @param times times which to filter
#'
#' @importFrom navr filter_times
#'
#' @return vremt object with only filtered times
#' @export
#'
#' @examples
filter_times.vremt <- function(obj, times){
  filtered_obj <- obj
  filtered_obj$data$actions_log$data <- filter_log_timestamp(
    filtered_obj$data$actions_log$data, times)
  filtered_obj$data$experiment_log$data <- filter_log_timestamp(
    filtered_obj$data$experiment_log$data, times)
  filtered_obj$data$position$data <- filter_log_timestamp(
    filtered_obj$data$position$data, times)
  return(filtered_obj)
}

#' @describeIn get_phase_data wrapper to get recallItems phase
#' @export
get_recallItems_data <- function(obj, index = NA){
  return(get_phase_data(obj, "recallItems", index))
}

#' @describeIn get_phase_data wrapper to get recallPlacement phase
#' @export
get_recallPlacement_data <- function(obj, index = NA){
  return(get_phase_data(obj, "recallPlacement", index))
}

#' Returns times of given phase.
#'
#' @param obj vremt object
#' @param phase name of the phase as it stands in the experiment_log. e.g. "recall"
#' @param order index of hte phase in case there were multiple phases
#' of the same number
#'
#' @return returns numeric(2) of time start
#' @export
#'
#' @examples
get_phase_time <- function(obj, phase, index = NA){
  df_phases <- get_experiment_log(obj)
  df_phases <- df_phases[df_phases$Sender == "phase", ]
  df_phases <- df_phases[df_phases$Message == phase, ]
  if(nrow(df_phases) == 0){
    warning("There are no phases of name ", phase)
    return(NULL)
  }
  if(is.na(index)){
    if(nrow(df_phases > 3)){
      warning("There are multiple phases ", phase, ". Need to speciy the index")
      return(NULL)
    }
  } else {
    df_phases <- df_phases[df_phases$Index == index, ]
  }
  if(nrow(df_phases) == 0){
    warning("There are no phases ", phase, " at index ", index)
    return(NULL)
  }
  # TODO - this is kinda weird and COULD be broken in some cases
  if(nrow(df_phases) > 3){
    warning("Multiple phases ", phase, " have the index ", index)
    return(NULLL)
  }
  start <- df_phases$Time[df_phases$Event == "start"]
  end <- df_phases$Time[df_phases$Event == "end"]
  # TODO - if missing the end? go to the end of experiment?
  return(c(start, end))
}

# Helper function to filter all various logs in the vremt object
filter_log_timestamp <- function(df_input, timewindow){
  out <- df_input[df_input$timestamp >= timewindow[1] &
                  df_input$timestamp <= timewindow[2], ]
  return(out)
}

get_experiment_time <- function(obj){
  # in this version the experiment
  end <- NULL
}

#' Returns action long
#'
#' @param obj vremt object
#'
#' @return
#' @export
get_actions_log <- function(obj){
  return(obj$data$actions_log$data)
}

#' Returns list of collected items from given object action log
#'
#' @param phase_obj Generally preselected vremt object. If you pass the entire
#' object, not just particular phase, all items will be returned.
#'
#' @return
#' @export
#'
#' @examples
get_collected_items <- function(phase_obj){
  df_actions <- get_actions_log(phase_obj)

  collected_items <- df_actions$item_name[df_actions$action == "picked"]
  dropped_items <- df_actions$item_name[df_actions$action == "dropped"]
  # removes dropped items from collected items
  if (length(dropped_items) > 0) {
    collected_items <- setdiff_unique(collected_items, dropped_items)
  }
  return(collected_items)
}

#' Returns the settings for the given phase object
#'
#' @description wrapper around get_task_settings and get_phase_task_index.
#' @param phase_obj
#'
#' @return
#' @export
#'
#' @examples
get_phase_task_settings <- function(phase_obj){
  index <- get_phase_task_index(phase_obj)
  if(!is.null(index)) settings <- get_task_settings(phase_obj, index)
  return(settings)
}

#' Returns the task settings for the object. If index is passed, returns only
#' settings for a particular trial
#'
#' @param obj vremt object
#' @param index numeric designation of the task order (usually between 1-3)
#'
#' @return list with items and locations fields
#' @export
#'
#' @examples
get_task_settings <- function(obj, index = NA){
  settings <- obj$data$experiment_info$Experiment$Task
  if(is.na(index)) return(settings)
  task <- settings[index, ]
  if(is.na(task$island)){
    warning("Task of index ", index, " does not exist")
    return(NULL)
  }
  return(task)
}

#' Returns the recall task index in the given object
#'
#' @description The recallPhases are randomized, with the specific task being
#' encoded in the particular pohase's action log. This extract what settings
#' ran during this particular phase. Only works on recall phase properly.
#' Exploration and pickup phases come in Task 0 1 2 ordcer always. This returns
#' ONE based task order for less interference with R subsetting.
#'
#' @param phase_obj filtered phase vremt object. Use get_phase_
#' @param zero_based task settings in the unity is reported as
#' zero based. should the task settings be returned as is in the log or returned
#' as a 1 based index (default)
#'
#' @return ONE BASED integer. This
#' @export
#'
#' @examples
get_phase_task_index <- function(phase_obj, zero_based = FALSE){
  df_actions <- get_actions_log(phase_obj)
  task_cities <- df_actions$taskcity
  if(any(task_cities != task_cities[1])){
    warning("there are multiple tasks in the given object. Filter it first so",
            "that it only contains a single task")
    return(NULL)
  }
  index <- as.integer(gsub("Task", "", task_cities[1]))
  if(!zero_based) index <- index + 1
  return(index)
}

#' Returns the position of a particular location (or item)
#'
#' @description either location or item has to be passed. The returned location
#' is the same for both location name and item name (e.g. both cemetery as
#' a location and a globe as an item have the same position)
#'
#' @param locations name of the location defining the location
#' @param simplify only valid in case of a single location return. If true,
#' only a vector is returned. Otherwise even single location is returned as a
#' data.frame
#'
#' @return named numeric vector or NULL if no location of such name is found or
#' a data.frame if
#' @export
#'
#' @examples
#' get_location_position("cemetery")
get_location_position <- function(locations, simplify = TRUE){
  out <- LOCATION_ITEM[LOCATION_ITEM$location %in% locations,
                       c("location", "position_x", "position_z", "position_y")]
  if(nrow(out) != length(locations)){
    warning("Locations of name ", locations[!(locations %in% LOCATION_ITEM$location)],
            " do not exist.")
    return(NULL)
  }
  rownames(out) <- out$location
  out$location <- NULL
  if(nrow(out) == 1 && simplify) out <- unlist(out)
  return(out)
}

#' @describeIn get_location_position getting item position
#' @param item name of the item
get_item_position <- function(items, simplify = TRUE){
  out <- LOCATION_ITEM[LOCATION_ITEM$item %in% items,
                       c("item", "position_x", "position_z", "position_y")]
  if(nrow(out) != length(items)){
    warning("Items of name ", items[!(items %in% LOCATION_ITEM$item)],
            " do not exist.")
    return(NULL)
  }
  rownames(out) <- out$item
  out$item <- NULL
  if(nrow(out) == 1 && simplify) out <- unlist(out)
  return(out)
}

#' @describeIn get_location_position returns position of a given arm
#' @param arms number 1-5 defining the arm of the island
get_arm_position <- function(arms, simplify = TRUE){
  if(!(all(arms %in% seq_len(5)))){
    warning("There are only arms 1 to 5")
    return(NULL)
  }
  out <- LOCATION_ITEM[LOCATION_ITEM$arm %in% arms,
                       c("arm", "position_x", "position_z", "position_y")]
  out <- unique(out)
  rownames(out) <- out$arms
  out$arm <- NULL
  if(nrow(out) == 1 && simplify) out <- unlist(out)
  return(out)
}
