#' Preprocesses vremt object
#'
#' @param obj object of class vremt. loaded with load_vremt_experiments
#' @param version version of the ITEM_CODES or loctions to use. If NA, all items are used
#'
#' @return
#' @export
#'
#' @examples
preprocess_vremt <- function(obj, version = NA) {
  # check if the obj has version set
  if (!is.null(obj$version) && !is.na(version)) {
    message("Version of the object is already set. Overwriting with ", version)
    obj$version <- obj$version
  }
  obj <- vremt_preprocess_experiment_log(obj, version)
  obj <- vremt_preprocess_actions_log(obj, version)
  obj <- vremt_preprocess_experiment_info(obj, version)
  obj$data$position <- add_area_boundaries(obj$data$position, AREA_BOUNDARIES)
  return(obj)
}

vremt_preprocess_experiment_info <- function(obj, version) {
  task <- obj$data$experiment_info$Experiment$Task
  obj$data$experiment_info$Experiment$Task <- decompose_task(task, version)
  obj$data$experiment_info$Experiment$Settings <-
    obj$data$experiment_info$Experiment$Settings$settings
  return(obj)
}

#' @importFrom brainvr.reader get_experiment_log
vremt_preprocess_experiment_log <- function(obj, version) {
  df_exp <- get_experiment_log(obj)
  df_exp$timestamp <- df_exp$Time
  # no longer needed as the loggin has been fixes
  df_exp$Sender[grep("subphase", df_exp$Event)] <- "phase"
  df_exp$Event <- gsub(" subphase", "", df_exp$Event)
  df_exp <- add_indices_experiment_log(df_exp)
  obj$data$experiment_log$data <- df_exp
  return(obj)
}

vremt_preprocess_actions_log <- function(obj, version) {
  df_actions <- get_actions_log(obj)
  colnames(df_actions) <- gsub("_", "", colnames(df_actions))
  to_snake_case <- \(x) {
    tolower(gsub("([a-z])([A-Z])", "\\1_\\2", x))
  }
  colnames(df_actions) <- sapply(colnames(df_actions),
                  to_snake_case, simplify = TRUE, USE.NAMES = FALSE)
  df_actions$item_name <- convert_czech_to_en(df_actions$item_name, version)
  df_actions$phase <- gsub("recallSpaceSound", "recallPlacement",
                           df_actions$phase)
  if (version == "2020") {
    # rename column taskcity to trial_name
    df_actions$trial_name <- df_actions$taskcity
  }
  obj$data$actions_log$data <- df_actions
  return(obj)
}

add_indices_experiment_log <- function(df_test) {
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
#' @param version version of the ITEM_CODES or loctions to use.
#' If NA, all items are used
#'
#' @return data.frame (island, locations, )
decompose_task <- function(task, version) {
  task <- gsub("\\]", "", task)
  task <- strsplit(task, "\\[")[[1]]
  task <- task[2:length(task)] # removes first always empty task
  out <- sapply(task, strsplit, ",", USE.NAMES = FALSE)
  res <- data.frame(
    island = sapply(out, `[`, 1),
    daytime = sapply(out, `[`, 7),
    weather = sapply(out, `[`, 8)
  )
  res$locations <- sapply(out, function(x) { x[2:6] }, simplify = FALSE)
  conversion_fnc <- \(x) convert_location_to_item(x, version = version)
  res$items <- sapply(res$locations, conversion_fnc, simplify = FALSE)
  return(res)
}


#' Converts location codes to item codes
#'
#' @description Each item only appears in a single location. This function then
#' conversts a vector of locations to a vector of items using the
#' \link{ITEM_CODES} data.
#'
#' @param locations english locations
#' @param version if not NA ("2020" and "2024" available),
#' only the data belonging to the version of the ITEM_CODES is used
#'
#' @return vector of characters with english codes of location names
#' @export
#'
#' @examples
convert_location_to_item <- function(locations, version = NA) {
  if (!is.na(version)) {
    df_locations <- LOCATION_ITEM[LOCATION_ITEM$version == version, ]
  } else {
    df_locations <- LOCATION_ITEM
  }
  replace_func <- function(x) {
    item_line <- df_locations[df_locations$location == x, ]
    if (nrow(item_line) == 0) return(paste0("MissingLocation(", x, ")"))
    return(item_line$item)
  }
  items <- sapply(locations, replace_func, simplify = TRUE, USE.NAMES = FALSE)
  return(items)
}

#' Converts czech names into english
#'
#' @param items
#' @param version if not NA ("2020" and "2024" available),
#' only the data belonging to the version of the ITEM_CODES is used
#'
#' @return
#' @export
#'
#' @examples
convert_czech_to_en <- function(items, version = NA) {
  if (!is.na(version)) {
    df_items <- ITEM_CODES[ITEM_CODES$version == version, ]
  } else {
    df_items <- ITEM_CODES
  }
  replace_func <- function(x) {
    item_line <- df_items[df_items$name_cz == x, ]
    if (nrow(item_line) == 0) return(paste0("MissingEN(", x, ")"))
    return(item_line$name_en)
  }
  en <- sapply(items, replace_func, simplify = TRUE, USE.NAMES = FALSE)
  return(en)
}
