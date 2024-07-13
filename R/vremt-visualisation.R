#' Plots path
#'
#' @description Wrapper around \code{\link{plot_path}{navr}}
#'
#' @param obj vremt object
#' @param background should the background be added to the plot
#' @param version version of the experiment (2020 or 2024), if NA uses all
#' @param ... ggplot arguments
#'
#' @return
#' @export
#'
#' @examples
plot_vremt_path <- function(obj, version = 2024, background = FALSE, ...) {
  position <- obj$data$position
  g <- ggplot() +
    geom_navr_limits(position)
  if (background) {
    g <- g + geom_navr_background(get_background_image(),
                                  xlim = IMG_BOUNDARIES$x,
                                  ylim = IMG_BOUNDARIES$y)
  }
  g <- g + geom_navr_path(position, ...) + theme_void()
  return(g)
}

#' Plots locations and placement circles
#'
#' @param locations character vector of locations to be plotted
#' @param point_args list with ggplot geom_point arguments. can be used to override
#' the default arguments of location drawing. By default it is list(
#' shape = 21, size = 25, stroke = 2, color = "grey60", alpha = 0.8)
#' @param text_args list of ggplot geom_text arguemnts. overrides default
#' argumetns in text. Default is list(hjust = 0, nudge_x = 15, size = 7)
#'
#' @return list with ggplot geom_point and geom_text
#' @export
#'
#' @examples
geom_vremt_location <- function(locations, version = NA,
                                point_args = list(),
                                text_args = list()) {
  DEFAULT_POINT_ARGS <- list(shape = 21, size = 25, stroke = 2,
                             color = "grey60", alpha = 0.8)
  point_args <- modifyList(DEFAULT_POINT_ARGS, point_args)
  df_pos <- get_location_position(locations, version, simplify = FALSE)
  point_args$data <- df_pos
  point_args$mapping <- aes(x = position_x, y = position_y)
  point_geom <- do.call("geom_point", point_args)

  DEFAULT_TEXT_ARGS <- list(hjust = 0, nudge_x = 15, size = 7)
  text_args <- modifyList(DEFAULT_TEXT_ARGS, text_args)
  text_args$data <- df_pos
  text_args$mapping <- aes(x = position_x, y = position_y,
                       label = row.names(df_pos))
  text_args$check_overlap = TRUE
  txt_geom <- do.call("geom_text", text_args)

  res <- list(point_geom, txt_geom)
  return(res)
}


#' Plots locations of item placements
#'
#' @param recallPlacement recallPlacement object got through
#' \code{\link{get_recallPlacement_data}} function
#' @param mark_correct should the placements be colored based on the correctness
#' @param correct_distance what is considered correct limit distance
#' @param version version of the experiment (2020 or 2024), if NA uses all
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
geom_vremt_placements <- function(recallPlacement,
                                  version = 2024,
                                  mark_correct = TRUE,
                                  correct_distance = 20, ...) {
  performance <- vremt_placement_performance(recallPlacement)
  if (mark_correct) {
    asses_func <- \(x) ifelse(x < correct_distance, "correct", "incorrect")
    performance$correct <- sapply(performance$target_distance, asses_func)
  } else {
    performance$correct <- "correct"
  }
  res <- list(geom_point(data = performance, aes(x = position_x, y = position_y,
                                                 color = correct),
                         shape = 4, size = 5, stroke = 2),
              geom_text(data = performance, aes(x = position_x, y = position_y,
                                                label = item_name, color = correct),
                        hjust = 0, nudge_x = -15, size = 8, nudge_y = -10,
                        check_overlap = TRUE)
              )
  return(res)
}


#' Plots the recallPlacement phase
#' 
#' @param obj vremt object
#' @param i index of the phase
#' 
#' @return ggplot object
#' @export
recallPlacement_plot <- function(obj, i) {
  phase_obj <- get_recallPlacement_data(obj, i)
  i_phase <- get_phase_task_index(phase_obj)
  task <- get_task_settings(obj, i_phase)
  plt <- plot_vremt_path(phase_obj, background = TRUE,
                         color = "grey40", size = 1.25) +
    geom_vremt_location(task$locations[[1]], version = obj$version,
                        text_args = list(size = 7, nudge_x = 10,
                                         nudge_y = 10)) +
    geom_vremt_placements(phase_obj, correct_distance = 15) +
    scale_color_manual(values = c("#1d28e2", "#cd3333")) +
    guides(color = "none")
  return(plt)
}
