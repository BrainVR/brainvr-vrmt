#' Plots path
#'
#' @description Wrapper around \code{\link{plot_path}{navr}}
#'
#' @param obj vremt object
#' @param background should the background be added to the plot
#' @param ... ggplot arguments
#'
#' @return
#' @export
#'
#' @examples
plot_vremt_path <- function(obj, background = FALSE, ...){
  position <- obj$data$position
  g <- ggplot() +
    geom_navr_limits(position)
  if(background){
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
#'
#' @return lsit with ggplot geoms
#' @export
#'
#' @examples
geom_vremt_location <- function(locations){
  df_pos <- get_location_position(locations, simplify = FALSE)
  res <- list(geom_point(data = df_pos, aes(x = position_x, y = position_y),
                         shape = 21, size = 25, stroke = 2, color = "grey60",
                         alpha = 0.8),
              geom_text(data = df_pos, aes(x = position_x, y = position_y,
                                           label = row.names(df_pos)),
                        hjust = 0, nudge_x = 15, size = 7,
                        check_overlap = TRUE))
  return(res)
}


#' Plots locations of item placements
#'
#' @param recallPlacement recallPlacement object got through
#' \code{\link{get_recallPlacement_data}} function
#' @param mark_correct should the placements be colored based on the correctness
#' @param correct_distance what is considered correct limit distance
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
geom_vremt_placements <- function(recallPlacement, mark_correct = TRUE,
                                  correct_distance = 12, ...){
  performance <- vremt_placement_performance(recallPlacement)
  if(mark_correct){
  performance$correct <- sapply(performance$target_distance, function(x){
    ifelse(x < correct_distance, "correct", "incorrect")})
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
