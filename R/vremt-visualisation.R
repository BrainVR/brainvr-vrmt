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
plot_path.vremt <- function(obj, background = FALSE, ...){
  position <- obj$data$position
  g <- ggplot() +
    geom_navr_limits(position)
  if(background){
    g <- g + geom_navr_background(IMG_BACKGROUND_PATH,
                                  xlim = IMG_BOUNDARIES$x,
                                  ylim = IMG_BOUNDARIES$y)
  }
  g <- g + geom_navr_path(position, ...) + theme_void()
  return(g)
}
