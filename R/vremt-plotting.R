#' Plots path
#'
#' @description Wrapper around \code{\link{plot_path}{navr}}
#'
#' @param obj vremt object
#' @param ... ggplot arguments
#'
#' @return
#' @export
#'
#' @examples
plot_path.vremt <- function(obj, ...){
  position <- obj$data$position
  plot_path(position, ...)
}
