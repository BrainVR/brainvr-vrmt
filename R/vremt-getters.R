#' Returns portion of the object relevant to thegiven phase
#'
#' @param obj
#' @param phase Name of the phase as appears in the experiment log in the
#' Message parameter (e.g. exploration, recall, recallPlacement). Some phases
#' (recall) have multiple "subphases"
#' @param index Index of the phase (one based) in case there are multiple phases
#' of the same name
#'
#' @return
#' @export
#'
#' @examples
get_phase_data <- function(obj, phase, index = NA){
  phase_time <- get_phase_time(obj, phase, index)
  phase_obj <- obj
  phase_obj$data$actions_log$data <- filter_log_timestamp(
    phase_obj$data$actions_log$data, phase_time)
  phase_obj$data$experiment_log$data <- filter_log_timestamp(
    phase_obj$data$experiment_log$data, phase_time)
  phase_obj$data$position$data <- filter_log_timestamp(
    phase_obj$data$position$data, phase_time)
  return(phase_obj)
}

#' Returns times of given phase.
#'
#' @param obj vremt object
#' @param phase name of the phase as it stands in the experiment_log. e.g. "recall"
#' @param order index of hte phase in case there were multiple phases
#' of hte same number
#'
#' @return returns numeric(2) of time start
#' @export
#'
#' @examples
get_phase_time <- function(obj, phase, index = NA){
  df_phases <- get_experiment_log(obj)
  df_phases <- df_phases[df_phases$Sender == "phase", ]
  df_phases <- df_phases[df_phases$Message == phase, ]
  if(is.na(index)){
    if(nrow(df_phases > 3)){
      warning("There are multiple phases of this name. Need to speciy the index")
      return(NULL)
    }
  } else {
    df_phases <- df_phases[df_phases$Index == index, ]
  }
  if(nrow(df_phases) == 0) return(NULL)
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
