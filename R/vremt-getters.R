get_phase_data <- function(obj, phase, order = NA){
  exp$data
  return(ob)
}

#' Title
#'
#' @param obj 
#' @param phase 
#' @param order 
#'
#' @return returns numeric(2) 
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

filter_log_timestamp <- function(df_input, timewindow){
  out <- df_input[df_input$timestamp >= timewindow[1] &
                  df_input$timestamp <= timewindow[2], ]
  return(out)
}

get_experiment_time <- function(obj){
  # in this version the experiment 
  end <- NULL
}