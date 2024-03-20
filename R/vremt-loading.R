#' Loads all vremt experiments from given folder
#'
#' @param folders Which folder to load
#' @param preprocess should the loaded files be preprocessed
#' @param flatten in case a single experiment is found, should the results be
#' flattened (list[[1]] becomes the result)
#'
#' @return list of class vremt
#' @export
#'
#' @examples
load_vremt_experiments <- function(folder, preprocess = TRUE, flatten = FALSE){
  message("Loading brainvr experiments")
  exps <- load_experiments(folder)
  message("Brainvr experiments loaded succesfully. Processing VREMT logs")
  for(i_exp in 1:length(exps)){
    exp <- exps[[i_exp]]
    message("----------------------")
    message("Processing log ", exp$timestamp)
    message("loading actions log")
    exp$data$actions_log <- open_actions_log(folder, exp)
    message("Processing experiment")
    if(preprocess) exp <- preprocess_vremt(exp)
    class(exp) <- append(class(exp), "vremt")
    exps[[i_exp]] <- exp
  }
  if(length(exps) == 1 & flatten) exps <- exps[[1]]
  return(exps)
}

open_actions_log <- function(folder, obj){
  pth <- find_actions_log_file(folder, obj)
  if(is.null(pth)) return(NULL)
  return(load_brainvr_log(pth))
}

find_actions_log_file <- function(folder, obj){
  ptr <- paste0("actions_", obj$timestamp)
  files <- list.files(folder, pattern = ptr, full.names = TRUE)
  if(length(files) != 1){
    warning("Actions log not found in the folder")
    return(NULL)
  }
  return(files[1])
}
