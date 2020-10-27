load_vremt_experiments <- function(folder, preprocess = TRUE){
  exps <- load_experiments(folder)
  for(i_exp in 1:length(exps)){
    exp <- exps[[i_exp]]
    exp$data$actions_log <- open_actions_log(folder, exp)
    if(preprocess) exp <- preprocess_vremt(exp)
    exps[[i_exp]] <- exp
  }
  return(exps)
}
load_vremt_experiment <- function(folder, preprocess = TRUE){
  exp <- load_experiment(folder)
  exp$data$actions_log <- open_actions_log(folder, exp)
  if(preprocess) exp <- preprocess_vremt(exp)
  return(exp)
}

open_actions_log <- function(folder, obj){
  pth <- find_actions_log_file(folder, obj)
  if(is.null(pth)) return(NULL)
  return(load_actions_log(pth))
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

load_actions_log <- function(pth){
  res <- read.table(pth, skip = 9, header = TRUE, sep = ";", encoding = "UTF-8")
  return(res)
}
