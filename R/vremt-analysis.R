vremt_collection_performance <- function(obj, index){
  obj_phase <- get_recallItems_data(obj, index)
  if(is.null(obj_phase)){
    warning("There is not a recall at index ", index)
    return(NULL)
  }
}

vremt_placemnt_performance <- function(obj, index){

}



