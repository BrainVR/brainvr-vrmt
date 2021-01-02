#' unlike setdiff and intersect, returns proper count for differences
#' @noRd
setdiff_unique <- function(wanted, received, nomatch = wanted) {
  # https://stackoverflow.com/questions/52941312/set-difference-between-two-vectors-with-duplicate-values # nolint
  i_matching <- (-match(make.unique(as.character(received)),
                        make.unique(as.character(wanted)), nomatch = 0))
  if (all(i_matching == 0)) {
    res <- nomatch
  } else {
    res <- wanted[i_matching]
  }
  return(res)
}
#' @noRd
intersect_unique <- function(wanted, received) {
  res <- wanted[match(make.unique(as.character(received)),
                      make.unique(as.character(wanted)), nomatch = 0)]
  return(res)
}

# Calculates lengths of vectors and saves to new fields
add_field_lengths <- function(res, fields) {
  for (field in fields) {
    field_name <- paste("n", field, sep = "_")
    res[[field_name]] <- length(res[[field]])
  }
  return(res)
}

# collapses collected items or categories into a single vector
collapse_fields <- function(res, fields) {
  for (field in fields) {
    res[[field]] <- paste0(res[[field]], collapse = ",")
  }
  return(res)
}
