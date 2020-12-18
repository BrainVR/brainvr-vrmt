#' Correct answers binding specific locations and items to a concrete arm
#'
#' This dataset allows analysis of correct answers as the items are effectively
#' coded as locations not itmes (each item always belong to a single location)
#'
#' @format A data frame with 15 values and 3 variables:
#' \describe{
#'   \item{location}{English name of the location}
#'   \item{item}{Name of the item in EN code}
#'   \item{arm}{Numbering order of the arm with the location (1-5)}
#'   \item{position_x}{Item's X position}
#'   \item{position_y}{Item's Y position}
#'   \item{position_z}{Item's Z (height) position}
#'   ...
#' }
"LOCATION_ITEM"

#' Dataset with Czech and English codes for items.
#'
#' This dataset allows translation of Czech codes from logs to more error
#' error free English variants. It is primarily used during data loading
#'
#' @format A data frame with 25 values and 4 variables:
#' \describe{
#'   \item{name_cz}{Czech item name}
#'   \item{name_en}{English item name}
#'   \item{position}{Position of the item given as a string (X, Y, Z)}
#'   \item{details}{some specific description of the item}
#'   ...
#' }
"ITEM_CODES"

