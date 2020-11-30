#' Check to see if any values in a vector are missing (NULL, NA, INF, or NaN)
#'
#' @param obj The vector of values to check
#'
#' @keywords internal
any_missing <- function(obj) {
  any(is.null(obj) |
    is.na(obj) |
    is.infinite(obj) |
    is.nan(obj) |
    length(obj) == 0)
}

#' Check to see if all values in a vector are missing (NULL, NA, INF, or NaN)
#'
#' @param obj The vector of values to check
#'
#' @keywords internal
all_missing <- function(obj) {
  all(is.null(obj) |
    is.na(obj) |
    is.infinite(obj) |
    is.nan(obj) |
    length(obj) == 0)
}
