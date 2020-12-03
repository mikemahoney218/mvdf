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


#' Create an options string from a ... object
#'
#' @param dts The results of list(...) to create an options string from
#'
#' @keywords internal
create_options <- function(dts) {
  paste0(
    mapply(
      function(nm, ob) paste0(nm, "=", ob),
      names(dts),
      dts,
      SIMPLIFY = FALSE
    ),
    ", ",
    collapse = ","
  )
}
