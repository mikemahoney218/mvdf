#' Check to see if any values in a vector are missing (NULL, NA, INF, or NaN)
#'
#' @param obj The vector of values to check
#'
#' @keywords internal
any_missing <- function(obj) {
  any(is_missing(obj))
}

#' Check to see if all values in a vector are missing (NULL, NA, INF, or NaN)
#'
#' @param obj The vector of values to check
#'
#' @keywords internal
all_missing <- function(obj) {
  all(is_missing(obj))
}

#' Check values in a vector for missingness (NULL, NA, INF, or NaN)
#'
#' @param obj The vector of values to check
#'
#' @keywords internal
is_missing <- function(obj) {
  is.null(obj) |
    is.na(obj) |
    is.infinite(obj) |
    is.nan(obj) |
    length(obj) == 0
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

#' Convert decimal degrees to radians
#'
#' @param deg A vector of values, in decimal degrees, to convert to radians
#'
#' @family utilities
#'
#' @return A vector of the same length in radians
#'
#' @keywords internal
deg_to_rad <- function(deg) {
  stopifnot(is.numeric(deg))
  deg * base::pi / 180
}

#' Convert R logical values to Python equivalents
#'
#' @param dots The results of running `list(...)` in an exporter function.
#'
#' @family utilities
#'
#' @keywords internal
pythonize_booleans <- function(dots) {
  # friendly conversion from R logicals to Python
  dots[which(is.logical(dots) && dots == FALSE)] <- "False"
  dots[which(is.logical(dots) && dots == TRUE)] <- "True"
  dots
}

#' Build the location argument for exporting classes inheriting from `mvdf_obj`
#'
#' @param object An object inheriting from `mvdf_obj`
#'
#' @family utilities
#'
#' @keywords internal
build_location <- function(object) {
  mvdf_df <- mvdf(object)
  location <- glue::glue(
    "location=({xloc}, {yloc}, {zloc})",
    xloc = mvdf_df$x,
    yloc = mvdf_df$y,
    zloc = mvdf_df$z
  )
}


#' Standard way to evaluate arguments for class-creating functions
#'
#' This approach enables users to specify arguments as quoted strings
#' referencing column names (passes the first try, passes the second try), as
#' unquoted column names evaluated via rlang's NSE (fails the first try, passes
#' the second try), or as pure values (passes the first try, fails the second
#' try).
#'
#' If the user provides an unquoted string that does not correspond to a column,
#' this function returns NULL.
#'
#' @param data The data frame to try retrieving values from.
#' @param arg The argument to evaluate.
#'
#' @keywords internal
eval_arg <- function(data, arg) {
  arg <- tryCatch(arg, error = function(e) rlang::ensym(arg))
  tryCatch(data[[arg]], error = function(e) arg)
}
