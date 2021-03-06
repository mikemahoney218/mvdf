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
    do.call(
      paste,
      c(
        mapply(
          function(nm, ob) paste0(nm, "=", ob),
          names(dts),
          dts,
          SIMPLIFY = FALSE
        ),
        sep = ","
      )
    ),
    ", "
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
  glue::glue(
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

#' Standard way to replace & replicate values for class-creation functions
#'
#' @param vec The vector to replace and replicate values throughout
#' @param length_out The target length of the vector, used to repeat length 1 
#' vectors and ignored in all other cases.
#' @param replace_val The value to replace missing values with
#'
#' @return A vector of length `length_out` with missing values replaced by
#' `replace_val`
#'
#' @keywords internal
calc_val <- function(vec, length_out, replace_val = 0) {
  if (length(vec) == 1 &&
    vec == deparse(substitute(vec))) {
    vec <- NA
  }

  if (all_missing(vec)) vec <- replace_val
  if (any_missing(vec)) vec[which(is_missing(vec))] <- replace_val

  if (length(vec) == 1) vec <- rep(vec, length_out)
  vec
}
