#'  Validate arguments provided as vectors
#'
#' @details
#'
#' Validation of arguments provided as a vector involves:
#'
#' - Checking that it is of expected length.
#' - Checking for `NULL` or other unexpected values (`NaN`, `Inf`, `NA` of the
#'   wrong type) and standardizing them to `NA` of desired type.
#' - Checking that each element in the vector is of expected type.
#' - If a non-atomic list is provided, converting it to an atomic vector.
#'
#' @param x A vector of arguments.
#' @param expectedLength An integer to denote the expected length of the vector.
#' @inheritParams flattenList
#'
#' @return
#'
#' An atomic vector of desired type containing specified arguments.
#'
#' @examples
#'
#' cleanVectorArgs(list(1, 2, NA, NULL), 4L, "numeric")
#' cleanVectorArgs(c(1, 2, NA, NA_complex), 4L, "numeric")
#' @keywords internal
#' @noRd
cleanVectorArgs <- function(arg = NULL, expectedLength = NULL, type) {
  # return early if argument was not specified
  if (is.null(arg)) {
    return(NULL)
  }

  # validate the length of vector arguments
  if (!is.null(expectedLength)) {
    validateIsOfLength(arg, expectedLength)
  }

  # convert `NULL`s or logical `NA`s to `NA` of required type

  # Note that `purrr::map()` will return a list
  arg <- purrr::map(arg, ~ toMissingOfType(.x, type))

  # validate the type of arguments

  # `nullAllowed = TRUE` is necessary because `NULL` in vector arguments is
  # used to specify no change for the corresponding dataset
  validateIsOfType(arg, type, nullAllowed = TRUE)

  # arguments are still in a list
  # flatten them to an atomic vector of required type
  arg <- flattenList(arg, type)

  return(arg)
}

#' Flatten a list to an atomic vector of desired type
#'
#' @param x A list or an atomic vector. If the latter, no change will be made.
#' @param type Type of atomic vector to be returned.
#'
#' @details
#'
#' The `type` argument will decide which variant from `purrr::flatten()` family
#' is used to flatten the list.
#'
#' @examples
#'
#' flattenList(list(1, 2, 3, NA), type = "numeric")
#' flattenList(list(TRUE, FALSE, NA), type = "integer")
#' @return An atomic vector of desired type.
#'
#' @keywords internal
#' @noRd
flattenList <- function(x, type) {
  if (!is.vector(x)) {
    stop("`x` argument can only be a vector.", call. = FALSE)
  }

  if (is.list(x)) {
    x <- switch(type,
      "character" = purrr::flatten_chr(x),
      "numeric" = ,
      "real" = ,
      "double" = purrr::flatten_dbl(x),
      "integer" = purrr::flatten_int(x),
      "logical" = purrr::flatten_lgl(x),
      purrr::flatten(x)
    )
  }

  return(x)
}


#' Convert `NULL` or `NA`s to `NA` of desired type
#'
#' @param x A single element.
#' @inheritParams flattenList
#'
#' @examples
#'
#' toMissingOfType(NA, type = "real")
#' toMissingOfType(NULL, type = "integer")
#' @keywords internal
#' @noRd
toMissingOfType <- function(x, type) {
  # all unexpected values will be converted to `NA` of a desired type
  if (is.null(x) || is.na(x) || is.nan(x) || is.infinite(x)) {
    x <- switch(type,
      "character" = NA_character_,
      "numeric" = ,
      "real" = ,
      "double" = NA_real_,
      "integer" = NA_integer_,
      "complex" = NA_complex_,
      "logical" = NA,
      stop("Incorrect type entered.")
    )
  }

  return(x)
}
