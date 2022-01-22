# TODO: move these functions to the utilities package

#' Count number of objects
#'
#' @details
#'
#' If the argument is not a vector, unlike `length()`, this function will not
#' count the number of named bindings in an environment, but only the number of
#' instances of a class.
#'
#' For example, `length(mtcars)` will return 11, but `objCount(mtcars)` will
#' return 1.
#'
#' @param x An object (an atomic vector, a list, or instance(s) of a class).
#'
#' @examples
#'
#' objCount(c(1, 2, 3)) # 3
#' objCount(list("a", "b")) # 2
#' objCount(mtcars) # 1
#'
#' @return Integer representing the count of objects.
#'
#' @keywords internal
#' @noRd

objCount <- function(x) {
  # `is.vector()` can handle both atomic vectors and lists, i.e.
  # both `is.vector(c(1, 2))` and `is.vector(list(1, 2))` will be `TRUE`
  if (is.vector(x)) {
    l <- length(x)
  } else {
    l <- length(list(x))
  }

  return(l)
}

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
#' validateVectorArgs(list(1, 2, NA, NULL), 4L, "numeric")
#' validateVectorArgs(c(1, 2, NA, NA_complex), 4L, "numeric")
#'
#' @keywords internal
#' @noRd

validateVectorArgs <- function(arg = NULL, expectedLength = NULL, type) {
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
  # `purrr::walk()` is needed below because validation helper functions
  # are called only for their side effects
  purrr::walk(.x = arg, .f = ~ validateIsOfType(.x, type, nullAllowed = TRUE))

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
#'
#' @return An atomic vector of desired type.
#'
#' @keywords internal
#' @noRd

flattenList <- function(x, type) {
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
#'
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
