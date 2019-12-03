#' @title Parameter
#'
#' @description A model parameter
#'
#' @details  Derived from \link{Quantity}, please see base class documentation.
#'
#' @docType class
#' @name Parameter
#'
#' @keywords Parameter
#' @format NULL
Parameter <- R6::R6Class(
  "Parameter",
  inherit = Quantity,
  public = list(
    print = function(...) {
      private$printQuantity()
    }
  )
)
