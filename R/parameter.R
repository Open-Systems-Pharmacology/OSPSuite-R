#' @title Parameter
#' @docType class
#' @description  A model parameter with a value
#'
#' @field value The value of the parameter
Parameter <- R6Class(
  "Parameter",
  inherit = Entity,
  active = list(
    value = function(value) {
      private$wrapProperties("Value", value)
    }
  ),
  public = list(
    print = function(...) {
      print(paste0("Parameter '", self$path, "' has a value of ", self$value))
      invisible(self)
    }
  )
)
