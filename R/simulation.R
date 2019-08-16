#' @title Simulation
#' @docType class
#' @description  An OSPSuite simulation
#'
#' @field root The root container of the simulation (read-only)
Simulation <- R6Class(
  "Simulation",
  inherit = ObjectBase,
  active = list(
    root = function(value) {
      if (missing(value)) {
        model <- clrGet(self$ref, "Model")
        root <- clrGet(model, "Root")
        Container$new(root)
      } else {
        stop(messages$errorPropertyReadOnly("root"), call. = FALSE)
      }
    },
    path = function(value) {
      if (missing(value)) {
        return(self$root$path)
      } else {
        stop(messages$errorPropertyReadOnly("path"), call. = FALSE)
      }
    }
  )
)
