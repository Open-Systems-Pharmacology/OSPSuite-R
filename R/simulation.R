#' @title Simulation
#' @docType class
#' @description  An OSPSuite simulation
#'
#' @field root The rot container of the simulation (read-only)
Simulation <- R6Class(
  "Simulation",
  inherit = ObjectBase,
  active = list(
    root = function(value) {
      if (missing(value)) {
        model <- rClr::clrGet(self$ref, "Model")
        root <- rClr::clrGet(model, "Root")
        Container$new(root)
      } else {
        stop("Property '$root' is readonly", call. = FALSE)
      }
    },
    path = function(value) {
      if (missing(value)) {
        return(self$root$path)
      } else {
        stop("Property '$path' is readonly", call. = FALSE)
      }
    }
  )
)
