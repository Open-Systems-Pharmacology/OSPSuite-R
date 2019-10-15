#' @title OutputSelections
#' @docType class
#' @description  List of selected quantities selected as output for a given simulation
#'
OutputSelections <- R6::R6Class(
  "OutputSelections",
  inherit = DotNetWrapper,
  active = list(
    allOutputs = function(value) {
      if (missing(value)) {
        toObjectType(rClr::clrGet(self$ref, "OutputsAsArray"), QuantitySelection)
      } else {
        stop(messages$errorPropertyReadOnly("allOutputs"), call. = FALSE)
      }
    }
  ),
  public = list(
    clear = function() {
      rClr::clrCall(self$ref, "Clear")
    },
    addQuantity = function(quantity) {
      validateIsOfType(quantity, Quantity)
      rClr::clrCall(self$ref, "AddQuantity", quantity$ref)
    },
    removeQuantity = function(value) {
      validateIsOfType(quantity, Quantity)
      rClr::clrCall(self$ref, "RemoveQuantity", quantity$ref)
    },
    print = function(...) {
      private$printClass()
      for (output in self$allOutputs) {
        print(output)
      }

      invisible(self)
    }
  )
)
