#' @title OutputSelections
#' @docType class
#' @description  List of selected quantities selected as output for a given simulation
#'
OutputSelections <- R6::R6Class(
  "OutputSelections",
  inherit = DotNetWrapper,
  active = list(
    #' @field allOutputs Returns all outputs defined in the selection
    allOutputs = function(value) {
      if (missing(value)) {
        toObjectType(rClr::clrGet(self$ref, "OutputsAsArray"), QuantitySelection)
      } else {
        stop(messages$errorPropertyReadOnly("allOutputs"), call. = FALSE)
      }
    }
  ),
  public = list(
    #' @description
    #' Removes all selected output from the selection
    clear = function() {
      rClr::clrCall(self$ref, "Clear")
    },
    #' @description
    #' Adds a quantity as selected
    #' @param quantity Quantity to add to the selection
    addQuantity = function(quantity) {
      validateIsOfType(quantity, Quantity)
      rClr::clrCall(self$ref, "AddQuantity", quantity$ref)
    },
    #' @description
    #' Removes a quantity from the selection
    #' @param quantity Quantity to remove from the selection
    removeQuantity = function(quantity) {
      validateIsOfType(quantity, Quantity)
      rClr::clrCall(self$ref, "RemoveQuantity", quantity$ref)
    },
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$printClass()
      for (output in self$allOutputs) {
        print(output)
      }

      invisible(self)
    }
  )
)
