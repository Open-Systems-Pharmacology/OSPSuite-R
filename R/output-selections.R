#' @title OutputSelections
#' @docType class
#' @description  List of selected quantities selected as output for a given simulation
#'
OutputSelections <- R6::R6Class(
  "OutputSelections",
  cloneable = FALSE,
  inherit = DotNetWrapper,
  active = list(
    #' @field allOutputs Returns all outputs defined in the selection
    allOutputs = function(value) {
      if (missing(value)) {
        .toObjectType(self$get("OutputsAsArray"), QuantitySelection)
      } else {
        stop(messages$errorPropertyReadOnly("allOutputs"))
      }
    }
  ),
  public = list(
    #' @description
    #' Removes all selected output from the selection
    clear = function() {
      self$call("Clear")
    },
    #' @description
    #' Adds a quantity as selected
    #' @param quantity Quantity to add to the selection
    addQuantity = function(quantity) {
      validateIsOfType(quantity, "Quantity")
      self$call("AddQuantity", quantity)
    },
    #' @description
    #' Removes a quantity from the selection
    #' @param quantity Quantity to remove from the selection
    removeQuantity = function(quantity) {
      validateIsOfType(quantity, "Quantity")
      self$call("RemoveQuantity", quantity)
    },
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$.printClass()
      for (output in self$allOutputs) {
        print(output)
      }

      invisible(self)
    }
  )
)
