#' @title MoBi Module # OR SHOULD WE CALL IT AN OspModule ?
#' @docType class
#' @description  A MoBi module, either loaded from a project or from a pkml file
#' @format NULL
MoBiModule <- R6::R6Class(
  "MoBiModule",
  cloneable = FALSE,
  inherit = ObjectBase,
  active = list(
    mergeBehavior = function(value) {
      if (missing(value)) {
        return(self$get("MergeBehavior"))
      } else {
        # Check that the provided merge behavior is either "Extend" or "Overwrite".
        self$set("MergeBehavior", value)
      }
    }
  ),
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #'
    #' @param netObject Reference to `NetObject` .NET MoBi-module object
    #' @return A new `MoBiModule` object.
    initialize = function(netObject) {
      super$initialize(netObject)
    },

    #' @description
    #' Get the building blocks from the module.
    #'
    #' @param type Optional, type of building blocks to return. Must be one of `BuildingBlockTypes`.
    #' If `NULL` (default), all building blocks of the module are returned.
    #' @return A named list of building block objects.
    getBuildingBlocks = function(type = NULL) {

    },

    #' @description
    #' Is this module a PK-Sim module?
    #'
    isPkSimModule = function() {
    },
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      ospsuite.utils::ospPrintClass(self)
      ospsuite.utils::ospPrintItems(list(
        "Name" = self$name,
        "PK-Sim module" = self$isPkSimModule()
      ))
    }
  ),
  private = list()
)
