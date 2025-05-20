#' Types of building blocks.
#'
#' MUST WE DEFINE IT HERE, OR CAN WE GET IT FROM .NET SOMEHOW?
#' ARE INDIVIDUAL AND EXPRESSION PROFILE ALSO A BB TYPE?
#'
#' @export
BuildingBlockTypes <- enum(c("Spatial Structure",
                           "Reactions",
                           "Molecules",
                           "Passive Transports",
                           "Observer",
                           "Events",
                           "Initial Conditions",
                           "Parameter Values",
                           "Expression Profile",
                           "Individual"
))


#' @title Building block # ONLY MOBI BBs, or should we consider having PK-Sim BBs as pkml export in the future?
#' @docType class
#' @description  A representation of a building block
#' @format NULL
BuildingBlock <- R6::R6Class(
  "BuildingBlock",
  cloneable = FALSE,
  inherit = ObjectBase,
  active = list(
    #' @field type Type of the building block (Spatial structure, molecules, reactions, etc)
    type = function(value){
      private$.readOnlyProperty("type", value, self$get("type"))
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
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      ospsuite.utils::ospPrintClass(self)
      ospsuite.utils::ospPrintItems(list(
        "Name" = self$name,
        "Type" = self$type
      ))
    }
  ),
  private = list(

  )
)
