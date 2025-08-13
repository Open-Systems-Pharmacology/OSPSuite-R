#' Types of building blocks.
#'
#' MUST WE DEFINE IT HERE, OR CAN WE GET IT FROM .NET SOMEHOW?
#' ARE INDIVIDUAL AND EXPRESSION PROFILE ALSO A BB TYPE?
#'
#' @export
BuildingBlockTypes <- enum(c(
  "Spatial Structure",
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
    type = function(value) {
      private$.readOnlyProperty("type", value, private$.type)
    },
    #' @field id ID of the building block
    id = function(value) {
      private$.readOnlyProperty("type", value, self$get("Id"))
    },
    #' @field name Name of the building block. Read-only.
    name = function(value) {
      private$.readOnlyProperty("name", value, self$get("Name"))
    }
  ),
  public = list(
    #' @description
    #' Initialize a new instance of the class
    #'
    #' @param netObject Reference to `NetObject` .NET object representing a building block.
    #' @param type Type of the building block (optional, defaults to `NULL`).
    #' Must be one of the `BuildingBlockTypes`.
    #' @return A new `BuildingBlock` object.
    initialize = function(netObject, type = NULL) {
      validateEnumValue(type, enum = BuildingBlockTypes, nullAllowed = TRUE)
      if (!is.null(type) && !type %in% BuildingBlockTypes) {
        stop(messages$invalidBuildingBlockType(type), call. = FALSE)
      }

      super$initialize(netObject)
        private$.type <- type
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
    .type = NULL)
)
