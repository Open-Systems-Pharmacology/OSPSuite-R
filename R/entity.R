EntityExtensions <- "OSPSuite.Core.Domain.EntityExtensions"


#' @title ObjectBase
#' @docType class
#' @description  Abstract wrapper for an OSPSuite.Core ObjectBase.
#'
#' @format NULL
ObjectBase <- R6::R6Class(
  "ObjectBase",
  cloneable = FALSE,
  inherit = DotNetWrapper,
  active = list(
    #' @field name The name of the object
    name = function(value) {
      private$wrapReadOnlyProperty("Name", value)
    },
    #' @field id The id of the .NET wrapped object. (read-only)
    id = function(value) {
      private$wrapReadOnlyProperty("Id", value)
    }
  )
)


#' @title Entity
#' @docType class
#' @description  Abstract wrapper for an OSPSuite.Core Entity class
#'
#' @format NULL
Entity <- R6::R6Class(
  "Entity",
  cloneable = FALSE,
  inherit = ObjectBase,
  active = list(
    #' @field path The path of the entity in the container hiearchy without the simulation name. (read-only)
    path = function(value) {
      private$wrapExtensionMethod(EntityExtensions, "ConsolidatedPath", "path", value)
    },
    #' @field fullPath Same as \code{path}, but with the simulation name. (read-only)
    fullPath = function(value) {
      private$wrapExtensionMethod(EntityExtensions, "EntityPath", "fullPath", value)
    }
  )
)
