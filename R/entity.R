
#' @title ObjectBase
#' @docType class
#' @description  Abstract wrapper for an OSPSuite.Core ObjectBase.
#'
#' @field id The id of the .NET wrapped object. (read-only)
#' @field name The name of the object
#' @format NULL
ObjectBase <- R6::R6Class(
  "ObjectBase",
  inherit = DotNetWrapper,
  active = list(
    name = function(value) {
      private$wrapReadOnlyProperty("Name", value)
    },
    id = function(value) {
      private$wrapReadOnlyProperty("Id", value)
    }
  )
)


#' @title Entity
#' @docType class
#' @description  Abstract wrapper for an OSPSuite.Core Entity class
#'
#' @field path The path of the entity in the container hiearchy. (read-only)
#' @field consolidatePath Same as \code{path}, but without the preceding simulation name. (read-only)
Entity <- R6::R6Class(
  "Entity",
  inherit = ObjectBase,
  active = list(
    path = function(value) {
      private$wrapExtensionMethod("OSPSuite.Core.Domain.EntityExtensions", "EntityPath", "path", value)
    },
    consolidatePath = function(value) {
      private$wrapExtensionMethod("OSPSuite.Core.Domain.EntityExtensions", "ConsolidatedPath", "consolidatePath", value)
    }
  )
)
