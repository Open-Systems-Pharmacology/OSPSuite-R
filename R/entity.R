
#' @title ObjectBase
#' @docType class
#' @description  Abstract wrapper for an OSPSuite.Core ObjectBase.
#'
#' @field id The id of the .NET wrapped object. (read-only)
#' @field name The name of the object
ObjectBase <- R6Class("ObjectBase",
  inherit = DotNetWrapper,
  active = list(
    name = function(value) {
      private$wrapReadOnlyProperties("Name", value)
    },
    id = function(value) {
      private$wrapReadOnlyProperties("Id", value)
    }
  )
)


#' @title Entity
#' @docType class
#' @description  Abstract wrapper for an OSPSuite.Core Entity class
#'
#' @field path The path of the entity in the container hiearchy. (read-only)
Entity <- R6Class("Entity",
  inherit = ObjectBase,
  active = list(
    path = function(value) {
      private$wrapExtensionMethod("OSPSuite.Core.Domain.EntityExtensions", "EntityPath", "path", value)
    }
  )
)
