EntityExtensions <- "OSPSuite.Core.Domain.EntityExtensions"

#' @title Entity
#' @docType class
#' @description  Abstract wrapper for an OSPSuite.Core Entity class
#'
#' @format NULL
Entity <- R6::R6Class(
  "Entity",
  cloneable = FALSE,
  inherit = ObjectBase,
  private = list(
    .parentContainer = NULL
  ),
  active = list(
    #' @field path The path of the entity in the container hiearchy without the simulation name. (read-only)
    path = function(value) {
      private$wrapExtensionMethod(EntityExtensions, "ConsolidatedPath", "path", value)
    },
    #' @field fullPath Same as \code{path}, but with the simulation name. (read-only)
    fullPath = function(value) {
      private$wrapExtensionMethod(EntityExtensions, "EntityPath", "fullPath", value)
    },
    #' @field parentContainer Returns a new wrapper instance to the .NET parent container. Multiple call to this method
    #' will always return the same instance. However two children of the same parent will return two different instances of \code{Container}
    #' pointing to the same .NET container
    parentContainer = function(value) {
      if (is.null(private$.parentContainer)) {
        netParentContainer <- private$wrapProperty("ParentContainer")
        if (is.null(netParentContainer)) {
          return(NULL)
        }
        private$.parentContainer <- Container$new(netParentContainer)
      }
      return(private$.parentContainer)
    }
  )
)
