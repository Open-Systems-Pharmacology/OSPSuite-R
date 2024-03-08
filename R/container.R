#' @title Container
#' @docType class
#' @description  Contains other entities such as Parameter or containers
#' @format NULL
#' @keywords internal
Container <- R6::R6Class(
  "Container",
  cloneable = FALSE,
  inherit = Entity,
  active = list(
    #' @field containerType Type of container
    containerType = function(value) {
      private$.wrapReadOnlyProperty("ContainerTypeAsString", value)
    }
  ),
  public = list(
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$.printClass()
      private$.printLine("Container type", self$containerType)
      private$.printLine("Path", self$path)
      invisible(self)
    }
  )
)
