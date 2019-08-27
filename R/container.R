#' @title Container
#' @docType class
#' @description  Contains other entities such as Parameter or containers
#' @section Methods:
#' \describe{
#'   \item{containerType}{Type of container}
#'   }
#'
Container <- R6Class("Container",
  inherit = Entity,
  active = list(
    containerType = function(value) {
      private$wrapReadOnlyProperties("ContainerType", value)
    }
  ),
  public = list(
    print = function(...) {
      private$printClass()
      private$printLine("Container type", getEnumKey(ContainerType, self$containerType))
      private$printLine("Path", self$path)
      invisible(self)
    }
  )
)
