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
    containerTypeInt = function(value) {
      private$wrapReadOnlyProperties("ContainerType", value)
    },
    containerTypeStr = function(value){
      names(which(ContainerType == self$containerTypeInt))
    }
  ),
  public = list(
    print = function(...) {
      private$printClass()
      private$printLine("Container type", self$containerTypeStr)
      private$printLine("Path", self$path)
      invisible(self)
    }
  )
)
