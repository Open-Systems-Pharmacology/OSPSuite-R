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
      ospsuite.utils::osp_print_class(self)
      ospsuite.utils::osp_print_items(list(
        "Container type" = self$containerType,
        "Path" = self$path
      ),
      print_empty = TRUE)
    }
  )
)
