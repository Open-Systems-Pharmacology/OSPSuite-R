#' @title Population
#' @docType class
#' @description  List of individuals used in a population simulation
#' @field count the number of individual in the population
#' @section Methods:
#' \describe{
#'   \count{containerType}{Type of container}
#'   }
#'
Population <- R6::R6Class(
  "Population",
  inherit = DotNetWrapper,
  active = list(
    count = function(value) {
      private$wrapReadOnlyProperties("Count", value)
    }
  ),
  public = list(
    print = function(...) {
      private$printClass()
      private$printLine("Number of Individuals", self$count)
      invisible(self)
    }
  )
)
