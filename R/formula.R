FormulaExtensions <- "OSPSuite.Core.Domain.Formulas.FormulaExtensions"

#' @title Formula
#' @docType class
#' @description  A formula of the model (Typically related to a `Quantity` such as a parameter)
#' @format NULL
Formula <- R6::R6Class(
  "Formula",
  cloneable = FALSE,
  inherit = ObjectBase,
  active = list(
    #' @field isTable Is this a table formula (Read-Only)
    isTable = function(value) {
      private$wrapExtensionMethod(FormulaExtensions, "IsTable", "isTable", value)
    },
    #' @field isTableWithOffSet Is this a table formula with Offset (Read-Only)
    isTableWithOffSet = function(value) {
      private$wrapExtensionMethod(FormulaExtensions, "IsTableWithOffSet", "isTableWithOffSet", value)
    },
    #' @field isTableWithXArgument Is this a table formula with xArgs (typically time, or pH) (Read-Only)
    isTableWithXArgument = function(value) {
      private$wrapExtensionMethod(FormulaExtensions, "IsTableWithXArgument", "isTableWithXArgument", value)
    },
    #' @field isConstant Is this a constant formula (Read-Only)
    isConstant = function(value) {
      private$wrapExtensionMethod(FormulaExtensions, "IsConstant", "isConstant", value)
    },
    #' @field isExplicit Is this an explicit formula (Read-Only)
    isExplicit = function(value) {
      private$wrapExtensionMethod(FormulaExtensions, "IsExplicit", "isExplicit", value)
    },
    #' @field isDistributed Is this a distributed formula (Read-Only)
    isDistributed = function(value) {
      private$wrapExtensionMethod(FormulaExtensions, "IsDistributed", "isDistributed", value)
    },
    #' @field dimension The dimension in which the quantity is defined  (Read-Only)
    dimension = function(value) {
      private$wrapExtensionMethod(WITH_DIMENSION_EXTENSION, "DimensionName", "dimension", value)
    },
    #' @field formulaString Returns the formula as a string for an `ExplicitFormula` or `NULL` otherwise (Read-Only).
    formulaString = function(value) {
      if (missing(value)) {
        if (self$isExplicit) {
          return(rClr::clrGet(self$ref, "FormulaString"))
        }
        return(NULL)
      } else {
        private$throwPropertyIsReadonly("formulaString")
      }
    }
  ),
  public = list(
    #' @description
    #' Print the object to the console
    #' @param ... Rest arguments.
    print = function(...) {
      private$printClass()
      self$printFormula()
    },
    #' @description
    #' Print the formula to the console without the name of the class
    printFormula = function() {
      if (self$isConstant) {
        private$printLine("isConstant", TRUE)
      } else if (self$isExplicit) {
        private$printLine("isFormula", TRUE)
        private$printLine("formula", self$formulaString)
      } else if (self$isTable) {
        private$printLine("isTable", TRUE)
      } else if (self$isDistributed) {
        private$printLine("isDistributed", TRUE)
      } else if (self$isTableWithOffSet) {
        private$printLine("isTableWithOffSet", TRUE)
      } else if (self$isTableWithXArgument) {
        private$printLine("isTableWithXArgument", TRUE)
      }
      invisible(self)
    }
  )
)
