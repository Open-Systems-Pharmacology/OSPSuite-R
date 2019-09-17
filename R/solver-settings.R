#' @title SolverSettings
#' @docType class
#' @description  Solver settings associated with a given simulation
#'
#' @field useJacobian Use of Jacobian matrix during calculations
#' @field h0 Initial time step size
#' @field hMin Minimum absolute value of step size allowed
#' @field hMax Maximum absolute value of step size allowed
#' @field mxStep Maximum number of internal steps to be taken by the solver in its attempt to reach tout
#' @field relTol Relative tolerance of unknowns
#' @field absTol Absolute tolerance of unknowns
SolverSettings <- R6::R6Class(
  "SolverSettings",
  inherit = DotNetWrapper,
  active = list(
    useJacobian = function(value) {
      private$wrapProperty("UseJacobian", value)
    },

    h0 = function(value) {
      private$wrapProperty("H0", value)
    },

    hMin = function(value) {
      private$wrapProperty("HMin", value)
    },

    hMax = function(value) {
      private$wrapProperty("HMax", value)
    },

    mxStep = function(value) {
      # cannot use base property because of cast to integer
      if (missing(value)) {
        rClr::clrGet(self$ref, "MxStep")
      } else {
        rClr::clrSet(self$ref, "MxStep", as.integer(value))
      }
    },

    relTol = function(value) {
      private$wrapProperty("RelTol", value)
    },

    absTol = function(value) {
      private$wrapProperty("AbsTol", value)
    }
  ),
  public = list(
    print = function(...) {
      private$printClass()
      private$printLine("useJacobian", self$useJacobian)
      private$printLine("h0", self$h0)
      private$printLine("hMin", self$hMin)
      private$printLine("hMax", self$hMax)
      private$printLine("mxStep", self$mxStep)
      private$printLine("relTol", self$relTol)
      private$printLine("absTol", self$absTol)
      invisible(self)
    }
  )
)
