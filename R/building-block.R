#' @title ExpressionProfile
#' @docType class
#' @description Wrapper for a PK-Sim expression profile building block.
#' @keywords internal
ExpressionProfile <- R6::R6Class(
  "ExpressionProfile",
  cloneable = FALSE,
  inherit = ObjectBase
)

#' @title Individual
#' @docType class
#' @description Wrapper for a PK-Sim individual building block.
#' @keywords internal
Individual <- R6::R6Class(
  "Individual",
  cloneable = FALSE,
  inherit = ObjectBase
)

#' @title InitialConditions
#' @docType class
#' @description Wrapper for a PK-Sim initial conditions building block.
#' @keywords internal
InitialConditions <- R6::R6Class(
  "InitialConditions",
  cloneable = FALSE,
  inherit = ObjectBase
)

#' @title ParameterValues
#' @docType class
#' @description Wrapper for a PK-Sim parameter values building block.
#' @keywords internal
ParameterValues <- R6::R6Class(
  "ParameterValues",
  cloneable = FALSE,
  inherit = ObjectBase
)
