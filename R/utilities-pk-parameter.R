#' @title Adds a User-Defined PK-Parameter to the managed list of PK-Parameters
#'
#' @param userDefinedPKParameter User defined PK-Parameter to add
#'
#' @examples
#'
#' userDefinedPKParameter <- UserDefinedPKParameter$new(name = "MyAUC", standardPKParameter = StandardPKParameter$AucTend)
#' addUserDefinedPKParameter(userDefinedPKParameter)
#' @export
addUserDefinedPKParameter <- function(userDefinedPKParameter){
  pkParameterTask <- getNetTask("PKParameterTask")
  validateIsOfType(userDefinedPKParameter, UserDefinedPKParameter)
  rClr::clrCall(pkParameterTask, "AddUserDefinedPKParameter", userDefinedPKParameter$ref)
}
