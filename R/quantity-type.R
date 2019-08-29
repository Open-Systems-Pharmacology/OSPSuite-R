#' All possible quantity types as defined in OSPSuite
#'
#' @include enum.R
#' @export
QuantityType <- enum(list(
  Undefined = bitwShiftL(2,0),
  Drug  = bitwShiftL(2,1),
  Metabolite = bitwShiftL(2,2),
  Enzyme  = bitwShiftL(2,3),
  Transporter = bitwShiftL(2,4),
  Complex = bitwShiftL(2,5),
  OtherProtein = bitwShiftL(2,6),
  Observer = bitwShiftL(2,7),
  Parameter = bitwShiftL(2,8),
  Time = bitwShiftL(2,9)
))
