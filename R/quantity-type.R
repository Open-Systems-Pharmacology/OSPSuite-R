#' All possible quantity types as defined in OSPSuite
#'
#' @include enum.R
#' @export
QuantityType <- enum(list(
  Undefined = bitwShiftL(2, 0),
  Drug = bitwShiftL(2, 1),
  Metabolite = bitwShiftL(2, 2),
  Enzyme = bitwShiftL(2, 3),
  Transporter = bitwShiftL(2, 4),
  Complex = bitwShiftL(2, 5),
  OtherProtein = bitwShiftL(2, 6),
  Observer = bitwShiftL(2, 7),
  Parameter = bitwShiftL(2, 8),
  Time = bitwShiftL(2, 9)
))


#' getQuantityTypeAsString
#'
#' Convert an integer returned by the .NET class representing the quantity type
#' to the string representation
#' @param quantityInt Integer defining the quantity type as returned by the .NET
#' object
#'
#' @return String representation of the quantity type
#' @export
getQuantityTypeAsString <- function(quantityInt){
  # quantityInt encodes for a quantity type or a combination of quantity types.
  # Every bit in a 32-bit representation of quantityInt represents a certain
  # quantity type, as defined in the enum QuantityType
  # Get the 32-bit representation of quantityInt
  quantityTypes <- intToBits(quantityInt)
  # Get all positions that are set (= 1)
  quantityTypes <- which(as.logical(quantityTypes))
  # Convert to integer to get values from the QuantityType-enum.
  # -2 because QuantityType starts at 2 and not 0
  quantityTypes <- bitwShiftL(2, quantityTypes-2)

  quantityTypes <- lapply(quantityTypes, getEnumKey, QuantityType)
  #Collapse the separate entries to a string separated by a comma
  paste(quantityTypes, collapse = ", ")
}
