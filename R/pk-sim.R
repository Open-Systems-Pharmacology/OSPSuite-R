
#' Default species defined in PK-Sim
#'
#' @include enum.R
#' @export
Species <- enum(c(
  "Beagle",
  "Dog",
  "Human",
  "Minipig",
  "Monkey",
  "Mouse",
  "Rabbit",
  "Rat"
))

#' Default human population defined in PK-Sim
#'
#' @include enum.R
#' @export
HumanPopulation <- enum(c(
  "Asian_Tanaka_1996",
  "BlackAmerican_NHANES_1997",
  "European_ICRP_2002",
  "Japanese_Population",
  "MexicanAmericanWhite_NHANES_1997",
  "Pregnant",
  "Preterm",
  "WhiteAmerican_NHANES_1997"
))

#' Default genders  defined in PK-Sim
#'
#' @include enum.R
#' @export
Gender <- enum(c(
  Female = "FEMALE",
  Male = "MALE",
  Unknown = "UNKNOWN"
))

#' List of ontogeny supported in PK-Sim
#'
#' @include enum.R
#' @export
StandardOntogeny <- enum(c(
  "CYP1A2",
  "CYP2C18",
  "CYP2C19",
  "CYP2C8",
  "CYP2C9",
  "CYP2D6",
  "CYP2E1",
  "CYP3A4",
  "CYP3A5",
  "CYP3A7",
  "UGT1A1",
  "UGT1A4",
  "UGT1A6",
  "UGT1A9",
  "UGT2B4",
  "UGT2B7"
))
