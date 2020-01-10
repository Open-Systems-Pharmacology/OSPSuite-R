
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
