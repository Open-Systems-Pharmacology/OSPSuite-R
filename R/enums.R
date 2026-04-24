#' Types of building blocks supported.
#'
#' @export
BuildingBlockTypes <- enum(c(
  "SpatialStructure",
  "Molecules",
  "Reactions",
  "Passive Transports",
  "Observers",
  "EventGroups",
  "Initial Conditions",
  "Parameter Values",
  "Expression Profile",
  "Individual"
))

#' Categories of molecule calculation methods that can be overridden on a
#' per-molecule basis in a simulation configuration.
#'
#' @export
CalculationMethodCategories <- enum(c(
  PartitionCoefficient = "DistributionCellular",
  CellularPermeability = "DiffusionIntCell"
))

#' Available methods for calculation of cellular permeabilities.
#'
#' @export
CellularPermeabilityMethods <- enum(c(
  "PK-Sim Standard" = "Cellular permeability - PK-Sim Standard",
  "Charge dependent Schmitt" = "Cellular permeability - Charge dependent Schmitt",
  "Charge dependent Schmitt normalized to PK-Sim" = "Cellular permeability - Charge dependent Schmitt normalized to PK-Sim"
))

#' How should comparison of entities be performed
#'
#' @export
CompareBy <- enum(c(
  "id",
  "name",
  "path"
))

#' Names of aggregation available for plotPopulationTimeProfile()
#'
#' @export
DataAggregationMethods <- enum(c(
  "quantiles",
  "arithmetic",
  "geometric"
))

#' Supported types of the error
#'
#' @export
DataErrorType <- enum(c(
  "ArithmeticStdDev",
  "GeometricStdDev"
))

#' @title Expression Profile Categories
#' @description Allowed categories for expression profiles
#' @export
ExpressionProfileCategories <- enum(list(
  "Metabolizing Enzyme",
  "Transport Protein",
  "Protein Binding Partner"
))

#' Default genders  defined in PK-Sim
#'
#'
#' @export
Gender <- enum(c(
  Female = "FEMALE",
  Male = "MALE",
  Unknown = "UNKNOWN"
))

#' Default human population defined in PK-Sim
#'
#'
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

#' Available disease states
#'
#' @export
IndividualDiseaseStates <- enum(c(
  "None",
  "Renal impairment",
  "Hepatic impairment"
))

#' Standard molecule parameter names typically available in an endogenous molecule (enzyme, transporter etc...) coming from PK-Sim
#'
#'
#' @export
MoleculeParameter <- enum(c(
  ReferenceConcentration = "Reference concentration",
  THalfLiver = "t1/2 (liver)",
  THalfIntestine = "t1/2 (intestine)",
  OntogenyFactor = "Ontogeny factor",
  OntogenyFactorGI = "Ontogeny factor GI"
))

#' Available methods for calculation of partition coefficients.
#'
#' @export
PartitionCoefficientMethods <- enum(c(
  "PK-Sim Standard" = "Cellular partition coefficient method - PK-Sim Standard",
  "Rodgers and Rowland" = "Cellular partition coefficient method - Rodgers and Rowland",
  "Schmitt" = "Cellular partition coefficient method - Schmitt",
  "Poulin and Theil" = "Cellular partition coefficient method - Poulin and Theil",
  "Berezhkovskiy" = "Cellular partition coefficient method - Berezhkovskiy"
))

#' Default species defined in PK-Sim
#'
#'
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

#' Standard containers typically available in a PBPK simulation
#'
#'
#' @export
StandardContainer <- enum(c(
  Organism = "Organism",
  Applications = "Applications",
  Neighborhoods = "Neighborhoods",
  Events = "Events"
))

#' List of ontogeny supported in PK-Sim
#'
#'
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

#' Standard PK-Parameters types defined in OSPSuite
#' This is only used to defined how a user defined PK Parameter should be calculated
#'
#' @export
StandardPKParameter <- enum(c(
  Unknown = 0,
  C_max = 1,
  C_max_norm = 2,
  C_min = 3,
  C_min_norm = 4,
  t_max = 5,
  t_min = 6,
  C_trough = 7,
  C_trough_norm = 8,
  AUC_tEnd = 9,
  AUC_tEnd_norm = 10,
  AUCM_tEnd = 11,
  AUC_inf = 12,
  AUC_inf_norm = 13,
  AUC_tEnd_inf = 14,
  AUC_tEnd_inf_norm = 15,
  CL = 16,
  MRT = 17,
  FractionAucEndToInf = 18,
  Thalf = 19,
  Vss = 20,
  Vd = 21,
  Tthreshold = 22
))
