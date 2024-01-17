# These variables are set to NULL to avoid R CMD Check warning
# 'no visible global function definition for ..."

# defining global variables and functions to appease R CMD Check

utils::globalVariables(
  names = c(
    ".",
    ".rowidInternal",
    "Time",
    "dataType",
    "group",
    "name",
    "paths",
    "residualValues",
    "unitFrequency",
    "xValues",
    "yValues",
    "yErrorValues",
    "yValuesObserved",
    "yValuesSimulated"
  ),
  package = "ospsuite",
  add = FALSE
)
