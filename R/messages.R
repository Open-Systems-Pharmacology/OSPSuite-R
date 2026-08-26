#' List of functions and strings used to signal error messages
#' Extends the `messages` list from ospsuite.utils
messages <- ospsuite.utils::messages

messages$errorFileDoesNotExist <- function(path) {
  paste0("File does not exist: ", path)
}

messages$errorPkmlContainsMultipleModules <- function() {
  paste(
    "The PKML you are trying to load the module from contains more than one",
    "module, but the function expects only one module.",
    "Most probably you are trying to load a simulation export."
  )
}

messages$errorWrongPopulation <- function(species, population) {
  cliFormat(
    "Could not find {.field population} {.val {population}} for {.field species} {.val {species}}"
  )
}

messages$errorOneOfNameAndPathMustBeSpecified <- function() {
  cliFormat(
    "One of the {.field parameterName} or {.field parameterPath} must be specified, but not both."
  )
}

messages$errorMissingColumns <- function(cols) {
  cliFormat(
    "{length(cols)} required column{?s} {?is/are} missing from the data.frame: {.val {cols}}"
  )
}

messages$errorInvalidDataSetNames <- function() {
  cliFormat(
    "The {.field name} column must not contain NA or empty string values."
  )
}

messages$noDatasetsToGroup <- function() {
  cliFormat(
    "There are currently no datasets to be grouped.",
    "You can add them with {.fun $addDataSets} and/or {.fun $addSimulationResults} methods."
  )
}

messages$datasetsToGroupNotFound <- function() {
  cliFormat("Following datasets were specified to be grouped but not found")
}

messages$unpairableDatasetsRemoved <- function() {
  cliFormat("Following non-grouped or unpairable datasets have been removed")
}

messages$printMultipleEntries <- function(header, entries) {
  message(cliFormat("{.emph {header}}:", paste("-", entries, collapse = "\n")))
}

messages$errorLoadingUnitsForDimension <- function(dimensions) {
  header <- cliFormat(
    "Could not load {.field units} for {length(dimensions)} {.field dimension{?s}}"
  )
  messages$printMultipleEntries(header, dimensions)
}

messages$residualsCanNotBeComputed <- function() {
  cliFormat(
    "No residuals can be computed because the entered {.field DataCombined} object does not contain any observed-simulated datasets that can be paired."
  )
}

messages$residualsColumnNotFound <- function(columnName) {
  cliFormat("Column {.val {columnName}} not found in pairedData.")
}

messages$residualsLogNonPositive <- function(n) {
  cliFormat(
    paste(
      "{.val {n}} residual value{?s} could not be computed because the observed or predicted",
      "value is zero or negative (log of non-positive values is undefined)."
    ),
    "These data points are set to NaN and excluded from the output."
  )
}

messages$residualsRatioPredNonPositive <- function(n) {
  cliFormat(
    paste(
      "{.val {n}} residual value{?s} could not be computed because the predicted value is",
      "zero or negative (division by zero or undefined denominator)."
    ),
    "These data points are set to NaN and excluded from the output."
  )
}

messages$logScaleNotAllowed <- function() {
  cliFormat(
    "The Y-axis for this plot should {.strong not} be on a log scale, since the {.field residuals} are expected to be centered around 0."
  )
}

messages$lloqOnlyScalar <- function() {
  cliFormat(
    "Only {.strong one} LLOQ value per {.field DataSet} is supported!",
    "Please provide a scalar value and not a vector."
  )
}

messages$simBatchStartValueNaN <- function(entityPaths) {
  cliFormat(
    "Start values of the entities with paths {.val {entityPaths}} is {.val NaN}!",
    "Cannot add such run values set"
  )
}

messages$DataFrameNameAlreadyUsed <- function(DataFrameName) {
  warning(cliFormat(
    "{length(DataFrameName)} name{?s} already {?exists/exist} in {.field DataCombined}: {.val {DataFrameName}}.",
    "Existing data will be overwritten."
  ))
}

messages$wrongUnitForQuantity <- function(quantityPath, unit, dimension) {
  cliFormat(paste(
    "Unit {.val {unit}} is {.strong not} valid for",
    "quantity with path {.code {quantityPath}} and dimension {.val {dimension}}"
  ))
}

messages$invalidDataType <- function(name, dataType) {
  cliFormat(
    "Data type {.val {dataType}} specified for data set {.val {name}} is {.strong not} valid.",
    "Valid data types are: {.val simulated} or {.val observed}."
  )
}

messages$valueNotPositive <- function(value, propertyName) {
  cliFormat(
    "The value of {.field {propertyName}} must be > 0, but it is {.val {value}}"
  )
}

messages$yErrorValuesNegative <- function(n) {
  cliFormat(
    "{.val {n}} negative {.field yErrorValue{?s}} found.",
    "Negative values have been set to {.val NA}."
  )
}

messages$molWeightErrorMessage <- function(quantityPath) {
  cliFormat(
    "Unable to retrieve the molecular weight for: {.code {quantityPath}}"
  )
}

messages$illegalCharactersInName <- function(name) {
  illegalChars <- .getIllegalCharacters()
  cliFormat(
    "The name {.val {name}} contains illegal characters.",
    "Illegal characters are: {.val {illegalChars}}."
  )
}

messages$forbiddenSimulationName <- function(name, sim) {
  forbiddenNames <- .getIllegalSimulationNames(sim)
  cliFormat(
    "The name {.val {name}} is {.strong not} allowed for this simulation.",
    "Forbidden names for this simulation are: {.val {forbiddenNames}}."
  )
}

messages$plotNoDataAvailable <- function() {
  "No data for this plot available."
}

messages$plotUnitConsistency <- function() {
  "Units have to be consistent within one datatype."
}


messages$plotMissingColumnPredicted <- function() {
  cliFormat(
    "No column available for {.val predicted}.",
    "Please use combinedData format or a data.frame with column {.val predicted}."
  )
}


messages$plotTooManyYDimension <- function(yDimensions) {
  cliFormat(
    "Data contains too many yDimensions: {.val {yDimensions}}.",
    "Automatic y-Unit conversion failed."
  )
}

messages$plotWrongColumnsForCustomErrorType <- function(errorTypes) {
  uniqueTypes <- unique(errorTypes)
  cliFormat(
    "The error values for custom errorTypes {.val {uniqueTypes}} must be provided in {.val yMin} and {.val yMax} columns.",
    "Only {.val ArithmeticStdDev} and {.val GeometricStdDev} can use {.val yErrorValues}."
  )
}

messages$plotShowLegendPerDatasetHasNoEffect <- function(dataType) {
  cliFormat(
    "{.code showLegendPerDataset = {.val {dataType}}} but no {.val {dataType}} data present.",
    "This setting will have no effect."
  )
}


messages$plotUntypicalAesthetic <- function(aesthetic, dataType) {
  otherDataType <- setdiff(c("simulated", "observed"), dataType)
  cliFormat(
    "Aesthetic {.val {aesthetic}} is set to mapping for {.val {dataType}} data.",
    "This aesthetic is usually only used for {.val {otherDataType}} data mapping."
  )
}


messages$errorParameterValuesCountMismatch <- function(
  parameterPath,
  expectedCount,
  actualCount
) {
  cliFormat(
    "Parameter values for {.code {parameterPath}} does not have the expected number of elements.",
    "(Expected {.val {expectedCount}} vs Actual {.val {actualCount}})"
  )
}

messages$errorExportResultsOnlyOneObject <- function() {
  cliFormat(
    "Only {.strong one} {.field SimulationResults} object is allowed.",
    "Lists of results are not supported."
  )
}

messages$errorEmptyOutputSelections <- function(simulationName) {
  cliFormat(
    "The simulation {.val {simulationName}} has no output selections defined.",
    "Please add outputs using {.fun addOutputs} or {.fun setOutputs}."
  )
}

messages$errorIndividualIdsNotFoundInPopulation <- function(missingIds) {
  cliFormat(
    "{length(missingIds)} individual id{?s} {?was/were} not found in the population: {.val {missingIds}}"
  )
}

##### MoBiProject#####
messages$modulesNotPresentInProject <- function(modules) {
  paste0(
    "Modules with the name(s) ",
    paste(modules, collapse = ", "),
    " is/are not present in the project!"
  )
}

messages$errorSimulationNotFound <- function(simulationName) {
  paste0(
    "Simulation with the name '",
    simulationName,
    "' is not present in the project!"
  )
}

messages$errorDataSetsNotPresentInProject <- function(dataSetNames) {
  paste0(
    "Data set(s) with the name(s) ",
    paste(dataSetNames, collapse = ", "),
    " is/are not present in the project!"
  )
}

##### BuildingBlock #####
messages$errorExpressionProfileNotFound <- function(names) {
  paste0(
    "Expression profile(s) with the name(s) ",
    paste(names, collapse = ", "),
    " is/are not present in the project!"
  )
}

messages$errorIndividualNotFound <- function(name) {
  paste0("Individual with the name ", name, " is not present in the project!")
}

messages$errorMissingRequiredBBs <- function(missing) {
  paste0(
    "The provided modules do not contain the required building blocks: ",
    paste(missing, collapse = ", "),
    ". Please provide modules with the required building blocks."
  )
}

messages$errorWrongBuildingBlockType <- function(
  bbName,
  expectedType,
  actualType
) {
  paste0(
    "Building Block with the name '",
    bbName,
    "' is of type '",
    actualType,
    "', but expected type is '",
    expectedType,
    "'."
  )
}

messages$errorICNotFoundInModule <- function(icName, moduleName) {
  paste0(
    "Initial Condition Building Block with the name '",
    icName,
    "' is not present in the module '",
    moduleName,
    "'."
  )
}

messages$errorPVNotFoundInModule <- function(pvName, moduleName) {
  paste0(
    "Parameter Values Building Block with the name '",
    pvName,
    "' is not present in the module '",
    moduleName,
    "'."
  )
}

messages$errorBBTypeAutoDetectFailed <- function(filePath) {
  paste0(
    "Could not auto-detect the building block type from file '",
    filePath,
    "'. Specify the `type` argument explicitly using one of `BuildingBlockTypes$...`."
  )
}

messages$errorBBLoadFromPKMLFailed <- function(filePath, type, cause) {
  paste0(
    "Failed to load a '",
    type,
    "' building block from file '",
    filePath,
    "'. ",
    cause
  )
}

##### Simulation #####
messages$errorFeatureNotSupportedBySimulation <- function(
  featureName,
  version,
  requiredVersion
) {
  paste0(
    "The feature '",
    featureName,
    "' is not supported by this simulation. The simulation was created with OSP version ",
    version,
    ". Minimal required OSP version is ",
    requiredVersion
  )
}

##### SimulationConfiguration #####
messages$errorExpressionProfileAlreadyDefined <- function(
  profileName,
  proteinName
) {
  paste0(
    "Expression for the protein '",
    proteinName,
    "' has already been defined for this simulation configuration with the expression profile '",
    profileName,
    "'."
  )
}

messages$errorOnlyOneIndividualPerConfiguration <- function() {
  "Only one individual can be assigned to a simulation configuration."
}

messages$errorDuplicateModuleNames <- function(duplicateNames) {
  paste0(
    "Module names must be unique. The following name(s) appear more than once: ",
    paste0("'", unique(duplicateNames), "'", collapse = ", "),
    "."
  )
}

messages$errorModuleNotInConfiguration <- function(moduleName) {
  paste0(
    "Module(s) with the name(s) '",
    moduleName,
    "' is not part of the simulation configuration."
  )
}

messages$errorInvalidCalculationMethod <- function(methodName, validMethods) {
  paste0(
    "'",
    methodName,
    "' is not a valid calculation method. Valid methods are: ",
    paste0("'", validMethods, "'", collapse = ", ")
  )
}

messages$errorUnsupportedMacArchitecture <- function(machine) {
  cliFormat(
    "Unsupported architecture for macOS: {.val {machine}}.",
    "Only {.val arm64} (Apple Silicon) is supported."
  )
}

messages$runtimeNotInitialised <- function(loadError = NULL) {
  cliFormat(
    "The OSPSuite .NET runtime could not be initialised.",
    "ospsuite is installed, but calls into the .NET API will fail until a working runtime is available.",
    if (!is.null(loadError)) "Details: {loadError}"
  )
}
