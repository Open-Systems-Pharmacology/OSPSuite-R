# Introspect Test_Project.mbp3 structure for test-authoring reference.
# Run from the package root:
#   Rscript tests/data/MoBiProject/introspect_test_project.R
# Output is plain text; pipe it to a file if you want to update
# `tests/data/MoBiProject/Test_Project.md`.

devtools::load_all(".", quiet = TRUE)

options(width = 200)

projectPath <- "tests/data/MoBiProject/Test_Project.mbp3"
if (!file.exists(projectPath)) {
  projectPath <- file.path(
    dirname(sys.frame(1)$ofile %||% "."),
    "Test_Project.mbp3"
  )
}
project <- loadMoBiProject(projectPath)

printHeader <- function(title) {
  cat("\n\n=== ", title, " ===\n", sep = "")
}

printHeader("MODULE NAMES")
print(project$moduleNames)

printHeader("INDIVIDUAL NAMES")
print(project$individualNames)

printHeader("EXPRESSION PROFILE NAMES")
print(project$expressionProfilesNames)

for (moduleName in project$moduleNames) {
  module <- project$getModules(moduleName)[[1]]
  printHeader(paste0("MODULE: '", moduleName, "'"))
  cat("isPKSimModule: ", module$isPKSimModule, "\n", sep = "")
  cat("mergeBehavior: ", module$mergeBehavior, "\n", sep = "")
  cat(
    "PV BB names: ",
    paste(module$parameterValuesBBnames, collapse = ", "),
    "\n",
    sep = ""
  )
  cat(
    "IC BB names: ",
    paste(module$initialConditionsBBnames, collapse = ", "),
    "\n",
    sep = ""
  )

  pvBBs <- tryCatch(module$getParameterValuesBBs(), error = function(e) list())
  for (bbName in names(pvBBs)) {
    cat("\n-- PV BB '", bbName, "' --\n", sep = "")
    df <- parameterValuesBBToDataFrame(pvBBs[[bbName]])
    cat("  rows: ", nrow(df), "\n", sep = "")
    if (nrow(df) > 0) print(utils::head(df, 30))
  }

  icBBs <- tryCatch(module$getInitialConditionsBBs(), error = function(e) {
    list()
  })
  for (bbName in names(icBBs)) {
    cat("\n-- IC BB '", bbName, "' --\n", sep = "")
    df <- initialConditionsBBToDataFrame(icBBs[[bbName]])
    cat("  rows: ", nrow(df), "\n", sep = "")
    if (nrow(df) > 0) print(utils::head(df, 30))
  }
}

printHeader("INDIVIDUALS")
for (indName in project$individualNames) {
  cat("\n-- Individual '", indName, "' --\n", sep = "")
  ind <- tryCatch(project$getIndividual(indName), error = function(e) NULL)
  if (!is.null(ind)) {
    df <- tryCatch(individualsBBToDataFrame(ind), error = function(e) NULL)
    if (!is.null(df)) {
      cat("  rows: ", nrow(df), "\n", sep = "")
      print(utils::head(df, 20))
    } else {
      cat("  (could not dataframe-ify)\n")
    }
  }
}

printHeader("EXPRESSION PROFILES")
for (epName in project$expressionProfilesNames) {
  cat("\n-- ExpressionProfile '", epName, "' --\n", sep = "")
  bb <- tryCatch(
    project$getExpressionProfiles(epName)[[1]],
    error = function(e) NULL
  )
  if (!is.null(bb)) {
    df <- tryCatch(expressionProfileBBToDataFrame(bb), error = function(e) NULL)
    if (!is.null(df)) {
      cat(
        "  expressionParameters rows: ",
        nrow(df$expressionParameters),
        "\n",
        sep = ""
      )
      cat(
        "  initialConditions rows: ",
        nrow(df$initialConditions),
        "\n",
        sep = ""
      )
      cat(
        "  unique molecules in initialConditions: ",
        paste(unique(df$initialConditions$`Molecule Name`), collapse = ", "),
        "\n",
        sep = ""
      )
      organs <- unique(sapply(
        strsplit(df$initialConditions$`Container Path`, "\\|"),
        `[`,
        2
      ))
      cat(
        "  top-level organs (level-2 containers under 'Organism'): ",
        paste(organs, collapse = ", "),
        "\n",
        sep = ""
      )
      compartments <- unique(sapply(
        strsplit(df$initialConditions$`Container Path`, "\\|"),
        `[`,
        3
      ))
      cat(
        "  distinct sub-compartments (level-3): ",
        paste(compartments, collapse = ", "),
        "\n",
        sep = ""
      )
      paramNames <- unique(df$expressionParameters$`Parameter Name`)
      cat(
        "  expression parameter name set: ",
        paste(paramNames, collapse = ", "),
        "\n",
        sep = ""
      )
    }
  }
}
