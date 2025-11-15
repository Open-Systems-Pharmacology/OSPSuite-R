# GitHub Copilot Instructions for OSPSuite-R

> **Note**: Please update this file with new instructions as the project evolves to ensure Copilot provides accurate and context-aware assistance.

## Project Overview

The `{ospsuite}` R package provides functionality for loading, manipulating, and simulating pharmacokinetic/pharmacodynamic (PK/PD) models created in the Open Systems Pharmacology Software tools (PK-Sim and MoBi). This is a binary R package that wraps .NET libraries through the `{rSharp}` package.

**Key characteristics:**
- **Language**: R (version 4.1+)
- **Architecture**: R6 class-based object-oriented design wrapping .NET objects
- **Platforms**: Windows and Linux (Ubuntu)
- **Testing**: testthat (edition 3)
- **Documentation**: roxygen2 with markdown support
- **Dependencies**: Includes binary .NET DLLs, uses rSharp for .NET interoperability

## Project Structure

```
OSPSuite-R/
├── R/                      # R source code (~89 files)
│   ├── *-base.R           # Base classes (ObjectBase, DotNetWrapper, etc.)
│   ├── *.R                # R6 class definitions and functions
│   ├── utilities-*.R      # Helper utilities for various operations
│   └── zzz.R              # Package initialization hooks
├── tests/
│   └── testthat/          # Unit tests (~87 test files)
├── man/                    # Generated documentation (roxygen2)
├── vignettes/             # Package vignettes (~15 .Rmd files)
├── inst/                   # Installed files (DLLs, example data)
├── BinaryFiles/           # Platform-specific .NET binaries
└── .github/               # GitHub workflows and configurations
```

## Architecture Patterns

### R6 Classes and .NET Wrapping

The package extensively uses R6 classes to wrap .NET objects:

1. **Base hierarchy**:
   - `NetObject` - Base class for all .NET object wrappers
   - `DotNetWrapper` - Extends NetObject with common wrapper methods
   - `ObjectBase` - Extends DotNetWrapper for OSPSuite.Core objects

2. **Domain classes**: Most classes inherit from `ObjectBase` or `DotNetWrapper`:
   - `Simulation`, `Container`, `Parameter`, `Quantity`, `Molecule`
   - `DataSet`, `DataRepository`, `DataColumn`
   - `Population`, `IndividualCharacteristics`

3. **Key patterns**:
   - Use `R6::R6Class()` with `cloneable = FALSE` for .NET-backed objects
   - Active bindings for properties (read-only and read-write)
   - `self$ref` or `self$pointer` to access underlying .NET object
   - `rSharp::` functions for .NET interoperability

### Example R6 Class Structure

```r
ClassName <- R6::R6Class(
  "ClassName",
  cloneable = FALSE,
  inherit = ObjectBase,  # or DotNetWrapper
  active = list(
    #' @field propertyName Description (read-only or read-write)
    propertyName = function(value) {
      if (missing(value)) {
        # Getter
        return(self$get("PropertyName"))
      } else {
        # Setter (if applicable)
        validateIsString(value)
        self$set("PropertyName", value)
      }
    }
  ),
  public = list(
    #' @description Constructor
    #' @param netObject .NET object reference
    initialize = function(netObject) {
      # Initialization code
    },
    
    #' @description Method description
    #' @param arg1 Description
    #' @return Return value description
    methodName = function(arg1) {
      # Method implementation
    }
  ),
  private = list(
    # Private fields and methods
  )
)
```

## Coding Standards

Follow the Open Systems Pharmacology R coding standards:
**Reference**: https://dev.open-systems-pharmacology.org/r-development-resources/coding_standards_r

### Naming Conventions

1. **Variables and functions**: `camelCase`
   ```r
   simulationResults <- runSimulation(sim)
   getParameterValue <- function(parameter) { ... }
   ```

2. **Classes**: `PascalCase`
   ```r
   SimulationResults <- R6::R6Class("SimulationResults", ...)
   ```

3. **Constants**: `ALL_CAPS` or `ALL_CAPS_WITH_UNDERSCORES`
   ```r
   MAX_ITERATIONS <- 1000
   DEFAULT_TOLERANCE <- 1e-6
   ```

4. **Private members**: Prefix with `.` (dot)
   ```r
   private = list(
     .cachedValue = NULL,
     .internalMethod = function() { ... }
   )
   ```

### Code Style

1. **Indentation**: 2 spaces (no tabs)
2. **Line length**: Keep lines under 80-100 characters
3. **Assignment**: Use `<-` for assignment, not `=`
4. **Spacing**: Space after commas, around operators
5. **Function definitions**: Opening brace on same line, closing brace on new line
6. **Avoid hard-coded values**: Use named constants or parameters

### Documentation

Every exported function and class must have roxygen2 documentation:

```r
#' @title Brief title
#' @description Detailed description of what the function/class does
#'
#' @param paramName Description of parameter
#' @param anotherParam Description
#'
#' @return Description of return value (type and structure)
#'
#' @examples
#' \dontrun{
#'   result <- functionName(arg1, arg2)
#' }
#'
#' @export
functionName <- function(paramName, anotherParam) {
  # Implementation
}
```

**For R6 classes**:
- Use `@field` for active bindings
- Use `@description` for methods
- Document constructor parameters in `initialize()`

### Input Validation

Always validate function inputs:

```r
# Use validation functions from ospsuite.utils or base R
validateIsString(value)
validateIsNumeric(value)
validateIsOfType(object, "Simulation")

# Or use stopifnot, assertthat, checkmate
stopifnot(length(values) > 0)
```

### Error Handling

```r
# Use stop() for errors with informative messages
if (is.null(simulation)) {
  stop("Simulation cannot be NULL")
}

# Use warning() for non-fatal issues
if (deprecated) {
  warning("This function is deprecated. Use newFunction() instead.")
}

# Use tryCatch() for operations that may fail
result <- tryCatch(
  {
    riskyOperation()
  },
  error = function(e) {
    stop(paste("Failed to perform operation:", e$message))
  }
)
```

### Performance

1. **Vectorization**: Prefer vectorized operations over loops
   ```r
   # Good
   results <- sapply(values, processValue)
   
   # Avoid
   results <- c()
   for (val in values) {
     results <- c(results, processValue(val))
   }
   ```

2. **Efficient data structures**: Use appropriate data structures (data.frame, data.table, lists)
3. **Avoid unnecessary copies**: R uses copy-on-modify; be mindful of large objects

### Code Complexity

- **Keep functions focused**: Each function should do one thing well
- **Limit cyclomatic complexity**: Aim for complexity ≤ 10 per function
- **Refactor complex logic**: Break down into smaller helper functions
- **Avoid deep nesting**: Limit nesting to 2-3 levels

## Best Practices

**Reference**: https://dev.open-systems-pharmacology.org/r-development-resources/best-practices-r

1. **Modular code**: Organize related functions together
2. **DRY principle**: Don't repeat yourself - refactor common code
3. **Meaningful names**: Use descriptive, self-explanatory names
4. **Comments**: Comment complex logic, not obvious code
5. **Unit tests**: Write tests for all new functions and bug fixes
6. **Documentation**: Keep documentation up-to-date with code changes

## R Code Structure

**Reference**: https://dev.open-systems-pharmacology.org/r-development-resources/r-code-structure

### File Organization

1. **One main class/concept per file**: `simulation.R`, `parameter.R`
2. **Utility files**: Group related utilities: `utilities-parameter.R`, `utilities-simulation.R`
3. **Base classes first**: `object-base.R`, `dot-net-wrapper.R` loaded early
4. **Collate order**: Controlled in DESCRIPTION file to ensure proper loading

### Function Organization Within Files

```r
# 1. Constants and global variables (if any)
CONSTANT_VALUE <- 100

# 2. Class definitions (R6::R6Class)
ClassName <- R6::R6Class(...)

# 3. Public exported functions
#' @export
publicFunction <- function() { ... }

# 4. Internal helper functions (not exported)
.internalHelper <- function() { ... }
```

## Testing Guidelines

**Framework**: testthat (edition 3)

### Test Structure

```r
# tests/testthat/test-feature-name.R

test_that("descriptive test name", {
  # Arrange
  sim <- loadTestSimulation()
  
  # Act
  result <- performOperation(sim)
  
  # Assert
  expect_true(is.numeric(result))
  expect_equal(length(result), 10)
  expect_gt(result[1], 0)
})
```

### Testing Guidelines

1. **Test file naming**: `test-<source-file-name>.R`
2. **Test organization**: Group related tests with `describe()` if needed
3. **Descriptive names**: Test names should clearly describe what's being tested
4. **AAA pattern**: Arrange, Act, Assert
5. **Test data**: Use test fixtures in `tests/testthat/data/` or `inst/extdata/`
6. **Snapshot tests**: Use for complex outputs (via vdiffr for plots)
7. **Coverage**: Aim for high test coverage of new code

### Common Test Patterns

```r
# Test for errors
expect_error(invalidOperation(), "expected error message")

# Test for warnings
expect_warning(deprecatedFunction(), "deprecated")

# Test object types
expect_s3_class(result, "data.frame")
expect_r6_class(sim, "Simulation")

# Test values
expect_equal(value, expected, tolerance = 1e-6)
expect_identical(obj1, obj2)

# Test conditions
expect_true(condition)
expect_false(!condition)
```

## Collaboration Guide

**Reference**: https://dev.open-systems-pharmacology.org/r-development-resources/collaboration_guide

### Branching Strategy

- `main`: Stable release branch
- Feature branches: `feature/description` or `copilot/description`
- Pull requests required for all changes

### Commit Messages

- Clear, concise descriptions of changes
- Reference issue numbers when applicable: `Fix #123: Description`

### Pull Request Process

1. Create feature branch from `main`
2. Make focused, minimal changes
3. Write/update tests for changes
4. Update documentation (roxygen2 comments)
5. Run R CMD check locally
6. Create pull request with clear description
7. Address code review feedback
8. Ensure CI checks pass

### Version Management

- Follows semantic versioning (major.minor.patch)
- Development versions use `.9000` suffix (e.g., `12.3.2.9006`)
- Version automatically bumped on merge to main

## rSharp Package

**Reference**: https://www.open-systems-pharmacology.org/rSharp

The `{rSharp}` package provides R-to-.NET interoperability:

### Common rSharp Patterns

```r
# Call .NET static method
result <- rSharp::callStatic("Namespace.ClassName", "MethodName", arg1, arg2)

# Get .NET property
value <- netObject$get("PropertyName")

# Set .NET property
netObject$set("PropertyName", newValue)

# Get .NET type
typeObj <- rSharp::getType("Namespace.ClassName")

# Enumerate .NET collection
items <- rSharp::toList(netCollection)
```

### .NET-R Type Conversions

- .NET `null` → R `NULL`
- .NET collections → R lists (via `rSharp::toList()`)
- .NET arrays → R vectors
- .NET objects → Wrapped in R6 classes

## Development Workflow

### Setting Up Development Environment

```r
# 1. Clone repository
# 2. Run development setup
source("tools/setup_dev.R")
setup_dev()

# 3. Install dependencies
remotes::install_deps(dependencies = TRUE)

# 4. Load package for development
devtools::load_all()
```

### Building and Checking

```r
# Check package
devtools::check()

# Build documentation
devtools::document()

# Run tests
devtools::test()

# Build vignettes
devtools::build_vignettes()

# Install locally
devtools::install()
```

### Updating Core Binary Files

```r
# Update .NET DLLs from OSPSuite.Core
source(".github/scripts/update_core_files.R")
# Follow instructions in script
```

## Common Pitfalls and Solutions

### 1. .NET Object Lifetime
**Issue**: .NET objects may be garbage collected unexpectedly  
**Solution**: Keep R references alive, use `clearMemory()` explicitly when needed

### 2. Locale Issues
**Issue**: Package loading fails with non-English locale  
**Solution**: Set locale to `en_US.UTF-8` (see README "Known issues")

### 3. Platform Differences
**Issue**: DLL naming differs between Windows and Linux  
**Solution**: Configure scripts handle this automatically; use `setup_dev()` for development

### 4. Workspace Saving
**Issue**: .NET objects cannot be saved/restored in R workspace  
**Solution**: Don't save workspace; reload simulations from files instead

### 5. Memory Management
**Issue**: Memory not freed after intensive operations  
**Solution**: Call `clearMemory(clearSimulationsCache = TRUE)` periodically

## Additional Resources

- **R Coding Standards**: https://dev.open-systems-pharmacology.org/r-development-resources/coding_standards_r
- **Best Practices R**: https://dev.open-systems-pharmacology.org/r-development-resources/best-practices-r
- **R Code Structure**: https://dev.open-systems-pharmacology.org/r-development-resources/r-code-structure
- **Collaboration Guide**: https://dev.open-systems-pharmacology.org/r-development-resources/collaboration_guide
- **rSharp Documentation**: https://www.open-systems-pharmacology.org/rSharp
- **Package Documentation**: https://www.open-systems-pharmacology.org/OSPSuite-R/
- **GitHub Repository**: https://github.com/Open-Systems-Pharmacology/OSPSuite-R
- **Bug Reports**: https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues

---

# GitHub Copilot Code Review Instructions for R Package PRs

For each file modified in a pull request, GitHub Copilot should generate a Markdown section with the following structure:

---

## [File: `filename.R`]

Copilot must analyze every function and every R6 class method in the file for the following criteria:

### a) Syntactic Correctness and Performance
- Code is free of parse errors.
- Uses vectorized operations where possible.
- Avoids unnecessary loops.
- Employs efficient data structures.

### b) Documentation and Comments
- Functions and classes have roxygen2 docstrings.
- Inline comments are present for complex/nontrivial logic.
- Parameters and return values are described.
- Explanations are meaningful and non-obvious logic is commented.

### c) Cyclomatic Complexity
- Cyclomatic complexity of each function/method does not exceed 10.
- Flag and report functions/methods exceeding this threshold.

### d) Coding Standard Compliance
- Follows OSP coding standards: https://dev.open-systems-pharmacology.org/r-development-resources/coding_standards_r
- Naming conventions, indentation, modularity, function length, avoidance of hard-coded values, and language construct use.
- Object naming conventions:
  - Variable and function names: lowercase letters/numbers, camelCase.
  - Class names: PascalCase.
  - Constant variables: ALL_CAPS.

### e) Syntax Errors
- No incomplete statements, unmatched parentheses/brackets, or unfinished/misleading comments.

### f) Naming Conventions and Meaningfulness
- Function and variable names are self-explanatory and meaningful.
- Follows OSP naming conventions (see d).

### g) No Deprecated/Discouraged Patterns
- No use of deprecated or discouraged functions, packages, or coding patterns.

### h) Error Handling
- Proper error/warning handling: use `stop()`, `warning()`, `tryCatch()` as appropriate.
- Error messages are informative.

### i) Input Validation
- All input arguments validated (e.g., with `stopifnot()`, `assertthat`, `checkmate`).

### j) Code Duplication
- No obvious duplication; repeated logic is refactored into helpers.

### k) Magic Numbers and Constants
- No unexplained constants or "magic numbers"; all such values are named or documented.

### l) Output Description and Type
- Return values are documented and their types are clear and as expected.

### m) Unit Tests or Examples
- Major functions/methods have unit tests or testable `@examples` roxygen2 tags.
- Flag absence if missing.

---

## Markdown Output Format

For each criterion above, create a Markdown subsection.  
Within each subsection, provide a table of findings for *only those functions/methods with issues*.  
Each table should have the following columns:

| Function/Method Name | Finding | Suggestions |
|----------------------|---------|-------------|

- The last column (Suggestions) should provide specific, actionable guidance on how to resolve the issue.

If no findings are present for a criterion, omit the table for that criterion.

---

### Example

#### a) Syntactic Correctness and Performance

| Function/Method Name | Finding | Suggestions |
|----------------------|---------|-------------|
| calculateStats       | Unnecessary for-loop used for vectorized operation | Replace loop with `apply()` or vectorized function. |

#### c) Cyclomatic Complexity

| Function/Method Name | Finding | Suggestions |
|----------------------|---------|-------------|
| processData          | Cyclomatic complexity = 15 (exceeds 10) | Refactor into smaller helper functions to reduce complexity. |

#### h) Error Handling

| Function/Method Name | Finding | Suggestions |
|----------------------|---------|-------------|
| loadData             | No error handling for missing files | Use `tryCatch()` to handle file loading errors and provide informative messages. |

---

### Additional Instructions

- For criteria (d) and (f), always cross-check against the OSP coding standards: https://dev.open-systems-pharmacology.org/r-development-resources/coding_standards_r
- Always format output as Markdown tables, grouped under their respective heading.
- Do not include tables for criteria with no issues found.
- If a function or method has multiple findings (across different criteria), report them in the relevant tables under each criterion.

---
