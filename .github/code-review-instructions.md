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
