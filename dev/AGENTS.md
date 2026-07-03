# OSPSuite-R — agent & contributor guide

Guidance for working in this repository with coding agents (Claude Code,
Copilot, CodeRabbit, …). Tool-neutral; `CLAUDE.md` points here.

## What this package is

`ospsuite` is an R wrapper around the OSP Suite **.NET Core** libraries.
R talks to .NET through the **`rSharp`** package. The .NET binaries the
package needs are committed into **`inst/lib/`** (OSPSuite.Core.dll,
NPOI, SQLite, etc.) and are loaded at runtime by rSharp when the package
initializes.

## Where the .NET implementation lives (look here, not just in R)

The R functions are thin wrappers over .NET. When you need to understand
what a method *actually does* — or why it throws — read the .NET source
in the upstream repositories, on the **`develop`** branch (the branch
this line of the R package builds against):

- **OSPSuite.Core** —
  <https://github.com/Open-Systems-Pharmacology/OSPSuite.Core> (core
  domain/import/simulation logic, e.g. `ExcelDataSourceFile`,
  `DataImporter`).
- **MoBi** — <https://github.com/Open-Systems-Pharmacology/MoBi>
- **PK-Sim** — <https://github.com/Open-Systems-Pharmacology/PK-Sim>
  (`PKSim.R` is the entry assembly the R package initializes).

Fetch raw files directly, e.g.
`https://raw.githubusercontent.com/Open-Systems-Pharmacology/OSPSuite.Core/develop/src/...`.

## How `inst/lib` is populated

The `.NET` binaries in `inst/lib/` are generated, not hand-maintained.
`.github/scripts/update_core_files.R` (run by the **Update Core Files**
workflow) downloads and unzips the DLL set from the PK-Sim build
artifacts into `inst/lib`.

## renv, branch switching, and worktrees

The package uses **renv**; the pinned dependency set (including
`rSharp`) is in `renv.lock`. Run
[`renv::restore()`](https://rstudio.github.io/renv/reference/restore.html)
to sync your library to the lockfile.

- **Never run `Rscript --vanilla`.** `--vanilla` skips `.Rprofile`, so
  `renv/activate.R` never runs and you silently fall back to the
  *global* user library (wrong / unpinned packages, including the wrong
  rSharp). Run without `--vanilla` so renv activates and the project
  library is used.

- **Switching branches is not free.** Different branches pin **different
  rSharp builds** and ship **different `inst/lib/` .NET binaries**. Two
  consequences:

  1.  You must run
      **[`renv::restore()`](https://rstudio.github.io/renv/reference/restore.html)**
      after switching to get the matching rSharp. (renv keys its cache
      on content hash, so it resolves the correct build even when two
      branches share the same rSharp version string.)
  2.  rSharp **loads the native DLLs into the running R process and
      locks them**, so you must **restart the R session before
      switching**, otherwise
      [`renv::restore()`](https://rstudio.github.io/renv/reference/restore.html)
      cannot overwrite the locked `inst/lib` / rSharp DLLs.

- **`devtools::load_all()` runs a dependency check against the active
  library**, so dev dependencies must be present
  ([`renv::restore()`](https://rstudio.github.io/renv/reference/restore.html)d)
  first, or it errors before it can load the package.

- **One option: a separate git worktree per branch** when you work on
  more than one at a time. Each worktree has its own `inst/lib/` (no
  DLL-lock conflicts) and its own renv **project library** (keyed by
  project-path hash), while sharing the global renv **cache** safely.
  This avoids the restore/restart thrash entirely:

  ``` bash
  git worktree add ../OSPSuite-R-main main
  git worktree add ../OSPSuite-R-<branch> <branch>
  # then run renv::restore() once in each
  ```

## Running and debugging

- `devtools::load_all()` loads the **current source + this tree’s
  `inst/lib` DLLs** (the true “what does this branch do” test). The
  *installed* `ospsuite` in the global library may be a different build
  and can mask branch-specific behavior. (It also runs a dependency
  check — see the renv section above.)

- .NET exceptions surface through rSharp as R errors, with two gotchas:

  - rSharp **truncates** the exception text (~1024 chars), so long stack
    traces get cut off before any InnerException detail.
  - Several Core readers **catch the real exception and rethrow a
    generic one** (e.g. `ExcelDataSourceFile.DoLoadWork` throws
    `InvalidObservedDataFileException("An error occurred while reading the file…")`,
    logging the real message only to a logger that is silent in the
    R/CLI setup).

  To get the real cause, **replicate the failing .NET call path at a low
  level from R** via
  [`rSharp::newObjectFromName()`](http://www.open-systems-pharmacology.org/rSharp/reference/newObjectFromName.md)
  / `$call()` / `$get()`, wrapping each step in `tryCatch`, so the raw
  unwrapped exception surfaces at the exact failing operation.
