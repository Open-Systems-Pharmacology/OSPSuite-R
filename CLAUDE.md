# CLAUDE.md — OSPSuite-R

Guidance for working in this repository with Claude Code.

## What this package is

`ospsuite` is an R wrapper around the OSP Suite **.NET Core** libraries. R talks to
.NET through the **`rSharp`** package. The .NET binaries the package needs are
committed into **`inst/lib/`** (OSPSuite.Core.dll, NPOI, SQLite, etc.) and are
loaded at runtime by rSharp when the package initializes.

## Where the .NET implementation lives (look here, not just in R)

The R functions are thin wrappers over .NET. When you need to understand what a
method *actually does* — or why it throws — read the .NET source in the upstream
repositories:

- **OSPSuite.Core** — https://github.com/Open-Systems-Pharmacology/OSPSuite.Core
  (the core domain/import/simulation logic, e.g. `ExcelDataSourceFile`, `DataImporter`).
- **MoBi** — https://github.com/Open-Systems-Pharmacology/MoBi
- **PK-Sim** — https://github.com/Open-Systems-Pharmacology/PK-Sim
  (`PKSim.R` is the entry assembly the R package initializes).

**Branch mapping matters:** this R package's `main` ships Core **v12** = the Core
repo's `develop` branch; this package's `v13` ships Core **v13** = the Core repo's
`V13` branch. Always read the branch that matches the R branch you're on, or you'll
be comparing against the wrong .NET code. Fetch raw files, e.g.
`https://raw.githubusercontent.com/Open-Systems-Pharmacology/OSPSuite.Core/V13/src/...`.

## How `inst/lib` is populated (differs by branch)

The `.NET` binaries in `inst/lib/` are generated, not hand-maintained — but the
mechanism differs between the release lines:

**v13 — DependencyManager + allow-list.** `shared/DependencyManager/` is a .NET
project whose only purpose is dependency resolution. Its `.csproj` references the
pinned NuGet packages (`MoBi.R`, `PKSim.R`, `OSPSuite.*`) and, on **PostBuild**,
copies their DLLs into `inst/lib/` then enforces an **allow-list**
(`@(AllowedAssembly)`): any `inst/lib` binary whose name is not on the list is
**deleted**. This is deliberate (issue #1587) — a new upstream transitive dependency
surfaces as a runtime `FileNotFoundException` instead of silently bloating the package.
**To ship a new DLL, add its name (without extension) to `@(AllowedAssembly)`.**
`.github/workflows/build-libraries.yaml` rebuilds the solution and auto-commits the
regenerated `inst/lib` when `DependencyManager.csproj` changes — but note it is a
`workflow_call` reusable workflow and must be wired into a triggering workflow to run;
if it isn't invoked on your PR, commit the DLL manually alongside the `.csproj` change.

**main (v12) — download from build artifacts.** `.github/scripts/update_core_files.R`
(run by the `Update Core Files` workflow) downloads and unzips the DLL set from the
PK-Sim build artifacts into `inst/lib`. There is no DependencyManager / allow-list here.

## Branch ↔ dependency coupling (READ THIS BEFORE SWITCHING BRANCHES)

Different R branches are pinned to different **rSharp** builds and ship different
**`inst/lib/` .NET binaries**:

| R branch | Core version | rSharp `RemoteRef` (in `renv.lock`) |
|----------|--------------|-------------------------------------|
| `main`   | v12          | `main`                              |
| `v13`    | v13          | `V13`                               |

Consequences:

- **Both branches pin rSharp version string `1.2.0.9000`** but with *different git
  SHAs*. renv distinguishes them by content hash in its global cache (verified:
  two hash dirs exist under `.../cache/.../rSharp/1.2.0.9000/`), so `renv::restore()`
  resolves the correct build per branch — but you MUST run it after switching.
- Switching a branch in place therefore requires **`renv::restore()`** to get the
  matching rSharp.
- rSharp **loads native DLLs into the running R process**, which **locks them**.
  You must **restart the R session before switching branches**, otherwise the
  `inst/lib` DLLs and the rSharp DLLs cannot be overwritten by `renv::restore()`.

### Preferred workflow: separate git worktrees per branch

Instead of switching branches in one working directory, use **one git worktree per
branch**. Each worktree:

- has its own `inst/lib/` copy (no DLL-lock conflicts between branches), and
- gets its own renv **project library** (the library path is keyed by project
  path hash, e.g. `.../renv/library/OSPSuite-R-<hash>/...`), while sharing the
  global renv **cache** (which correctly keeps the per-branch rSharp builds apart).

Run `renv::restore()` once in each worktree. After that you can keep an R session
open per worktree and compare `main` vs `v13` behavior without any restore/restart
thrash. Example:

```bash
git worktree add ../OSPSuite-R-main main
git worktree add ../OSPSuite-R-v13  v13
# then in each: R -> renv::restore()
```

## Running things

- The system R here is `C:\Program Files\R\R-4.6.0\bin\Rscript.exe` (not on PATH).
- **Do NOT run with `Rscript --vanilla`** — `--vanilla` skips `.Rprofile`, so
  `renv/activate.R` never runs and you silently fall back to the *global* user
  library (wrong rSharp / missing dev deps). Run without `--vanilla` so renv activates.
- `devtools::load_all()` loads the **current source + this tree's `inst/lib` DLLs**
  (the true "what does this branch do" test). The *installed* `ospsuite` in the
  global library may be a different (e.g. v12) build and can mask branch-specific bugs.
- `load_all()` runs a dependency check; dev deps like `ospsuite.plots (>= …9003)`
  must be installed in the active (renv) library or it errors before loading.

## Debugging .NET-side errors

Exceptions from .NET surface through rSharp as R errors. Two gotchas:

- rSharp **truncates** the exception text (~1024 chars), so long stack traces get
  cut off before any InnerException detail.
- Several Core readers **catch the real exception and rethrow a generic one**
  (e.g. `ExcelDataSourceFile.DoLoadWork` catches everything and throws
  `InvalidObservedDataFileException("An error occurred while reading the file…")`,
  logging the real message only to a logger that is silent in the R/CLI setup).

To get the real cause, **replicate the failing .NET call path at a low level from R**
via `rSharp::newObjectFromName()` / `$call()` / `$get()`, wrapping each step in
`tryCatch`, so the raw unwrapped exception surfaces at the exact failing operation.
