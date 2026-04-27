# Phase A audit — re-run on macOS / Ubuntu

Instructions for Claude. The user has run Phase A on Windows already
(see `tools/audit/findings.md`) and wants the same capture on macOS arm64
and on Ubuntu so we can union the loaded-assembly sets across platforms.

Issue: https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/1587

## What is already in place (don't redo)

- `shared/DependencyManager/src/Auditor.cs` — `DependencyManager.Auditor.GetLoadedAssemblies()` returning `string[]` of `"Name|Version|Location"`.
- `shared/DependencyManager/src/DependencyManager.csproj` — the `<Delete>` line that used to remove `DependencyManager.dll` from `inst/lib` is gone, so the auditor ships.
- `tests/testthat/teardown.R` — appends an env-var-guarded block that calls the auditor and writes `tools/audit/logs/loaded-assemblies-<platform>-pid<PID>.log`. One file per testthat worker (testthat parallel mode runs 2 workers).
- `inst/lib/DependencyManager.dll` — committed to the repo.

If any of those is missing on the new machine, the user has not pushed the branch yet — ask them before recreating.

## Steps

### 1. Confirm the audit plumbing is present

```bash
ls inst/lib/DependencyManager.dll
grep -q OSPSUITE_AUDIT_ASSEMBLIES tests/testthat/teardown.R && echo OK
grep -q AppDomain shared/DependencyManager/src/Auditor.cs && echo OK
```

If `DependencyManager.dll` is missing but `Auditor.cs` and the csproj edit are present, rebuild it:

```bash
dotnet build shared/DependencyManager/DependencyManager.sln --configuration Release
```

You may need to close any open R/RStudio/Positron sessions first — they hold file locks on the DLLs in `inst/lib`. If the build errors with "file is locked by ... R for Windows / R / Rsession", ask the user to close those.

### 2. Make sure R can load the package

R deps come via `renv` on this repo. From the package root:

```bash
Rscript -e 'renv::restore()'
```

(macOS: `brew install r` first if needed; Ubuntu: `sudo apt install r-base` and `r-base-dev`. The test suite expects `devtools`, `testthat`, `rSharp`, `pkgload`, `rprojroot`. `renv::restore` should install whatever the lockfile names.)

If `rSharp` is the bottleneck (it's the .NET-interop bridge, sometimes finicky to install), check the rSharp install instructions in the package README. The user has it installed on Windows already.

### 3. Run the test suite with auditing enabled

The full suite takes ~3–5 min on Windows; allow 10 min headroom on slower machines. Use a background task so you don't time out the foreground.

**macOS / Linux**:

```bash
OSPSUITE_AUDIT_ASSEMBLIES=1 Rscript -e 'devtools::test()' 2>&1 | tee tools/audit/test-run.log
```

Run via `Bash` with `run_in_background: true` and a 30-min timeout. You'll get a notification when it completes; don't poll.

If `Rscript` is not on `$PATH`:
- macOS: `/Library/Frameworks/R.framework/Resources/bin/Rscript` (Homebrew: `/opt/homebrew/bin/Rscript`)
- Ubuntu: `/usr/bin/Rscript` (or `which Rscript`)

### 4. Sanity-check the output

```bash
ls -la tools/audit/logs/
```

You should see one or two `loaded-assemblies-<platform>-pid<PID>.log` files. Platform tag is `darwin` on macOS, `linux` on Ubuntu (per `tolower(Sys.info()[["sysname"]])`). On Windows it's `windows`.

Tail one of the logs to confirm it has 100+ entries with `Name|Version|Location` rows. If it's empty or missing, the env-var path didn't fire — check `OSPSUITE_AUDIT_ASSEMBLIES` was set in the same shell that launched Rscript.

If tests **failed**, that's a separate problem to debug — capture is still useful but report failures to the user before drawing exclusion conclusions.

### 5. Union per-PID logs

```bash
cat tools/audit/logs/loaded-assemblies-<platform>-pid*.log | sort -u \
  > tools/audit/logs/loaded-assemblies-<platform>-union.log
```

Replace `<platform>` with `darwin` or `linux`.

### 6. Diff against `inst/lib`

```bash
ls inst/lib/*.dll | sed 's|inst/lib/||; s|\.dll$||' | sort > /tmp/lib-dlls.txt
awk -F'|' '{print $1}' tools/audit/logs/loaded-assemblies-<platform>-union.log | sort -u > /tmp/loaded.txt

echo "=== DLLs in inst/lib NEVER loaded on <platform> ==="
comm -23 /tmp/lib-dlls.txt /tmp/loaded.txt

echo "=== inst/lib DLLs loaded but resolved from runtime, not inst/lib ==="
awk -F'|' '$3 != "" && $3 !~ /inst.lib/ {print $1}' \
  tools/audit/logs/loaded-assemblies-<platform>-union.log | sort > /tmp/runtime-resolved.txt
comm -12 /tmp/lib-dlls.txt /tmp/runtime-resolved.txt
```

(Use forward slashes in awk patterns; `inst.lib` is regex for both `inst/lib` and `inst\lib`.)

### 7. Append findings to the report

Open `tools/audit/findings.md` and add a new section per platform showing:
- Counts (loaded / never-loaded / runtime-shadowed / native).
- The "never loaded on this platform" list.
- The "runtime-shadowed on this platform" list.
- Note any divergence from the Windows results — e.g. Windows-specific assemblies (`Microsoft.Win32.SystemEvents`, `System.Diagnostics.EventLog`, `System.Security.Cryptography.ProtectedData`) won't be present on Linux/macOS as runtime resolutions, which is normal.

### 8. Commit the new logs

```bash
git add tools/audit/logs/loaded-assemblies-<platform>-*.log tools/audit/findings.md
git commit -m "Phase A audit: capture loaded assemblies on <platform> (#1587)"
```

Don't commit `tools/audit/test-run.log` — it's noisy testthat output and not interesting after the run.

## Platform-specific things to watch for

### macOS arm64

- `init-package.R` rejects non-arm64 macOS — only Apple Silicon is supported.
- The `.dylib` natives (`libOSPSuite.FuncParserNative.dylib`, `libOSPSuite.SimModelNative.dylib`, `libOSPSuite.SimModelSolver_CVODES.dylib`, `libe_sqlite3.dylib`) are loaded via `dyn.load` at package init.
- macOS sets `tolower(Sys.info()[["sysname"]])` to `darwin`, so the log filename will be `loaded-assemblies-darwin-pid<PID>.log`.
- The `Location` field on macOS will use forward slashes.

### Ubuntu

- The `.so` natives are loaded via `dyn.load` at package init.
- Test suite runs the same way; expect the same FAIL/PASS counts as Windows (zero failures).
- Some Windows-only `System.*` libs (e.g. `System.Diagnostics.EventLog`, `Microsoft.Win32.SystemEvents`, `System.Security.Cryptography.ProtectedData`) won't load on Linux even if used. That confirms they're Windows-specific and removable on non-Windows shipments — but since `inst/lib` is shared across platforms in this package, removing them affects Windows too. Cross-reference with the Windows union before deciding.

### .NET 8 runtime location

- macOS: `/usr/local/share/dotnet/shared/Microsoft.NETCore.App/8.0.x/` or `~/.dotnet/`
- Linux: `/usr/share/dotnet/shared/Microsoft.NETCore.App/8.0.x/` or `~/.dotnet/`
- Windows: `C:\Program Files\dotnet\shared\Microsoft.NETCore.App\8.0.x\`

The "loaded but resolved from runtime" buckets will use whichever path the platform installed dotnet to.

## What to report back to the user

A short summary in chat:
- Test pass count.
- Platform-specific never-loaded count and total size.
- Any new candidates that didn't appear on Windows.
- Any candidates from Windows that DO load on this platform (and so should be reclassified).
- Path to the updated `findings.md`.

Don't propose csproj exclusions yet. Phase C (empirical removal + per-batch test runs) is the gate for that. Phase A is just the data collection.
