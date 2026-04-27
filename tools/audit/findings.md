# Phase A — Loaded-assembly audit results

Captures from `AppDomain.CurrentDomain.GetAssemblies()` taken at the end of the
testthat run on Windows and macOS arm64. See issue
[#1587](https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/1587).

## Methodology

1. Added `DependencyManager.Auditor.GetLoadedAssemblies()` (in
   [`shared/DependencyManager/src/Auditor.cs`](../../shared/DependencyManager/src/Auditor.cs)).
2. Stopped excluding `DependencyManager.dll` from `inst/lib`.
3. `tests/testthat/teardown.R` writes the loaded-assembly snapshot to
   `tools/audit/logs/loaded-assemblies-<platform>-pid<PID>.log` when
   `OSPSUITE_AUDIT_ASSEMBLIES=1` is set.
4. Ran `devtools::test()` on Windows (testthat parallel mode = 2 workers).
   Both workers passed (FAIL 0 | PASS 2011) and each emitted its own log.
5. Took the union across PIDs.

Reproduce with:

```powershell
$env:OSPSUITE_AUDIT_ASSEMBLIES = '1'
& 'C:\Program Files\R\R-4.4.2\bin\Rscript.exe' -e 'devtools::test()'
```

## Summary

| Bucket | Count |
|---|---|
| Total managed DLLs in `inst/lib` | 100 |
| Loaded from `inst/lib` during tests | 66 |
| Loaded but resolved from .NET 8 runtime instead of `inst/lib` | 2 |
| Never loaded (managed) | 30 |
| Native `.dll` in `inst/lib` not visible to `GetAssemblies()` (P/Invoke; KEEP) | 4 |

Tier-1 candidates (UI/reporting/crypto) hypothesized in the original plan were
mostly **wrong** — `BouncyCastle.Cryptography`, `DevExpress.Data.v21.2`,
`OSPSuite.Assets.Images`, `SixLabors.{ImageSharp,Fonts}`, `OSPSuite.TeXReporting`,
`MoBi.Presentation`, `PKSim.Presentation`, `OSPSuite.Presentation`, `MarkdownLog`,
and the CLI assemblies all DO load. Castle.Windsor / Autofac scan and pull
them in eagerly even when no code path exercises them.

## Removable DLLs (32, ~5.5 MB total)

### Never loaded (30 files, ~5.0 MB)

Sorted by size:

| DLL | Size | Notes |
|---|---:|---|
| MathNet.Numerics.dll | 1.5 MB | Numerics. Surprising — verify via Phase C. |
| PKSim.Assets.Images.dll | 895 KB | Image resources (OSPSuite variant *is* loaded; PK-Sim variant isn't). |
| System.Data.SQLite.dll | 435 KB | Distinct from `Microsoft.Data.Sqlite` which IS loaded. |
| System.Security.Cryptography.Pkcs.dll | 258 KB | PKCS — unused. |
| Dapper.dll | 240 KB | Micro-ORM. NHibernate is what's used. |
| System.Security.Cryptography.Xml.dll | 193 KB | Signed-XML — unused. |
| System.Diagnostics.DiagnosticSource.dll | 169 KB | Diagnostic activity tracing. |
| Serilog.dll | 157 KB | Logging — Microsoft.Extensions.Logging is used instead. |
| Enums.NET.dll | 120 KB | Enum helpers. |
| System.Resources.Extensions.dll | 103 KB | Binary resource reader. |
| System.Security.Permissions.dll | 101 KB | CAS permissions — unused. |
| Microsoft.Extensions.DependencyModel.dll | 74 KB | Reflection over deps.json. |
| System.Formats.Nrbf.dll | 65 KB | NRBF binary serialization (.NET 9 fwd-compat). |
| Microsoft.IO.RecyclableMemoryStream.dll | 64 KB | Memory pooling. |
| System.Diagnostics.EventLog.dll | 50 KB | Windows Event Log. |
| System.Diagnostics.PerformanceCounter.dll | 45 KB | Perf counters. |
| Microsoft.Extensions.Primitives.dll | 43 KB | Used only via Microsoft.Extensions.Configuration which is unused. |
| OSPSuite.Infrastructure.Reporting.dll | 36 KB | Reporting orchestration. |
| Microsoft.Extensions.Configuration.Binder.dll | 33 KB | Config binding. |
| Serilog.Sinks.File.dll | 30 KB | Serilog sink. |
| Microsoft.Win32.SystemEvents.dll | 26 KB | Windows UI signaling. |
| System.Windows.Extensions.dll | 25 KB | Sound/media. |
| Microsoft.Extensions.Configuration.Abstractions.dll | 25 KB | Config abstractions. |
| System.Security.Cryptography.ProtectedData.dll | 20 KB | DPAPI. |
| MPFitLib.dll | 20 KB | Levenberg-Marquardt fit (managed). Surprising — verify. |
| Serilog.Sinks.RollingFile.dll | 18 KB | Serilog sink. |
| Serilog.Extensions.Logging.dll | 17 KB | Serilog→ILogger bridge. |
| Serilog.Extensions.Logging.File.dll | 11 KB | Serilog file logger. |
| Serilog.Sinks.Async.dll | 9 KB | Serilog async sink. |
| Serilog.Formatting.Compact.dll | 8 KB | Serilog formatter. |

### Loaded but resolved from .NET 8 runtime, not `inst/lib` (2 files, ~734 KB)

Our shipped copy is shadowed by the runtime's. Removing them is safe.

| DLL | Size | Resolved from |
|---|---:|---|
| System.Reflection.Metadata.dll | 489 KB | `C:\Program Files\dotnet\shared\Microsoft.NETCore.App\8.0.26\` |
| System.Collections.Immutable.dll | 245 KB | `C:\Program Files\dotnet\shared\Microsoft.NETCore.App\8.0.26\` |

## DLLs to KEEP

### Native libs (P/Invoke — invisible to `GetAssemblies()`)

These show as "not loaded" but ARE used via DllImport. Do **not** remove.

- `OSPSuite.FuncParserNative.dll`
- `OSPSuite.SimModelNative.dll`
- `OSPSuite.SimModelSolver_CVODES.dll`
- `e_sqlite3.dll`

Plus their Linux (`.so`) and macOS arm64 (`.dylib`) counterparts.

### Confirmed loaded (66 managed DLLs from `inst/lib`)

See [`logs/loaded-assemblies-windows-union.log`](logs/loaded-assemblies-windows-union.log)
for the full list with versions and locations.

## macOS arm64 (darwin) results

Captured on Apple Silicon (`uname -m: arm64`) with R 4.5.1 and the .NET 8
runtime at `/usr/local/share/dotnet/shared/Microsoft.NETCore.App/8.0.7/`.
Test suite: `FAIL 0 | WARN 95 | SKIP 0 | PASS 2011` (matches Windows).

Reproduce:

```bash
OSPSUITE_AUDIT_ASSEMBLIES=1 Rscript -e 'devtools::test()'
```

Logs: [`logs/loaded-assemblies-darwin-pid4117.log`](logs/loaded-assemblies-darwin-pid4117.log),
[`logs/loaded-assemblies-darwin-pid4118.log`](logs/loaded-assemblies-darwin-pid4118.log),
union [`logs/loaded-assemblies-darwin-union.log`](logs/loaded-assemblies-darwin-union.log).

### Counts (darwin)

| Bucket | Count |
|---|---|
| Total managed DLLs in `inst/lib` | 99 |
| Loaded from `inst/lib` during tests | 66 |
| Loaded but resolved from .NET 8 runtime instead of `inst/lib` | 3 |
| Never loaded (managed) | 30 |
| Native `.dll` not visible to `GetAssemblies()` (P/Invoke; KEEP) | 4 |

`inst/lib` had 103 `.dll` files at audit time vs. 100 in the Windows table:
`System.Text.Json.dll` was added since the Windows capture (it shadows on
darwin — see below) and `System.Data.SQLite.dll` was already on the
never-loaded list and is now also gitignored locally; both still ship via
the directory listing.

### Never loaded on darwin (30 managed)

The darwin never-loaded set is **identical to Windows** — same 30 managed
DLLs (plus the 4 natives which are loaded via `dyn.load` of their `.dylib`
counterparts and so don't appear in `GetAssemblies()` on either platform).
Refer to the size-sorted table under "Removable DLLs → Never loaded" above.

No darwin-only or Windows-only entries on this list.

### Runtime-shadowed on darwin (3 files, ~1.27 MB)

| DLL | Size | Resolved from |
|---|---:|---|
| System.Text.Json.dll | 567 KB | `/usr/local/share/dotnet/shared/Microsoft.NETCore.App/8.0.7/` |
| System.Reflection.Metadata.dll | 489 KB | `/usr/local/share/dotnet/shared/Microsoft.NETCore.App/8.0.7/` |
| System.Collections.Immutable.dll | 245 KB | `/usr/local/share/dotnet/shared/Microsoft.NETCore.App/8.0.7/` |

`System.Reflection.Metadata` and `System.Collections.Immutable` shadow on
both platforms. `System.Text.Json` is **new** vs. the Windows capture
(added to `inst/lib` after that run); re-run the Windows capture to
confirm it shadows there too before listing it as removable.

### Cross-platform divergence

Windows-specific runtime resolutions that do not appear on darwin (none
in `inst/lib`, so they don't affect removal candidates either way):

- `Microsoft.Win32.Registry`
- `System.Security.AccessControl`
- `System.Security.Claims`
- `System.Security.Principal.Windows`

Darwin extras not seen on Windows (also not in `inst/lib`):

- `System.IO.Pipes`

The Windows-only entries already on the `inst/lib` never-loaded list
(`Microsoft.Win32.SystemEvents`, `System.Diagnostics.EventLog`,
`System.Security.Cryptography.ProtectedData`) are also never loaded on
darwin — confirming they are Windows-only. Because `inst/lib` is shared
across platforms, removing them would still affect Windows; defer to
Phase C.

## Linux (Ubuntu) results

Captured on Ubuntu (Linux 7.0.0-14-generic) with R 4.5.2 and the .NET 8
runtime at `/usr/lib/dotnet/shared/Microsoft.NETCore.App/8.0.26/`.
Test suite: `FAIL 0 | WARN 95 | SKIP 0 | PASS 2011` (matches Windows / darwin).

Reproduce:

```bash
OSPSUITE_AUDIT_ASSEMBLIES=1 Rscript -e 'devtools::test()'
```

Logs: [`logs/loaded-assemblies-linux-pid6970.log`](logs/loaded-assemblies-linux-pid6970.log),
[`logs/loaded-assemblies-linux-pid6972.log`](logs/loaded-assemblies-linux-pid6972.log),
union [`logs/loaded-assemblies-linux-union.log`](logs/loaded-assemblies-linux-union.log).

### Counts (linux)

| Bucket | Count |
|---|---|
| Total managed DLLs in `inst/lib` | 97 |
| Loaded from `inst/lib` during tests | 66 |
| Loaded but resolved from .NET 8 runtime instead of `inst/lib` | 2 |
| Never loaded (managed) | 29 |
| Native `.dll` not visible to `GetAssemblies()` (`.so` loaded via `dyn.load`; KEEP) | 4 |

`inst/lib` had 101 `.dll` files at audit time vs. 103 on darwin and 104 on
Windows. Two files that darwin had — `System.Text.Json.dll` and
`System.Data.SQLite.dll` — are absent from this Linux working tree. They
are not present in this snapshot's never-loaded numbers; cross-platform
removal decisions should still account for them based on Windows / darwin.

### Never loaded on linux (29 managed)

The linux never-loaded set is **identical to the darwin / Windows set**
minus `System.Text.Json` and `System.Data.SQLite` (neither file is in
`inst/lib` on this checkout). Same 4 native `.dll` files appear as "not
loaded" by `GetAssemblies()`; on Linux the corresponding `.so` is loaded
via `dyn.load` (`libOSPSuite.FuncParserNative.so`,
`libOSPSuite.SimModelNative.so`,
`libOSPSuite.SimModelSolver_CVODES.so`, `libe_sqlite3.so`), so the `.dll`
shipped alongside isn't read on this platform — KEEP regardless.

No linux-only or platform-divergent never-loaded entries.

### Runtime-shadowed on linux (2 files, ~734 KB)

| DLL | Size | Resolved from |
|---|---:|---|
| System.Reflection.Metadata.dll | 489 KB | `/usr/lib/dotnet/shared/Microsoft.NETCore.App/8.0.26/` |
| System.Collections.Immutable.dll | 245 KB | `/usr/lib/dotnet/shared/Microsoft.NETCore.App/8.0.26/` |

These match the two assemblies shadowed on Windows. `System.Text.Json` is
loaded from the .NET runtime path on Linux as well, but since its `.dll`
isn't in `inst/lib` on this checkout, it doesn't count toward the
runtime-shadowed bucket here — re-confirm on a checkout that has it (per
the darwin section's note).

### Cross-platform divergence

Loaded-assembly *names* on Linux and darwin are identical (144 unique
names each, same set). The only differences are file-system locations
(.NET runtime path under `/usr/lib/dotnet/...` on this Ubuntu install vs
`/usr/local/share/dotnet/...` on darwin) and the absence of
`System.Text.Json.dll` / `System.Data.SQLite.dll` from `inst/lib` on
this Linux working tree.

Windows-only entries already on the `inst/lib` never-loaded list
(`Microsoft.Win32.SystemEvents`, `System.Diagnostics.EventLog`,
`System.Security.Cryptography.ProtectedData`) are also never loaded on
Linux — confirming they are Windows-only across both non-Windows
platforms. Because `inst/lib` ships unified across platforms, removing
them would still affect Windows; defer to Phase C.

## Risks before excluding

The Phase A capture is **necessary but not sufficient**. Things it does not catch:

1. **Code paths the test suite doesn't cover.** Some user workflows (vignettes,
   undocumented use of imports/exports, some snapshot edge cases) may load
   assemblies the tests don't. Cross-check with vignette renders before excluding.
2. **Per-platform variance.** Windows, macOS arm64, and Ubuntu Linux
   captures are now all in place. The never-loaded set is identical
   across platforms (modulo files not present in `inst/lib` on a given
   checkout). Safe to proceed to Phase C.
3. **Lazy `initPKSim()` paths.** `initPKSim()` is invoked implicitly by
   several tests (createIndividual, createPopulation, snapshots, parameter-range)
   so PK-Sim assemblies *are* covered. Verified — `PKSim.R`, `PKSim.Core`,
   `PKSim.Infrastructure`, `PKSim.Assets`, `PKSim.CLI.Core`, `PKSim.Presentation`
   all show as loaded.

## Next steps (deferred — not part of Phase A)

- **Phase B**: Mono.Cecil static reference closure to corroborate.
- **Phase C**: empirical removal in batches with full test + vignette + per-platform validation.
- Once exclusions are confirmed, extend the existing `<Delete>` pattern in
  [`shared/DependencyManager/src/DependencyManager.csproj`](../../shared/DependencyManager/src/DependencyManager.csproj)
  with an `<ExcludedAssemblies>` ItemGroup.
